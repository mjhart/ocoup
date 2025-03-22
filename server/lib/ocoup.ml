open! Core
open! Async
open Player_ios
open Game

let run_game ?game_state () =
  let%bind game_state =
    match game_state with
    | Some game_state -> return game_state
    | None -> Game_state.init ()
  in
  let%bind () =
    Game_state.players game_state
    |> List.map ~f:(fun player ->
           Player_io.notify_of_game_start player.player_io
             ~visible_game_state:
               (Game_state.to_visible_game_state game_state player.id))
    |> Deferred.all_unit
  in
  let%map final_game_state =
    Deferred.repeat_until_finished game_state take_turn
  in
  print_s [%message "Game over" (final_game_state : Game_state.t)]

module Server = struct
  module State = struct
    type t = {
      games : (string Pipe.Reader.t * string Pipe.Writer.t) String.Table.t;
    }

    let create () = { games = String.Table.create () }

    let get_game_id =
      let next_game_id = Ref.create 0 in
      fun () ->
        let id = !next_game_id in
        next_game_id := id + 1;
        [%string "game-%{id#Int}"]

    let add_game ~state ~reader ~writer =
      let game_id = get_game_id () in
      Hashtbl.set state.games ~key:game_id ~data:(reader, writer);
      game_id
  end

  let not_found_response =
    lazy
      ( Cohttp.Response.make ~status:`Not_found (),
        Cohttp_async.Body.of_string "Not found" )

  module Create_game = struct
    let handle ~state ~body:_ _inet _request =
      let reader, writer = Pipe.create () in
      let game_id = State.add_game ~state ~reader ~writer in
      let body =
        Yojson.Safe.to_string
          (`Assoc
             [
               ("game_id", `String game_id);
               ("updates_url", `String [%string "/games/%{game_id}/updates"]);
               ("player_url", `String [%string "/games/%{game_id}/player"]);
             ])
      in
      let headers =
        Cohttp.Header.of_list
          [
            ("Access-Control-Allow-Origin", "*");
            ("Access-Control-Allow-Methods", "POST, GET, OPTIONS");
            ("Access-Control-Allow-Headers", "Content-Type");
          ]
      in
      `Response
        ( Cohttp.Response.make ~status:`OK ~headers (),
          Cohttp_async.Body.of_string body )
      |> return
  end

  let run_server ~port =
    let non_ws_request ~body:_ _inet _request =
      Lazy.force not_found_response |> return
    in
    let ws_handler f =
      Cohttp_async_websocket.Server.create ~non_ws_request
        ~should_process_request:(fun _inet _header ~is_websocket_request:_ ->
          Ok ())
        (fun ~inet:_ ~subprotocol:_ _request ->
          Cohttp_async_websocket.Server.On_connection.create f |> return)
    in
    let updates_ws_handler ~updates_reader =
      ws_handler (fun websocket ->
          let _reader, writer = Websocket.pipes websocket in
          Pipe.transfer_id updates_reader writer)
    in
    let player_ws_handler =
      ws_handler (fun websocket ->
          let reader, writer = Websocket.pipes websocket in
          let reader, debugging_fork =
            Pipe.fork reader ~pushback_uses:`Both_consumers
          in
          don't_wait_for
            (Pipe.iter_without_pushback debugging_fork ~f:(fun message ->
                 print_endline message));
          let%bind game_state =
            Game_state.init
              ~create_ws_player_io:(fun player_id _card_1 _card_2 ->
                let player_io =
                  Websocket_player_io.create ~player_id
                    ~reader:(Pipe.map reader ~f:Yojson.Safe.from_string)
                    ~writer
                in
                return (Player_io.create (module Websocket_player_io) player_io))
              ()
          in
          match%bind
            Monitor.try_with ~extract_exn:true ~rest:`Log (fun () ->
                run_game ~game_state ())
          with
          | Ok () -> return ()
          | Error e ->
              print_endline "Error running game";
              print_endline (Exn.to_string e);
              return ())
    in
    let player_with_updates_ws_handler updates_writer =
      ws_handler (fun websocket ->
          let reader, writer = Websocket.pipes websocket in
          let intermediate_reader, from_game = Pipe.create () in
          let intermediate_reader_fork_1, intermediate_reader_fork_2 =
            Pipe.fork intermediate_reader ~pushback_uses:`Both_consumers
          in
          don't_wait_for (Pipe.transfer_id intermediate_reader_fork_1 writer);
          don't_wait_for
            (Pipe.transfer_id intermediate_reader_fork_2 updates_writer);

          let%bind game_state =
            Game_state.init
              ~create_ws_player_io:(fun player_id _card_1 _card_2 ->
                let player_io =
                  Websocket_player_io.create ~player_id
                    ~reader:(Pipe.map reader ~f:Yojson.Safe.from_string)
                    ~writer:from_game
                in
                Player_io.create (module Websocket_player_io) player_io
                |> return)
              ()
          in
          match%bind
            Monitor.try_with ~extract_exn:true ~rest:`Log (fun () ->
                run_game ~game_state ())
          with
          | Ok () -> return ()
          | Error e ->
              print_endline "Error running game";
              print_endline (Exn.to_string e);
              return ())
    in
    let with_state ~(state : State.t) ~game_id f =
      match Hashtbl.find state.games game_id with
      | Some (reader, writer) -> f ~reader ~writer
      | None -> `Response (Lazy.force not_found_response) |> return
    in

    let%bind server =
      let state = State.create () in
      Cohttp_async.Server.create_expert ~on_handler_error:`Ignore
        (* ~mode:(Ssl_config.conduit_mode ssl_config) *)
        (Tcp.Where_to_listen.of_port port) (fun ~body inet request ->
          print_s [%message (request : Cohttp.Request.t)];
          match
            ( Cohttp.Request.meth request,
              Cohttp.Request.uri request |> Uri.path |> Filename.parts )
          with
          | `OPTIONS, [ "/"; "games" ] ->
              let headers =
                Cohttp.Header.of_list
                  [
                    ("Access-Control-Allow-Origin", "*");
                    ("Access-Control-Allow-Methods", "POST, GET, OPTIONS");
                    ("Access-Control-Allow-Headers", "Content-Type");
                  ]
              in
              `Response
                ( Cohttp.Response.make ~status:`OK ~headers (),
                  Cohttp_async.Body.empty )
              |> return
          | `POST, [ "/"; "games" ] ->
              Create_game.handle ~state ~body inet request
          | `GET, [ "/"; "games"; game_id; "updates" ] ->
              with_state ~state ~game_id (fun ~reader ~writer:_ ->
                  updates_ws_handler ~updates_reader:reader ~body inet request)
          | `GET, [ "/"; "games"; game_id; "player" ] ->
              with_state ~state ~game_id (fun ~reader:_ ~writer ->
                  player_with_updates_ws_handler writer ~body inet request)
          | `GET, [ "/"; "new_game" ] -> player_ws_handler ~body inet request
          | _ -> `Response (Lazy.force not_found_response) |> return)
    in
    let%bind () = Cohttp_async.Server.close_finished server in
    return ()
end

let run_server ~port = Server.run_server ~port
let run_game () = run_game ()
