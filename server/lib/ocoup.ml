open! Core
open! Async
open Game

(* TODOS:
   - Visible_game_state -> Game_state and Game_state -> Hidden_game_state
   - maybe replace Player_io module with a record
   - create_rounds should really make sure every player is in the odd sized game the same number of times
     - Num players must divide odd sized game * num rounds
*)

let run_game ~game_state =
  let%map final_game_state = Game.run_game ~game_state in
  Log.Global.info_s [%message "Game over" (final_game_state : Game_state.t)]

let player_io_of_string = function
  | "cli" -> Player_ios.cli
  | "gpt-4o" -> Player_ios.llm ~model:Llm_player_io.gpt_4o
  | "gpt-4o-mini" -> Player_ios.llm ~model:Llm_player_io.gpt_4o_mini
  | "o3-mini" -> Player_ios.llm ~model:Llm_player_io.o3_mini
  | "gemini-2-5" ->
      fun player_id ->
        Player_ios.gemini player_id
          ~model:Gemini_player_io.gemini_2_5_pro_exp_03_25
        |> return
  | unrecognized -> failwith (sprintf "Unrecognized player io: %s" unrecognized)

module Server = struct
  module State = struct
    type tournament_data = { tournament : Tournament.t; started : bool }

    type t = {
      games : (string Pipe.Reader.t * string Pipe.Writer.t) String.Table.t;
      tournaments : tournament_data String.Table.t;
    }

    let create () =
      { games = String.Table.create (); tournaments = String.Table.create () }

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

    let get_tournament_id =
      let next_tournament_id = Ref.create 0 in
      fun () ->
        let id = !next_tournament_id in
        next_tournament_id := id + 1;
        [%string "%{id#Int}"]

    let add_tournament ~state ~tournament =
      let tournament_id = get_tournament_id () in
      Hashtbl.set state.tournaments ~key:tournament_id
        ~data:{ tournament; started = false };
      tournament_id
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

  module Create_tournament = struct
    let handle ~state ~body _inet _request =
      let%bind body_string = Cohttp_async.Body.to_string body in
      let json = Yojson.Safe.from_string body_string in
      let max_players, bot_players =
        match json with
        | `Assoc fields ->
            let max_players =
              match
                List.Assoc.find fields ~equal:String.equal "max_players"
              with
              | Some (`Int n) -> n
              | _ -> 12
            in
            let bot_players =
              match
                List.Assoc.find fields ~equal:String.equal "bot_players"
              with
              | Some (`List bots) ->
                  List.filter_map bots ~f:(function
                    | `String s -> Some s
                    | _ -> None)
              | _ -> []
            in
            (max_players, bot_players)
        | _ -> (12, [])
      in
      let tournament = Tournament.create ~max_players in
      (* Register bot players *)
      let%bind tournament =
        Deferred.Or_error.List.fold bot_players ~init:tournament
          ~f:(fun tournament bot_player_string ->
            let next_player_id = Tournament.num_players tournament in
            let player_id = Types.Player_id.of_int next_player_id in
            Deferred.Or_error.bind
              (Deferred.Or_error.try_with (fun () ->
                   let player_io_factory =
                     player_io_of_string bot_player_string
                   in
                   player_io_factory player_id))
              ~f:(fun player_ios ->
                Tournament.register tournament player_ios |> Deferred.return))
        >>| Or_error.ok_exn
      in
      let tournament_id = State.add_tournament ~state ~tournament in
      let response_body =
        Yojson.Safe.to_string
          (`Assoc
             [
               ("tournament_id", `String tournament_id);
               ( "register_url",
                 `String [%string "/tournaments/%{tournament_id}/register"] );
               ("num_bot_players", `Int (List.length bot_players));
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
          Cohttp_async.Body.of_string response_body )
      |> return
  end

  module Start_tournament = struct
    let handle ~(state : State.t) ~tournament_id ~body:_ _inet _request =
      match Hashtbl.find state.tournaments tournament_id with
      | None ->
          let body =
            Yojson.Safe.to_string
              (`Assoc [ ("error", `String "Tournament not found") ])
          in
          `Response
            ( Cohttp.Response.make ~status:`Not_found (),
              Cohttp_async.Body.of_string body )
          |> return
      | Some tournament_data ->
          if tournament_data.started then
            let body =
              Yojson.Safe.to_string
                (`Assoc [ ("error", `String "Tournament already started") ])
            in
            `Response
              ( Cohttp.Response.make ~status:`Bad_request (),
                Cohttp_async.Body.of_string body )
            |> return
          else (
            (* Mark tournament as started *)
            Hashtbl.set state.tournaments ~key:tournament_id
              ~data:{ tournament_data with started = true };
            (* Run the tournament *)
            Log.Global.info_s
              [%message "Starting tournament" (tournament_id : string)];
            let%bind results = Tournament.start tournament_data.tournament in
            (* Calculate scores *)
            let scores = Tournament.score_results results in
            (* Convert scores to JSON *)
            let scores_json =
              Map.to_alist scores
              |> List.map ~f:(fun (player_id, score) ->
                     (Types.Player_id.to_string player_id, `Int score))
              |> fun assoc -> `Assoc assoc
            in
            (* Convert results to JSON *)
            let results_json =
              `List
                (List.mapi results ~f:(fun round_idx round ->
                     `Assoc
                       [
                         ("round", `Int (round_idx + 1));
                         ( "games",
                           `List
                             (List.mapi round ~f:(fun game_idx game_result ->
                                  match game_result with
                                  | Ok game_state ->
                                      let Game.Game_state.
                                            { players; eliminated_players; _ } =
                                        game_state
                                      in
                                      `Assoc
                                        [
                                          ("game", `Int (game_idx + 1));
                                          ("status", `String "completed");
                                          ( "winners",
                                            `List
                                              (List.map players
                                                 ~f:(fun
                                                     Game.Player.{ id; _ } ->
                                                   `String
                                                     (Types.Player_id.to_string
                                                        id))) );
                                          ( "eliminated",
                                            `List
                                              (List.rev_map eliminated_players
                                                 ~f:(fun
                                                     Game.Player.{ id; _ } ->
                                                   `String
                                                     (Types.Player_id.to_string
                                                        id))) );
                                        ]
                                  | Error err ->
                                      `Assoc
                                        [
                                          ("game", `Int (game_idx + 1));
                                          ("status", `String "error");
                                          ( "error",
                                            `String (Error.to_string_hum err) );
                                        ])) );
                       ]))
            in
            let response_body =
              Yojson.Safe.to_string
                (`Assoc
                   [
                     ("status", `String "completed");
                     ("scores", scores_json);
                     ("results", results_json);
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
                Cohttp_async.Body.of_string response_body )
            |> return)
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

          let%bind game_state =
            Game_state.init
              [
                Player_ios.llm ~model:Llm_player_io.gpt_4o;
                Player_ios.llm ~model:Llm_player_io.o3_mini;
                (fun player_id ->
                  let player_io =
                    Websocket_player_io.create ~player_id ~reader ~writer
                  in
                  return
                    (Player_ios.create (module Websocket_player_io) player_io));
              ]
            >>| Or_error.ok_exn
          in
          match%bind
            Monitor.try_with ~extract_exn:true ~rest:`Log (fun () ->
                run_game ~game_state)
          with
          | Ok () -> return ()
          | Error e ->
              Log.Global.error_s [%message "Error running game" (e : Exn.t)];
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
              [
                Player_ios.llm ~model:Llm_player_io.gpt_4o;
                Player_ios.llm ~model:Llm_player_io.o3_mini;
                (fun player_id ->
                  let player_io =
                    Websocket_player_io.create ~player_id ~reader
                      ~writer:from_game
                  in
                  Player_ios.create (module Websocket_player_io) player_io
                  |> return);
              ]
            >>| Or_error.ok_exn
          in
          match%bind
            Monitor.try_with ~extract_exn:true ~rest:`Log (fun () ->
                run_game ~game_state)
          with
          | Ok () -> return ()
          | Error e ->
              Log.Global.error_s [%message "Error running game" (e : Exn.t)];
              return ())
    in
    let tournament_register_ws_handler ~(state : State.t) ~tournament_id =
      ws_handler (fun websocket ->
          let reader, writer = Websocket.pipes websocket in
          Deferred.upon (Pipe.closed writer) (fun () ->
              Log.Global.info_s [%message "PIPE TO CLIENT IS CLOSED"]);
          match Hashtbl.find state.tournaments tournament_id with
          | None ->
              let%bind () =
                Pipe.write writer
                  (Yojson.Safe.to_string
                     (`Assoc [ ("error", `String "Tournament not found") ]))
              in
              Pipe.close writer;
              return ()
          | Some tournament_data ->
              if tournament_data.started then (
                let%bind () =
                  Pipe.write writer
                    (Yojson.Safe.to_string
                       (`Assoc
                          [ ("error", `String "Tournament already started") ]))
                in
                Pipe.close writer;
                return ())
              else
                let next_player_id =
                  Tournament.num_players tournament_data.tournament
                in
                let player_id = Types.Player_id.of_int next_player_id in
                let player_io =
                  Websocket_player_io.create ~player_id ~reader ~writer
                in
                let player_ios_t =
                  Player_ios.create (module Websocket_player_io) player_io
                  |> return
                in
                let%bind updated_tournament =
                  let%bind player_ios = player_ios_t in
                  match
                    Tournament.register tournament_data.tournament player_ios
                  with
                  | Ok t -> return t
                  | Error e ->
                      let%bind () =
                        Pipe.write writer
                          (Yojson.Safe.to_string
                             (`Assoc
                                [ ("error", `String (Error.to_string_hum e)) ]))
                      in
                      Pipe.close writer;
                      Error.raise e
                in
                Hashtbl.set state.tournaments ~key:tournament_id
                  ~data:{ tournament_data with tournament = updated_tournament };
                let%bind () =
                  Pipe.write writer
                    (Yojson.Safe.to_string
                       (`Assoc
                          [
                            ("status", `String "registered");
                            ("player_id", `Int next_player_id);
                          ]))
                in
                Log.Global.info_s
                  [%message
                    "Registered player for tournament"
                      (tournament_id : string)
                      (player_id : Types.Player_id.t)];
                (* Keep the websocket alive by waiting for it to close *)
                Pipe.closed writer)
    in
    let with_state ~(state : State.t) ~game_id f =
      match Hashtbl.find state.games game_id with
      | Some (reader, writer) -> f ~reader ~writer
      | None -> `Response (Lazy.force not_found_response) |> return
    in

    let%bind server =
      let state = State.create () in
      let on_handler_error =
        `Call
          (fun _address exn ->
            Log.Global.error_s [%message "Error handling request" (exn : exn)])
      in
      Cohttp_async.Server.create_expert ~on_handler_error
        (* ~mode:(Ssl_config.conduit_mode ssl_config) *)
        (Tcp.Where_to_listen.of_port port) (fun ~body inet request ->
          Log.Global.info_s [%message (request : Cohttp.Request.t)];
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
          | `OPTIONS, [ "/"; "tournaments" ] ->
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
          | `POST, [ "/"; "tournaments" ] ->
              Create_tournament.handle ~state ~body inet request
          | `GET, [ "/"; "tournaments"; tournament_id; "register" ] ->
              tournament_register_ws_handler ~state ~tournament_id ~body inet
                request
          | `POST, [ "/"; "tournaments"; tournament_id; "start" ] ->
              Start_tournament.handle ~state ~tournament_id ~body inet request
              | `OPTIONS, [ "/"; "tournaments"; _tournament_id; "start" ] ->
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

let run_server ~port =
  Log.Global.set_output [ Log.Output.stderr ~format:`Sexp () ];
  Server.run_server ~port

let run_game player_ios =
  let player_ios = List.map player_ios ~f:player_io_of_string in
  let%bind game_state = Game_state.init player_ios >>| Or_error.ok_exn in
  run_game ~game_state

module For_testing = struct
  module Game = Game
  module Types = Types
  module Player_ios = Player_ios
  module Tournament = Tournament
end
