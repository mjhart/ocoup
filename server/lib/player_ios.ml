open! Core
open! Async
open Types

module Cli_player_io : sig
  include Player_io_S

  val cli_player : Player_id.t -> t Deferred.t
end = struct
  let unexpected_eof () = failwith "EOF"

  let stdin_throttle =
    Lazy.from_fun (fun () ->
        Throttle.Sequencer.create ~continue_on_error:false ())

  type t = { player_id : Player_id.t; writer : Writer.t }

  let cli_player player_id =
    (* open a file for writing *)
    let%map writer =
      Writer.open_file [%string "player_%{player_id#Player_id}.txt"]
    in
    { player_id; writer }

  let color_code t =
    let i = 34 + (Player_id.to_int t.player_id mod 6) in
    sprintf "\027[%dm" i

  let print_endline t message =
    Writer.write_line t.writer message;
    print_string (color_code t);
    print_endline message;
    print_string "\027[0m";
    ()

  let _print_s t message =
    Writer.write_sexp t.writer message;
    Async_unix.print_string (color_code t);
    print_s message;
    Async_unix.print_string "\027[0m";
    ()

  let with_stdin t ~f =
    Throttle.enqueue (Lazy.force stdin_throttle) (fun () ->
        print_endline t
          (sprintf "You are player %d" (Player_id.to_int t.player_id));
        f (Lazy.force Reader.stdin))

  let print_visible_game_state t visible_game_state =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    print_endline t visible_game_state_string

  let choose_action t ~visible_game_state =
    with_stdin t ~f:(fun stdin ->
        print_visible_game_state t visible_game_state;
        print_endline t "Choose action (I/FA/A n/C n/T/S n/E)";
        match%bind Reader.read_line stdin with
        | `Eof -> unexpected_eof ()
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "I" ] -> return `Income
            | [ "FA" ] -> return `Foreign_aid
            | [ "A"; n ] ->
                return (`Assassinate (Player_id.of_int (Int.of_string n)))
            | [ "C"; n ] -> return (`Coup (Player_id.of_int (Int.of_string n)))
            | [ "T" ] -> return `Tax
            | [ "S"; n ] -> return (`Steal (Player_id.of_int (Int.of_string n)))
            | [ "E" ] -> return `Exchange
            | _ ->
                print_endline t "Invalid action";
                return `Income))

  let choose_assasination_response t ~visible_game_state ~asassinating_player_id
      =
    with_stdin t ~f:(fun stdin ->
        print_visible_game_state t visible_game_state;
        print_endline t
          [%string
            "Player %{asassinating_player_id#Player_id} is attempting to \
             assassinate you. Block assassination? (Y/N)"];
        match%map Reader.read_line stdin with
        | `Eof -> unexpected_eof ()
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "N" ] -> `Allow
            | [ "Y" ] -> `Block
            | _ ->
                print_endline t "Invalid action";
                `Allow))

  let choose_steal_response t ~visible_game_state ~stealing_player_id =
    with_stdin t ~f:(fun stdin ->
        print_visible_game_state t visible_game_state;
        print_endline t
          [%string
            "Player %{stealing_player_id#Player_id} is stealing from you. \
             Block steal? (C/A/N)"];
        match%map Reader.read_line stdin with
        | `Eof -> unexpected_eof ()
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "C" ] -> `Block `Captain
            | [ "A" ] -> `Block `Ambassador
            | [ "N" ] -> `Allow
            | _ ->
                print_endline t "Invalid action";
                `Allow))

  let choose_foreign_aid_response t ~visible_game_state () ~cancelled_reason =
    upon cancelled_reason (function
        | Cancelled_reason.Other_player_responded player_id ->
        print_endline t
          [%string
            "Player %{player_id#Player_id} has already blocked the foreign aid."]);
    with_stdin t ~f:(fun stdin ->
        match Deferred.is_determined cancelled_reason with
        | true -> return `Allow
        | false -> (
            print_visible_game_state t visible_game_state;
            print_endline t "Block foreign aid? (Y/N)";
            match%map Reader.read_line stdin with
            | `Eof -> unexpected_eof ()
            | `Ok action_str -> (
                match String.split action_str ~on:' ' with
                | [ "Y" ] -> `Block
                | [ "N" ] -> `Allow
                | _ ->
                    print_endline t "Invalid action";
                    `Allow)))

  let choose_cards_to_return t ~visible_game_state card_1 card_2 hand =
    let cards =
      let cards_in_hand =
        match hand with
        | Hand.Both (card_1, card_2) -> [ card_1; card_2 ]
        | Hand.One { hidden; revealed = _ } -> [ hidden ]
      in
      [ card_1; card_2 ] @ cards_in_hand
    in
    let cards_string =
      cards
      |> List.mapi ~f:(fun i card -> [%string "%{i#Int}: %{card#Card}"])
      |> String.concat ~sep:" "
    in
    with_stdin t ~f:(fun stdin ->
        print_visible_game_state t visible_game_state;
        print_endline t
          [%string
            "Cards to choose from:\n\
             %{cards_string}\n\
             Choose 2 cards to return by entering the indices of the cards you \
             want to return, separated by a space and 0-indexed."];
        match%map Reader.read_line stdin with
        | `Eof -> unexpected_eof ()
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ i1; i2 ] ->
                ( List.nth_exn cards (Int.of_string i1),
                  List.nth_exn cards (Int.of_string i2) )
            | _ ->
                print_endline t "Invalid action";
                (card_1, card_2)))

  let reveal_card t ~visible_game_state ~card_1 ~card_2 =
    let cards_string =
      [ card_1; card_2 ]
      |> List.mapi ~f:(fun i card -> [%string "%{i#Int}: %{card#Card}"])
      |> String.concat ~sep:"\n"
    in
    with_stdin t ~f:(fun stdin ->
        print_visible_game_state t visible_game_state;
        print_endline t
          [%string
            "Cards to choose from:\n\
             %{cards_string}\n\
             Which card do you want to reveal? (0/1)"];
        match%map Reader.read_line stdin with
        | `Eof -> unexpected_eof ()
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "0" ] -> `Card_1
            | [ "1" ] -> `Card_2
            | _ ->
                print_endline t "Invalid action";
                `Card_1))

  let offer_challenge t ~visible_game_state acting_player_id challengable
      ~cancelled_reason =
    upon cancelled_reason (function
        | Cancelled_reason.Other_player_responded player_id ->
        print_endline t
          [%string "Player %{player_id#Player_id} has challenged the action."]);
    with_stdin t ~f:(fun stdin ->
        match Deferred.is_determined cancelled_reason with
        | true -> return `No_challenge
        | false -> (
            print_visible_game_state t visible_game_state;
            print_endline t
              [%string
                "Player %{acting_player_id#Player_id} is attempting to perform \
                 %{challengable#Challengable}. Challenge? (Y/N)"];
            match%map Reader.read_line stdin with
            | `Eof -> unexpected_eof ()
            | `Ok action_str -> (
                match String.split action_str ~on:' ' with
                | [ "N" ] -> `No_challenge
                | [ "Y" ] -> `Challenge
                | _ ->
                    print_endline t "Invalid action";
                    `No_challenge)))

  let notify_of_action_choice t player_id action =
    print_endline t
      [%string "Player %{player_id#Player_id} chose action %{action#Action}"]
    |> return

  let notify_of_new_card t card =
    print_endline t [%string "Got new card %{card#Card}"] |> return

  let notify_of_lost_influence t player_id card =
    print_endline t
      [%string "Player %{player_id#Player_id} lost influence %{card#Card}"]
    |> return

  let notify_of_challenge t ~challenging_player_id ~has_required_card =
    let success =
      match has_required_card with
      | true -> "unsuccessfully"
      | false -> "successfully"
    in
    print_endline t
      [%string
        "Player %{challenging_player_id#Player_id} challenged you %{success}"]
    |> return

  let notify_of_game_start _t ~visible_game_state:_ = return ()
end

module Llm_player_io : sig
  include Player_io_S

  val gpt_4o : string
  val gpt_4o_mini : string

  (* val o1_mini : string *)
  val o3_mini : string
  val create : Player_id.t -> model:string -> t Deferred.t
end = struct
  type role = Developer | Assistant | User

  let gpt_4o = "gpt-4o"
  let gpt_4o_mini = "gpt-4o-mini"

  (* let o1_mini = "o1-mini" *)
  let o3_mini = "o3-mini"

  let role_to_string = function
    | Developer -> "developer"
    | Assistant -> "assistant"
    | User -> "user"

  type t = {
    events : (role * string) Queue.t;
    player_id : Player_id.t;
    writer : Writer.t;
    model : string;
  }

  let create player_id ~model =
    let events = Queue.create () in
    Queue.enqueue events (Developer, Rules.rules);

    let%map writer =
      Writer.open_file [%string "player_%{player_id#Player_id}.txt"]
    in
    { events; player_id; writer; model }

  let print_endline t message =
    ignore t.player_id;
    Writer.write_line t.writer message;
    ()

  let headers =
    Lazy.from_fun (fun () ->
        Cohttp.Header.of_list
          [
            ("Content-Type", "application/json");
            ( "Authorization",
              "Bearer " ^ (Sys.getenv "OPENAI_API_KEY" |> Option.value_exn) );
          ])

  let send_request t prompt =
    let open Cohttp_async in
    Queue.enqueue t.events (User, prompt);
    Queue.to_list t.events |> List.tl_exn
    |> List.iter ~f:(fun (role, content) ->
           print_endline t
             [%string "Role: %{role_to_string role} Content: %{content}"]);
    let messages =
      Queue.to_list t.events
      |> List.map ~f:(fun (role, content) ->
             `Assoc
               [
                 ("role", `String (role_to_string role));
                 ("content", `String content);
               ])
    in
    let post_body =
      let json =
        `Assoc
          [
            ("model", `String t.model);
            ("messages", `List messages);
            ("response_format", `Assoc [ ("type", `String "json_object") ]);
          ]
      in
      Body.of_string (Yojson.Basic.to_string json)
    in
    let%bind _response, body =
      Client.post ~headers:(Lazy.force headers)
        (Uri.of_string "https://api.openai.com/v1/chat/completions")
        ~body:post_body
    in
    let%map body = Body.to_string body in
    print_endline t body;
    let json = Yojson.Basic.from_string body in
    let content_string =
      Yojson.Basic.Util.member "choices" json
      |> Yojson.Basic.Util.to_list |> List.hd_exn
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.member "content"
      |> function
      | `String content -> content
      | _ -> failwith "Invalid content"
    in
    Queue.enqueue t.events (Assistant, content_string);
    Yojson.Basic.from_string content_string

  (* TODO: send initial data here *)
  let notify_of_game_start t ~visible_game_state =
    (match visible_game_state with
    | {
     Visible_game_state.hand = Hand.Both (card_1, card_2);
     other_players;
     coins = _;
     active_player_id = _;
    } ->
        let other_players_string =
          other_players
          |> List.map
               ~f:(fun { Visible_game_state.Other_player.player_id; _ } ->
                 [%string "Player id: %{player_id#Player_id}"])
          |> String.concat ~sep:", "
        in
        Queue.enqueue t.events
          ( Developer,
            [%string
              "The game is starting. You are player %{t.player_id#Player_id}. \
               Your starting cards are %{card_1#Card} and %{card_2#Card}. \
               Players %{other_players_string} are your opponents."] )
    | _ -> failwith "Expected player to have two cards at game start");
    return ()

  let choose_action t ~visible_game_state =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         Choose action: Income | Foreign_aid | Assassinate player_id | Coup \
         target_player_id | Tax | Steal target_player_id | Exchange. Respond \
         with a json object with the key 'reasoning' containing the reasoning \
         behind your choice as a string, and the key 'action' containing the \
         action you want to take. If the action requires a target player, \
         additionally provide the key 'target_player_id' and the value will be \
         the the player_id of the target player."]
    in

    let%map response = send_request t prompt in
    let action = Yojson.Basic.Util.member "action" response in
    let target_player_id =
      Yojson.Basic.Util.member "target_player_id" response
    in
    match (action, target_player_id) with
    | `String "Income", `Null -> `Income
    | `String "Foreign_aid", `Null -> `Foreign_aid
    | `String "Assassinate", `Int target_player_id ->
        `Assassinate (Player_id.of_int target_player_id)
    | `String "Coup", `Int target_player_id ->
        `Coup (Player_id.of_int target_player_id)
    | `String "Tax", `Null -> `Tax
    | `String "Steal", `Int target_player_id ->
        `Steal (Player_id.of_int target_player_id)
    | `String "Exchange", `Null -> `Exchange
    | _ ->
        print_endline t "Invalid action";
        `Income

  let choose_assasination_response t ~visible_game_state ~asassinating_player_id
      =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         You are being assassinated by player \
         %{asassinating_player_id#Player_id}. Respond with a json object with \
         the key 'reasoning' containing the reasoning behind your choice as a \
         string, and the key 'response' containing 'Allow' or 'Block'."]
    in
    let%map response = send_request t prompt in
    let response = Yojson.Basic.Util.member "response" response in
    match response with
    | `String "Allow" -> `Allow
    | `String "Block" -> `Block
    | _ ->
        print_endline t "Invalid response";
        `Allow

  let choose_foreign_aid_response t ~visible_game_state () ~cancelled_reason:_ =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         Player is attempting to take foreign aid. Respond with a json object \
         with the key 'response' and the value being 'Allow' or 'Block'. \
         Provide the reasoning behind your choice as a string in the key \
         'reasoning'."]
    in
    let%map response = send_request t prompt in
    let response = Yojson.Basic.Util.member "response" response in
    match response with
    | `String "Allow" -> `Allow
    | `String "Block" -> `Block
    | _ ->
        print_endline t "Invalid response";
        `Allow

  let choose_steal_response t ~visible_game_state ~stealing_player_id =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         Player %{stealing_player_id#Player_id} is attempting to steal from \
         you. Respond with a json object with the key 'reasoning' containing \
         the reasoning behind your choice as a string, and the key 'response' \
         containing 'Allow' or 'Block'. If you choose to block, additionally \
         provide the card you are blocking with in the key 'card' with the \
         value being 'Ambassador' or 'Captain'."]
    in
    let%map response = send_request t prompt in
    let action = Yojson.Basic.Util.member "response" response in
    let card = Yojson.Basic.Util.member "card" response in
    match (action, card) with
    | `String "Allow", `Null -> `Allow
    | `String "Block", `String "Ambassador" -> `Block `Ambassador
    | `String "Block", `String "Captain" -> `Block `Captain
    | _ ->
        print_endline t "Invalid response";
        `Allow

  let choose_cards_to_return t ~visible_game_state card_1 card_2 hand =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let cards =
      let cards_in_hand =
        match hand with
        | Hand.Both (card_1, card_2) -> [ card_1; card_2 ]
        | Hand.One { hidden; revealed = _ } -> [ hidden ]
      in
      [ card_1; card_2 ] @ cards_in_hand
    in
    let cards_string =
      cards |> List.map ~f:Card.to_string |> String.concat ~sep:", "
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         You are exchanging cards. The cards available to you are \
         [%{cards_string}]. Choose two cards to return to the deck. Respond \
         with a json object with the key 'reasoning' containing the reasoning \
         behind your choice as a string, and the key 'response' containing a \
         json array of size exactly 2 containing the indices of the cards you \
         want to return. The indices are 0-indexed."]
    in
    let%map response = send_request t prompt in
    let indices = Yojson.Basic.Util.member "response" response in
    match Yojson.Basic.Util.to_list indices with
    | [ `Int index_1; `Int index_2 ] ->
        (List.nth_exn cards index_1, List.nth_exn cards index_2)
    | _ ->
        print_endline t "Invalid response";
        (card_1, card_2)

  let reveal_card t ~visible_game_state ~card_1 ~card_2 =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let cards = [ card_1; card_2 ] in
    let cards_string =
      cards |> List.map ~f:Card.to_string |> String.concat ~sep:" "
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         You have lost influence and must reveal a card. The cards available \
         to you are %{cards_string}. Which card do you want to reveal? Respond \
         with a json object with the key 'reasoning' containing the reasoning \
         behind your choice as a string, and the key 'response' containing the \
         name of the card you want to reveal, exactly as it appears in the \
         list of cards."]
    in
    let%map response = send_request t prompt in
    let response =
      Yojson.Basic.Util.member "response" response
      |> Yojson.Basic.Util.to_string
    in
    let n =
      List.findi cards ~f:(fun _ card ->
          String.equal (Card.to_string card) response)
    in
    match n with
    | Some (0, _) -> `Card_1
    | Some (1, _) -> `Card_2
    | _ ->
        print_endline t "Invalid response";
        `Card_1

  let offer_challenge t ~visible_game_state acting_player_id challengable
      ~cancelled_reason:_ =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         Player %{acting_player_id#Player_id} is attempting to perform \
         %{challengable#Challengable}. Respond with a json object with the key \
         'reasoning' containing the reasoning behind your choice as a string, \
         and the key 'response' containing 'No_challenge' or 'Challenge'."]
    in
    let%map response = send_request t prompt in
    let response = Yojson.Basic.Util.member "response" response in
    match response with
    | `String "No_challenge" -> `No_challenge
    | `String "Challenge" -> `Challenge
    | _ ->
        print_endline t "Invalid response";
        `No_challenge

  let notify_of_action_choice t player_id action =
    Queue.enqueue t.events
      ( Developer,
        [%string "Player %{player_id#Player_id} chose action %{action#Action}"]
      );
    return ()

  let notify_of_lost_influence t player_id card =
    Queue.enqueue t.events
      ( Developer,
        [%string
          "Player %{player_id#Player_id} lost influence card %{card#Card}"] );
    return ()

  let notify_of_new_card t card =
    Queue.enqueue t.events
      (Developer, [%string "Received new card %{card#Card}"]);
    return ()

  let notify_of_challenge t ~challenging_player_id ~has_required_card =
    let success =
      match has_required_card with
      | true -> "unsuccessfully"
      | false -> "successfully"
    in
    Queue.enqueue t.events
      ( Developer,
        [%string
          "Player %{challenging_player_id#Player_id} challenged you %{success}"]
      );
    return ()
end

module Gemini_player_io : sig
  include Player_io_S

  val gemini_2_5_pro_exp_03_25 : string
  val create : Player_id.t -> model:string -> t Deferred.t
end = struct
  type role = Model | User

  let gemini_2_5_pro_exp_03_25 = "gemini-2.5-pro-exp-03-25"
  let role_to_string = function Model -> "model" | User -> "user"

  type t = {
    events : (role * string) Queue.t;
    player_id : Player_id.t;
    writer : Writer.t;
    model : string;
  }

  let create player_id ~model =
    let events = Queue.create () in
    let%map writer =
      Writer.open_file [%string "player_%{player_id#Player_id}.txt"]
    in
    { events; player_id; writer; model }

  let print_endline t message =
    ignore t.player_id;
    Writer.write_line t.writer message;
    ()

  let headers =
    Lazy.from_fun (fun () ->
        Cohttp.Header.of_list [ ("Content-Type", "application/json") ])

  let send_request t prompt =
    let open Cohttp_async in
    let events = Queue.to_list t.events @ [ (User, prompt) ] in
    events
    |> List.iter ~f:(fun (role, content) ->
           print_endline t
             [%string "Role: %{role_to_string role} Content: %{content}"]);
    let messages =
      events
      |> List.map ~f:(fun (role, content) ->
             `Assoc
               [
                 ("role", `String (role_to_string role));
                 ("parts", `List [ `Assoc [ ("text", `String content) ] ]);
               ])
    in
    let post_body =
      let json =
        `Assoc
          [
            ( "system_instruction",
              `Assoc
                [
                  ("parts", `List [ `Assoc [ ("text", `String Rules.rules) ] ]);
                ] );
            ("contents", `List messages);
            ( "generationConfig",
              `Assoc [ ("response_mime_type", `String "application/json") ] );
          ]
      in
      Body.of_string (Yojson.Basic.to_string json)
    in
    let%bind _response, body =
      let url =
        Uri.of_string
          [%string
            "https://generativelanguage.googleapis.com/v1beta/models/%{t.model}:generateContent"]
      in
      let url =
        Uri.add_query_param url
          ("key", [ Sys.getenv "GEMINI_API_KEY" |> Option.value_exn ])
      in

      Client.post url ~headers:(Lazy.force headers) ~body:post_body
    in
    let%map body = Body.to_string body in
    print_endline t body;
    let json = Yojson.Basic.from_string body in
    let content_string =
      Yojson.Basic.Util.member "candidates" json
      |> Yojson.Basic.Util.to_list |> List.hd_exn
      |> Yojson.Basic.Util.member "content"
      |> Yojson.Basic.Util.member "parts"
      |> Yojson.Basic.Util.to_list |> List.hd_exn
      |> Yojson.Basic.Util.member "text"
      |> function
      | `String content -> content
      | _ -> failwith "Invalid content"
    in
    Queue.enqueue t.events (User, prompt);
    Queue.enqueue t.events (Model, content_string);
    Yojson.Basic.from_string content_string

  (* TODO: send initial data here *)
  let notify_of_game_start t ~visible_game_state =
    (match visible_game_state with
    | {
     Visible_game_state.hand = Hand.Both (card_1, card_2);
     other_players;
     coins = _;
     active_player_id = _;
    } ->
        let other_players_string =
          other_players
          |> List.map
               ~f:(fun { Visible_game_state.Other_player.player_id; _ } ->
                 [%string "Player id: %{player_id#Player_id}"])
          |> String.concat ~sep:", "
        in
        Queue.enqueue t.events
          ( User,
            [%string
              "The game is starting. You are player %{t.player_id#Player_id}. \
               Your starting cards are %{card_1#Card} and %{card_2#Card}. \
               Players %{other_players_string} are your opponents."] )
    | _ -> failwith "Expected player to have two cards at game start");
    return ()

  let choose_action t ~visible_game_state =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         Choose action: Income | Foreign_aid | Assassinate player_id | Coup \
         target_player_id | Tax | Steal target_player_id | Exchange. Respond \
         with a json object with the key 'reasoning' containing the reasoning \
         behind your choice as a string, and the key 'action' containing the \
         action you want to take. If the action requires a target player, \
         additionally provide the key 'target_player_id' and the value will be \
         the the player_id of the target player."]
    in

    let%map response = send_request t prompt in
    let action = Yojson.Basic.Util.member "action" response in
    let target_player_id =
      Yojson.Basic.Util.member "target_player_id" response
    in
    match (action, target_player_id) with
    | `String "Income", `Null -> `Income
    | `String "Foreign_aid", `Null -> `Foreign_aid
    | `String "Assassinate", `Int target_player_id ->
        `Assassinate (Player_id.of_int target_player_id)
    | `String "Coup", `Int target_player_id ->
        `Coup (Player_id.of_int target_player_id)
    | `String "Tax", `Null -> `Tax
    | `String "Steal", `Int target_player_id ->
        `Steal (Player_id.of_int target_player_id)
    | `String "Exchange", `Null -> `Exchange
    | _ ->
        print_endline t "Invalid action";
        `Income

  let visible_game_state_prompt visible_game_state =
    String.concat_lines
      [
        "Game state, after the active player has paid for their action:";
        Visible_game_state.to_string_pretty visible_game_state;
      ]

  let choose_assasination_response t ~visible_game_state ~asassinating_player_id
      =
    let visible_game_state_string =
      visible_game_state_prompt visible_game_state
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         You are being assassinated by player \
         %{asassinating_player_id#Player_id}. Respond with a json object with \
         the key 'reasoning' containing the reasoning behind your choice as a \
         string, and the key 'response' containing 'Allow' or 'Block'."]
    in
    let%map response = send_request t prompt in
    let response = Yojson.Basic.Util.member "response" response in
    match response with
    | `String "Allow" -> `Allow
    | `String "Block" -> `Block
    | _ ->
        print_endline t "Invalid response";
        `Allow

  let choose_foreign_aid_response t ~visible_game_state () ~cancelled_reason:_ =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         Player is attempting to take foreign aid. Respond with a json object \
         with the key 'response' and the value being 'Allow' or 'Block'. \
         Provide the reasoning behind your choice as a string in the key \
         'reasoning'."]
    in
    let%map response = send_request t prompt in
    let response = Yojson.Basic.Util.member "response" response in
    match response with
    | `String "Allow" -> `Allow
    | `String "Block" -> `Block
    | _ ->
        print_endline t "Invalid response";
        `Allow

  let choose_steal_response t ~visible_game_state ~stealing_player_id =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         Player %{stealing_player_id#Player_id} is attempting to steal from \
         you. Respond with a json object with the key 'reasoning' containing \
         the reasoning behind your choice as a string, and the key 'response' \
         containing 'Allow' or 'Block'. If you choose to block, additionally \
         provide the card you are blocking with in the key 'card' with the \
         value being 'Ambassador' or 'Captain'."]
    in
    let%map response = send_request t prompt in
    let action = Yojson.Basic.Util.member "response" response in
    let card = Yojson.Basic.Util.member "card" response in
    match (action, card) with
    | `String "Allow", `Null -> `Allow
    | `String "Block", `String "Ambassador" -> `Block `Ambassador
    | `String "Block", `String "Captain" -> `Block `Captain
    | _ ->
        print_endline t "Invalid response";
        `Allow

  let choose_cards_to_return t ~visible_game_state card_1 card_2 hand =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let cards =
      let cards_in_hand =
        match hand with
        | Hand.Both (card_1, card_2) -> [ card_1; card_2 ]
        | Hand.One { hidden; revealed = _ } -> [ hidden ]
      in
      [ card_1; card_2 ] @ cards_in_hand
    in
    let cards_string =
      cards |> List.map ~f:Card.to_string |> String.concat ~sep:", "
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         You are exchanging cards. The cards available to you are \
         [%{cards_string}]. Choose two cards to return to the deck. Respond \
         with a json object with the key 'reasoning' containing the reasoning \
         behind your choice as a string, and the key 'response' containing a \
         json array of size exactly 2 containing the indices of the cards you \
         want to return. The indices are 0-indexed."]
    in
    let%map response = send_request t prompt in
    let indices = Yojson.Basic.Util.member "response" response in
    match Yojson.Basic.Util.to_list indices with
    | [ `Int index_1; `Int index_2 ] ->
        (List.nth_exn cards index_1, List.nth_exn cards index_2)
    | _ ->
        print_endline t "Invalid response";
        (card_1, card_2)

  let reveal_card t ~visible_game_state ~card_1 ~card_2 =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let cards = [ card_1; card_2 ] in
    let cards_string =
      cards |> List.map ~f:Card.to_string |> String.concat ~sep:" "
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         You have lost influence and must reveal a card. The cards available \
         to you are %{cards_string}. Which card do you want to reveal? Respond \
         with a json object with the key 'reasoning' containing the reasoning \
         behind your choice as a string, and the key 'response' containing the \
         name of the card you want to reveal, exactly as it appears in the \
         list of cards."]
    in
    let%map response = send_request t prompt in
    let response =
      Yojson.Basic.Util.member "response" response
      |> Yojson.Basic.Util.to_string
    in
    let n =
      List.findi cards ~f:(fun _ card ->
          String.equal (Card.to_string card) response)
    in
    match n with
    | Some (0, _) -> `Card_1
    | Some (1, _) -> `Card_2
    | _ ->
        print_endline t "Invalid response";
        `Card_1

  let offer_challenge t ~visible_game_state acting_player_id challengable
      ~cancelled_reason:_ =
    let visible_game_state_string =
      Visible_game_state.to_string_pretty visible_game_state
    in
    let prompt =
      [%string
        "%{visible_game_state_string}\n\
         Player %{acting_player_id#Player_id} is attempting to perform \
         %{challengable#Challengable}. Respond with a json object with the key \
         'reasoning' containing the reasoning behind your choice as a string, \
         and the key 'response' containing 'No_challenge' or 'Challenge'."]
    in
    let%map response = send_request t prompt in
    let response = Yojson.Basic.Util.member "response" response in
    match response with
    | `String "No_challenge" -> `No_challenge
    | `String "Challenge" -> `Challenge
    | _ ->
        print_endline t "Invalid response";
        `No_challenge

  let notify_of_action_choice t player_id action =
    Queue.enqueue t.events
      ( User,
        [%string "Player %{player_id#Player_id} chose action %{action#Action}"]
      );
    return ()

  let notify_of_lost_influence t player_id card =
    Queue.enqueue t.events
      ( User,
        [%string
          "Player %{player_id#Player_id} lost influence card %{card#Card}"] );
    return ()

  let notify_of_new_card t card =
    Queue.enqueue t.events (User, [%string "Received new card %{card#Card}"]);
    return ()

  let notify_of_challenge t ~challenging_player_id ~has_required_card =
    let success =
      match has_required_card with
      | true -> "unsuccessfully"
      | false -> "successfully"
    in
    Queue.enqueue t.events
      ( User,
        [%string
          "Player %{challenging_player_id#Player_id} challenged you %{success}"]
      );
    return ()
end

module Websocket_player_io : sig
  include Player_io_S

  val create :
    player_id:Player_id.t ->
    reader:Yojson.Safe.t Pipe.Reader.t ->
    writer:string Pipe.Writer.t ->
    t
end = struct
  module Protocol = struct
    open Ppx_yojson_conv_lib.Yojson_conv.Primitives

    module Card = struct
      include Card

      let yojson_of_t card = `String (to_string card)
    end

    module Action = struct
      include Action

      let yojson_of_t : t -> Yojson.Safe.t = function
        | `Income -> `Assoc [ ("type", `String "Income") ]
        | `Foreign_aid -> `Assoc [ ("type", `String "Foreign_aid") ]
        | `Assassinate player_id ->
            `Assoc
              [
                ("type", `String "Assassinate");
                ("player_id", `Int (Player_id.to_int player_id));
              ]
        | `Coup player_id ->
            `Assoc
              [
                ("type", `String "Coup");
                ("player_id", `Int (Player_id.to_int player_id));
              ]
        | `Exchange -> `Assoc [ ("type", `String "Exchange") ]
        | `Steal player_id ->
            `Assoc
              [
                ("type", `String "Steal");
                ("player_id", `Int (Player_id.to_int player_id));
              ]
        | `Tax -> `Assoc [ ("type", `String "Tax") ]

      let t_of_yojson : Yojson.Safe.t -> t = function
        | `Assoc [ ("type", `String "Income") ] -> `Income
        | `Assoc [ ("type", `String "Tax") ] -> `Tax
        | `Assoc [ ("type", `String "Foreign_aid") ] -> `Foreign_aid
        | `Assoc
            [ ("type", `String "Assassinate"); ("player_id", `Int player_id) ]
          ->
            `Assassinate (Player_id.of_int player_id)
        | `Assoc [ ("type", `String "Coup"); ("player_id", `Int player_id) ] ->
            `Coup (Player_id.of_int player_id)
        | `Assoc [ ("type", `String "Exchange") ] -> `Exchange
        | `Assoc [ ("type", `String "Steal"); ("player_id", `Int player_id) ] ->
            `Steal (Player_id.of_int player_id)
        | _ -> `Income
    end

    module Allow_or_block = struct
      type t = [ `Allow | `Block ]

      let t_of_yojson : Yojson.Safe.t -> t = function
        | `Assoc [ ("type", `String "Allow") ] -> `Allow
        | `Assoc [ ("type", `String "Block") ] -> `Block
        | _ -> `Allow
    end

    module Visible_game_state = struct
      include Visible_game_state

      let yojson_of_t : t -> Yojson.Safe.t =
       fun { hand; coins; other_players; active_player_id } ->
        let hand =
          match hand with
          | Hand.Both (card_1, card_2) ->
              `List
                [
                  `Assoc
                    [
                      ("card", Card.yojson_of_t card_1);
                      ("revealed", `Bool false);
                    ];
                  `Assoc
                    [
                      ("card", Card.yojson_of_t card_2);
                      ("revealed", `Bool false);
                    ];
                ]
          | Hand.One { hidden; revealed } ->
              `List
                [
                  `Assoc
                    [
                      ("card", Card.yojson_of_t hidden);
                      ("revealed", `Bool false);
                    ];
                  `Assoc
                    [
                      ("card", Card.yojson_of_t revealed);
                      ("revealed", `Bool true);
                    ];
                ]
        in
        let other_players =
          List.map other_players
            ~f:(fun { Other_player.player_id; visible_card; coins } ->
              `Assoc
                [
                  ("player_id", Player_id.yojson_of_t player_id);
                  ("visible_card", [%yojson_of: Card.t option] visible_card);
                  ("coins", `Int coins);
                ])
        in
        `Assoc
          [
            ("hand", hand);
            ("coins", `Int coins);
            ("other_players", `List other_players);
            ("active_player_id", Player_id.yojson_of_t active_player_id);
          ]
    end

    module Game_start = struct
      let query ~player_id ~visible_game_state =
        `Assoc
          [
            ("type", `String "Game_start");
            ( "visible_game_state",
              Visible_game_state.yojson_of_t visible_game_state );
            ("self_player_id", Player_id.yojson_of_t player_id);
          ]
    end

    module Challengable = struct
      include Challengable

      let yojson_of_t : t -> Yojson.Safe.t = function
        | `Tax -> `Assoc [ ("type", `String "Tax") ]
        | `Assassinate player_id ->
            `Assoc
              [
                ("type", `String "Assassinate");
                ("player_id", `Int (Player_id.to_int player_id));
              ]
        | `Exchange -> `Assoc [ ("type", `String "Exchange") ]
        | `Steal player_id ->
            `Assoc
              [
                ("type", `String "Steal");
                ("player_id", `Int (Player_id.to_int player_id));
              ]
        | `Block_assassination ->
            `Assoc [ ("type", `String "Block_assassination") ]
        | `Block_steal blocking_card ->
            let blocking_card =
              match blocking_card with
              | `Ambassador -> "Ambassador"
              | `Captain -> "Captain"
            in
            `Assoc
              [
                ("type", `String "Block_steal");
                ("blocking_card", `String blocking_card);
              ]
        | `Block_foreign_aid -> `Assoc [ ("type", `String "Block_foreign_aid") ]
    end

    module Choose_action = struct
      let query ~visible_game_state =
        `Assoc
          [
            ("type", `String "Choose_action");
            ( "visible_game_state",
              Visible_game_state.yojson_of_t visible_game_state );
          ]

      let response_of_yojson = Action.t_of_yojson
    end

    (* TODO can get [Player_id.t] from visible game state *)
    module Choose_assasination_response = struct
      let create_query player_id ~visible_game_state =
        `Assoc
          [
            ("type", `String "Choose_assasination_response");
            ("player_id", Player_id.yojson_of_t player_id);
            ( "visible_game_state",
              Visible_game_state.yojson_of_t visible_game_state );
          ]

      let response_of_yojson = Allow_or_block.t_of_yojson
    end

    module Choose_foreign_aid_response = struct
      let create_query ~visible_game_state () =
        `Assoc
          [
            ("type", `String "Choose_foreign_aid_response");
            ( "visible_game_state",
              Visible_game_state.yojson_of_t visible_game_state );
          ]

      let response_of_yojson = Allow_or_block.t_of_yojson
    end

    module Choose_steal_response = struct
      type response = [ `Allow | `Block of [ `Ambassador | `Captain ] ]

      let create_query stealing_player_id ~visible_game_state =
        `Assoc
          [
            ("type", `String "Choose_steal_response");
            ("player_id", Player_id.yojson_of_t stealing_player_id);
            ( "visible_game_state",
              Visible_game_state.yojson_of_t visible_game_state );
          ]

      let response_of_yojson : Yojson.Safe.t -> response = function
        | `Assoc [ ("type", `String "Allow") ] -> `Allow
        | `Assoc [ ("type", `String "Block"); ("card", `String "Ambassador") ]
          ->
            `Block `Ambassador
        | `Assoc [ ("type", `String "Block"); ("card", `String "Captain") ] ->
            `Block `Captain
        | _ -> `Allow
    end

    module Choose_cards_to_return = struct
      let create_query cards ~visible_game_state =
        `Assoc
          [
            ("type", `String "Choose_cards_to_return");
            ("cards", `List (List.map ~f:Card.yojson_of_t cards));
            ( "visible_game_state",
              Visible_game_state.yojson_of_t visible_game_state );
          ]

      let response_of_yojson = function
        | `List [ `String card_1; `String card_2 ] ->
            (Card.of_string card_1, Card.of_string card_2)
        | _ -> failwith "Invalid choose cards to return response"
    end

    module Reveal_card = struct
      let create_query card_1 card_2 ~visible_game_state =
        `Assoc
          [
            ("type", `String "Reveal_card");
            ("card_1", Card.yojson_of_t card_1);
            ("card_2", Card.yojson_of_t card_2);
            ( "visible_game_state",
              Visible_game_state.yojson_of_t visible_game_state );
          ]

      let response_of_yojson = function
        | `Assoc [ ("type", `String "Card_1") ] -> `Card_1
        | `Assoc [ ("type", `String "Card_2") ] -> `Card_2
        | _ -> `Card_1
    end

    module Offer_challenge = struct
      let create_query acting_player_id challengable ~visible_game_state =
        `Assoc
          [
            ("type", `String "Offer_challenge");
            ("acting_player_id", Player_id.yojson_of_t acting_player_id);
            ("action", Challengable.yojson_of_t challengable);
            ( "visible_game_state",
              Visible_game_state.yojson_of_t visible_game_state );
          ]

      let response_of_yojson = function
        | `Assoc [ ("type", `String "No_challenge") ] -> `No_challenge
        | `Assoc [ ("type", `String "Challenge") ] -> `Challenge
        | _ -> `No_challenge
    end

    module Action_choice_notification = struct
      let create_query player_id action =
        `Assoc
          [
            ("type", `String "Action_chosen");
            ("player_id", Player_id.yojson_of_t player_id);
            ("action", Action.yojson_of_t action);
          ]
    end

    module Lost_influence_notification = struct
      let create_query player_id card =
        `Assoc
          [
            ("type", `String "Lost_influence");
            ("player_id", Player_id.yojson_of_t player_id);
            ("card", Card.yojson_of_t card);
          ]
    end

    module New_card_notification = struct
      let create_query card =
        `Assoc [ ("type", `String "New_card"); ("card", Card.yojson_of_t card) ]
    end

    module Challenge_notification = struct
      let create_query challenging_player_id has_required_card =
        `Assoc
          [
            ("type", `String "Challenge");
            ("player_id", Player_id.yojson_of_t challenging_player_id);
            ("has_required_card", `Bool has_required_card);
          ]
    end

    module Player_responded_notification = struct
      let create_query player_id =
        `Assoc
          [
            ("type", `String "Player_responded");
            ("player_id", Player_id.yojson_of_t player_id);
          ]
    end
  end

  type t = {
    player_id : Player_id.t;
    to_client : string Pipe.Writer.t;
    from_client : Yojson.Safe.t Pipe.Reader.t;
    mutable expected_message_handler : Yojson.Safe.t Ivar.t;
  }

  let create ~player_id ~reader ~writer =
    let t =
      {
        player_id;
        to_client = writer;
        from_client = reader;
        expected_message_handler = Ivar.create ();
      }
    in
    don't_wait_for
      (Pipe.iter t.from_client ~f:(fun message ->
           (match Ivar.peek t.expected_message_handler with
           | Some prior_message ->
               print_s
                 [%message
                   "Received unexpected message"
                     ~prior_message:(Yojson.Safe.to_string prior_message)
                     ~message:(Yojson.Safe.to_string message)]
           | None -> Ivar.fill_exn t.expected_message_handler message);
           return ()));
    t

  let write_to_client t query =
    Pipe.write t.to_client (Yojson.Safe.to_string query)

  let read_from_client t f =
    let ivar = Ivar.create () in
    t.expected_message_handler <- ivar;
    let%map message = Ivar.read ivar in
    f message

  let upon_next_message' t ~cancelled_reason ~on_cancel f =
    let ivar = Ivar.create () in
    t.expected_message_handler <- ivar;
    choose
      [
        choice (Ivar.read ivar) f; choice cancelled_reason (Fn.const on_cancel);
      ]

  let choose_action t ~visible_game_state =
    let response =
      read_from_client t Protocol.Choose_action.response_of_yojson
    in
    let%bind () =
      write_to_client t (Protocol.Choose_action.query ~visible_game_state)
    in
    response

  let choose_assasination_response t ~visible_game_state ~asassinating_player_id
      =
    let query =
      Protocol.Choose_assasination_response.create_query asassinating_player_id
        ~visible_game_state
    in
    let response =
      read_from_client t
        Protocol.Choose_assasination_response.response_of_yojson
    in
    let%bind () = write_to_client t query in
    response

  let choose_foreign_aid_response t ~visible_game_state () ~cancelled_reason =
    let query =
      Protocol.Choose_foreign_aid_response.create_query ~visible_game_state ()
    in

    upon cancelled_reason (function
        | Cancelled_reason.Other_player_responded player_id ->
        let query =
          Protocol.Player_responded_notification.create_query player_id
        in
        write_to_client t query |> don't_wait_for);

    let f =
     fun message ->
      Protocol.Choose_foreign_aid_response.response_of_yojson message
    in
    let response = upon_next_message' t ~cancelled_reason ~on_cancel:`Allow f in
    let%bind () = write_to_client t query in
    response

  let choose_steal_response t ~visible_game_state ~stealing_player_id =
    let query =
      Protocol.Choose_steal_response.create_query stealing_player_id
        ~visible_game_state
    in
    let response =
      read_from_client t Protocol.Choose_steal_response.response_of_yojson
    in
    let%bind () = write_to_client t query in
    response

  let choose_cards_to_return t ~visible_game_state card_1 card_2 hand =
    let query =
      Protocol.Choose_cards_to_return.create_query
        ([ card_1; card_2 ]
        @
        match hand with
        | Hand.Both (card_1, card_2) -> [ card_1; card_2 ]
        | Hand.One { hidden; revealed = _ } -> [ hidden ])
        ~visible_game_state
    in
    let response =
      read_from_client t Protocol.Choose_cards_to_return.response_of_yojson
    in
    let%bind () = write_to_client t query in
    response

  let reveal_card t ~visible_game_state ~card_1 ~card_2 =
    let query =
      Protocol.Reveal_card.create_query card_1 card_2 ~visible_game_state
    in
    let response = read_from_client t Protocol.Reveal_card.response_of_yojson in
    let%bind () = write_to_client t query in
    response

  let offer_challenge t ~visible_game_state challenging_player_id challengable
      ~cancelled_reason =
    let query =
      Protocol.Offer_challenge.create_query challenging_player_id challengable
        ~visible_game_state
    in
    upon cancelled_reason (function
        | Cancelled_reason.Other_player_responded player_id ->
        let query =
          Protocol.Player_responded_notification.create_query player_id
        in
        write_to_client t query |> don't_wait_for);
    let response =
      upon_next_message' t ~cancelled_reason ~on_cancel:`No_challenge
        Protocol.Offer_challenge.response_of_yojson
    in
    let%bind () = write_to_client t query in
    response

  let notify_of_action_choice t player_id action =
    let query =
      Protocol.Action_choice_notification.create_query player_id action
    in
    let%bind () = write_to_client t query in
    return ()

  let notify_of_lost_influence t player_id card =
    let query =
      Protocol.Lost_influence_notification.create_query player_id card
    in
    let%bind () = write_to_client t query in
    return ()

  let notify_of_new_card t card =
    let query = Protocol.New_card_notification.create_query card in
    let%bind () = write_to_client t query in
    return ()

  let notify_of_challenge t ~challenging_player_id ~has_required_card =
    let query =
      Protocol.Challenge_notification.create_query challenging_player_id
        has_required_card
    in
    let%bind () = write_to_client t query in
    return ()

  let notify_of_game_start t ~visible_game_state =
    let query =
      Protocol.Game_start.query ~player_id:t.player_id ~visible_game_state
    in
    write_to_client t query
end

module Player_io : sig
  type t

  val llm : Player_id.t -> model:string -> t Deferred.t
  val cli : Player_id.t -> t Deferred.t
  val gemini : Player_id.t -> model:string -> t Deferred.t
  val create : (module Player_io_S with type t = 'a) -> 'a -> t

  include Player_io_S with type t := t
end = struct
  type t = Packed : (module Player_io_S with type t = 'a) * 'a -> t

  let create (type a) (module M : Player_io_S with type t = a) implementation =
    Packed ((module M), implementation)

  let llm id ~model =
    let (module M) =
      (module Llm_player_io : Player_io_S with type t = Llm_player_io.t)
    in
    let%map implementation = Llm_player_io.create id ~model in
    Packed ((module M), implementation)

  let gemini id ~model =
    let (module M) =
      (module Gemini_player_io : Player_io_S with type t = Gemini_player_io.t)
    in
    let%map implementation = Gemini_player_io.create id ~model in
    Packed ((module M), implementation)

  let cli id =
    let (module M) =
      (module Cli_player_io : Player_io_S with type t = Cli_player_io.t)
    in
    let%map implementation = Cli_player_io.cli_player id in
    Packed ((module M), implementation)

  let choose_action (Packed ((module M), implementation)) =
    M.choose_action implementation

  let choose_assasination_response (Packed ((module M), implementation)) =
    M.choose_assasination_response implementation

  let choose_foreign_aid_response (Packed ((module M), implementation)) =
    M.choose_foreign_aid_response implementation

  let choose_steal_response (Packed ((module M), implementation)) =
    M.choose_steal_response implementation

  let choose_cards_to_return (Packed ((module M), implementation)) =
    M.choose_cards_to_return implementation

  let reveal_card (Packed ((module M), implementation)) =
    M.reveal_card implementation

  let offer_challenge (Packed ((module M), implementation)) =
    M.offer_challenge implementation

  let notify_of_action_choice (Packed ((module M), implementation)) =
    M.notify_of_action_choice implementation

  let notify_of_lost_influence (Packed ((module M), implementation)) =
    M.notify_of_lost_influence implementation

  let notify_of_new_card (Packed ((module M), implementation)) =
    M.notify_of_new_card implementation

  let notify_of_challenge (Packed ((module M), implementation)) =
    M.notify_of_challenge implementation

  let notify_of_game_start (Packed ((module M), implementation)) =
    M.notify_of_game_start implementation
end
