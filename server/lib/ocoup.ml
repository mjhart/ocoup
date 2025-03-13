open! Core
open! Async

(* TODO:
   - timeout for player action
   - validate responses from player
   - give more state to players
   - don't know who callenged you when you lose influence
 *)

(* The characters (and cards) available in the game *)
module Card = struct
  type t = Duke | Assassin | Captain | Ambassador | Contessa
  [@@deriving equal, sexp]

  let to_string = function
    | Duke -> "Duke"
    | Assassin -> "Assassin"
    | Captain -> "Captain"
    | Ambassador -> "Ambassador"
    | Contessa -> "Contessa"

  let of_string = function
    | "Duke" -> Duke
    | "Assassin" -> Assassin
    | "Captain" -> Captain
    | "Ambassador" -> Ambassador
    | "Contessa" -> Contessa
    | _ -> failwith "Invalid card"
end

module Player_id : sig
  type t [@@deriving sexp, equal, compare, yojson]

  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
end = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  type t = int [@@deriving sexp, equal, compare, yojson]

  let of_int = Fn.id
  let to_int = Fn.id
  let to_string = Int.to_string
end

type non_challengable_actions =
  [ `Income (* take 1 coin from the Treasury *)
  | `Foreign_aid (* take 2 coins (subject to blocking) *)
  | `Coup of
    Player_id.t (* pay 7 coins to launch a coup against target player *) ]

type challengable_actions =
  [ `Tax (* Duke: take 3 coins from the Treasury *)
  | `Assassinate of
    Player_id.t (* Assassin: pay 3 coins to assassinate target *)
  | `Steal of Player_id.t
  | `Exchange (* Ambassador: exchange cards with the Court deck *) ]

type challengable_responses =
  [ `Block_assassination
  | `Block_steal of [ `Captain | `Ambassador ]
  | `Block_foreign_aid ]

module Challengable = struct
  type t = [ challengable_actions | challengable_responses ]

  let required_card = function
    | `Tax -> Card.Duke
    | `Assassinate _ -> Assassin
    | `Steal _ -> Captain
    | `Exchange -> Ambassador
    | `Block_assassination -> Contessa
    | `Block_steal `Captain -> Captain
    | `Block_steal `Ambassador -> Ambassador
    | `Block_foreign_aid -> Duke

  let to_string = function
    | `Tax -> "Tax"
    | `Assassinate target_player_id ->
        sprintf "Assassinate %d" (Player_id.to_int target_player_id)
    | `Steal target_player_id ->
        sprintf "Steal from %d" (Player_id.to_int target_player_id)
    | `Exchange -> "Exchange"
    | `Block_assassination -> "Block assassination"
    | `Block_steal `Captain -> "Block steal with Captain"
    | `Block_steal `Ambassador -> "Block steal with Ambassador"
    | `Block_foreign_aid -> "Block foreign aid"
end

(* Actions a player may choose. Some actions have a target (represented by a player id). *)
module Action = struct
  type t = [ challengable_actions | non_challengable_actions ]

  let to_string = function
    | `Income -> "Income"
    | `Foreign_aid -> "Foreign aid"
    | `Coup target_player_id ->
        sprintf "Coup %d" (Player_id.to_int target_player_id)
    | `Tax -> "Tax"
    | `Assassinate target_player_id ->
        sprintf "Assassinate %d" (Player_id.to_int target_player_id)
    | `Steal target_player_id ->
        sprintf "Steal %d" (Player_id.to_int target_player_id)
    | `Exchange -> "Exchange"
end

module Hand = struct
  type t =
    | Both of Card.t * Card.t
    | One of { hidden : Card.t; revealed : Card.t }
  [@@deriving sexp]
end

module Cancelled_reason = struct
  type t = Other_player_responded of Player_id.t [@@deriving sexp]
end

module Visible_game_state = struct
  module Other_player = struct
    type t = {
      player_id : Player_id.t;
      visible_card : Card.t option;
      coins : int;
    }
    [@@deriving sexp]
  end

  type t = {
    hand : Hand.t;
    coins : int;
    other_players : Other_player.t list;
    active_player_id : Player_id.t;
  }
  [@@deriving sexp]

  let to_string_pretty t =
    let cards =
      match t.hand with
      | Hand.Both (card_1, card_2) ->
          [%string "%{card_1#Card} (hidden) %{card_2#Card} (hidden)"]
      | Hand.One { hidden; revealed } ->
          [%string "%{hidden#Card} (hidden) %{revealed#Card} (revealed)"]
    in
    let coins = [%string "You - Coins: %{t.coins#Int} Hand: %{cards}"] in
    let other_players =
      List.map t.other_players ~f:(fun other_player ->
          let revealed_string =
            match other_player.visible_card with
            | Some card -> [%string " Revealed: %{card#Card}"]
            | None -> ""
          in
          [%string
            "Player %{other_player.player_id#Player_id} - Coins: \
             %{other_player.coins#Int} %{revealed_string}"])
    in
    String.concat_lines (coins :: other_players)
end

module type Player_io_S = sig
  type t

  val choose_action :
    t -> visible_game_state:Visible_game_state.t -> Action.t Deferred.t

  val choose_assasination_response :
    t ->
    visible_game_state:Visible_game_state.t ->
    asassinating_player_id:Player_id.t ->
    [ `Allow | `Block ] Deferred.t

  val choose_foreign_aid_response :
    t ->
    visible_game_state:Visible_game_state.t ->
    unit ->
    cancelled_reason:Cancelled_reason.t Deferred.t ->
    [ `Allow | `Block ] Deferred.t

  val choose_steal_response :
    t ->
    visible_game_state:Visible_game_state.t ->
    stealing_player_id:Player_id.t ->
    [ `Allow | `Block of [ `Captain | `Ambassador ] ] Deferred.t

  val choose_cards_to_return :
    t ->
    visible_game_state:Visible_game_state.t ->
    Card.t ->
    Card.t ->
    Hand.t ->
    (Card.t * Card.t) Deferred.t

  val reveal_card :
    t ->
    visible_game_state:Visible_game_state.t ->
    card_1:Card.t ->
    card_2:Card.t ->
    [ `Card_1 | `Card_2 ] Deferred.t

  val offer_challenge :
    t ->
    visible_game_state:Visible_game_state.t ->
    Player_id.t ->
    Challengable.t ->
    cancelled_reason:Cancelled_reason.t Deferred.t ->
    [ `No_challenge | `Challenge ] Deferred.t

  val notify_of_game_start :
    t -> visible_game_state:Visible_game_state.t -> unit Deferred.t

  val notify_of_action_choice : t -> Player_id.t -> Action.t -> unit Deferred.t
  val notify_of_lost_influence : t -> Player_id.t -> Card.t -> unit Deferred.t
  val notify_of_new_card : t -> Card.t -> unit Deferred.t

  val notify_of_challenge :
    t ->
    challenging_player_id:Player_id.t ->
    has_required_card:bool ->
    unit Deferred.t
end

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

  val create :
    Player_id.t ->
    card_1:Card.t ->
    card_2:Card.t ->
    other_players:Player_id.t list ->
    model:string ->
    t Deferred.t
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

  let create player_id ~card_1 ~card_2 ~other_players ~model =
    let events = Queue.create () in
    Queue.enqueue events (Developer, Rules.rules);
    let other_players_string =
      other_players
      |> List.map ~f:(fun player_id ->
             [%string "Player id: %{player_id#Player_id}"])
      |> String.concat ~sep:", "
    in
    Queue.enqueue events
      ( Developer,
        [%string
          "The game is starting. You are player %{player_id#Player_id}. Your \
           starting cards are %{card_1#Card} and %{card_2#Card}. Players \
           %{other_players_string} are your opponents."] );

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
  let notify_of_game_start _t ~visible_game_state:_ = return ()

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

  val llm :
    Player_id.t ->
    card_1:Card.t ->
    card_2:Card.t ->
    other_players:Player_id.t list ->
    model:string ->
    t Deferred.t

  val cli : Player_id.t -> t Deferred.t
  val create : (module Player_io_S with type t = 'a) -> 'a -> t

  include Player_io_S with type t := t
end = struct
  type t = Packed : (module Player_io_S with type t = 'a) * 'a -> t

  let create (type a) (module M : Player_io_S with type t = a) implementation =
    Packed ((module M), implementation)

  let llm id ~card_1 ~card_2 ~other_players ~model =
    let (module M) =
      (module Llm_player_io : Player_io_S with type t = Llm_player_io.t)
    in
    let%map implementation =
      Llm_player_io.create id ~card_1 ~card_2 ~other_players ~model
    in
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

module Player = struct
  type t = {
    id : Player_id.t;
    player_io : (Player_io.t[@sexp.opaque]);
    coins : int;
    hand : Hand.t;
  }
  [@@deriving sexp_of]

  let id t = t.id
  (* let coins t = t.coins
  let hand t = t.hand *)
end

module Game_state = struct
  type t = {
    players : Player.t list;  (** the first player in the list is active *)
    deck : Card.t list;
  }
  [@@deriving sexp_of]

  let deck t = t.deck
  let players t = t.players

  let modify_player t id ~f =
    let new_players =
      List.map t.players ~f:(fun player ->
          if Player_id.equal player.id id then f player else player)
    in
    { t with players = new_players }

  let get_active_player t = List.hd_exn t.players
  let get_active_player_id t = get_active_player t |> Player.id

  let player_in_game t id =
    List.find t.players ~f:(fun player -> Player_id.equal id player.id)
    |> Option.is_some

  let to_string_pretty t =
    let sorted_players =
      List.sort t.players ~compare:(fun a b -> Player_id.compare a.id b.id)
    in
    List.map sorted_players ~f:(fun player ->
        let hand_sexp = [%sexp_of: Hand.t] player.hand in
        let body =
          [%string
            "Player id: %{player.id#Player_id}\tCoins: \
             %{player.coins#Int}\tHand: %{hand_sexp#Sexp}"]
        in
        let prefix =
          if Player_id.equal player.id (get_active_player t).id then "->"
          else "  "
        in
        prefix ^ body)
    |> String.concat_lines

  let modify_active_player t ~f =
    let active_player = get_active_player t in

    { t with players = f active_player :: List.tl_exn t.players }

  let end_turn t =
    { t with players = List.tl_exn t.players @ [ List.hd_exn t.players ] }

  let get_player_if_exists t id =
    List.find t.players ~f:(fun p -> Player_id.equal p.id id)

  let get_player_exn t id =
    List.find_exn t.players ~f:(fun p -> Player_id.equal p.id id)

  let is_valid_action t active_player_id action =
    let player = get_player_exn t active_player_id in
    let cost, target_player_id =
      match action with
      | `Income | `Foreign_aid | `Tax | `Exchange -> (0, None)
      | `Coup target_player_id -> (7, Some target_player_id)
      | `Assassinate target_player_id -> (3, Some target_player_id)
      | `Steal target_player_id -> (0, Some target_player_id)
    in
    let has_enough_coins = player.coins >= cost
    and is_valid_target =
      Option.value_map target_player_id ~default:true
        ~f:(fun target_player_id ->
          List.find_map t.players ~f:(fun p ->
              if Player_id.equal p.id target_player_id then Some p else None)
          |> Option.is_some)
    and not_targeting_self =
      Option.value_map target_player_id ~default:true
        ~f:(fun target_player_id ->
          not (Player_id.equal target_player_id active_player_id))
    in
    has_enough_coins && is_valid_target && not_targeting_self

  let has_card t id card =
    let player = get_player_exn t id in
    match player.hand with
    | Hand.Both (card_1, card_2) ->
        List.mem [ card_1; card_2 ] card ~equal:Card.equal
    | Hand.One { hidden; revealed = _ } -> Card.equal hidden card

  let to_visible_game_state t player_id =
    let player = get_player_exn t player_id in
    let other_players =
      List.filter t.players ~f:(fun p -> not (Player_id.equal p.id player_id))
      |> List.map ~f:(fun p ->
             let visible_card =
               match p.hand with
               | Hand.Both _ -> None
               | Hand.One { hidden = _; revealed } -> Some revealed
             in
             {
               Visible_game_state.Other_player.player_id = p.id;
               visible_card;
               coins = p.coins;
             })
    in
    {
      Visible_game_state.hand = player.hand;
      coins = player.coins;
      other_players;
      active_player_id = get_active_player_id t;
    }

  let sorted_deck =
    [ 1; 2; 3; 4 ]
    |> List.concat_map ~f:(fun _i ->
           [ Card.Duke; Assassin; Captain; Ambassador; Contessa ])

  let num_players = 3

  let create_player ?create_ws_player_io id card_1 card_2 =
    match (create_ws_player_io, Player_id.to_int id) with
    | (Some create_player_io, 2) as _i ->
        let%map player_io = create_player_io id card_1 card_2 in
        { Player.id; coins = 2; player_io; hand = Hand.Both (card_1, card_2) }
    | _, ((0 | 1 | 2 | 3) as i) ->
        let other_players =
          List.range 0 (num_players - 1)
          |> List.filter ~f:(fun i -> i <> Player_id.to_int id)
          |> List.map ~f:Player_id.of_int
        in
        let model =
          match i with
          | 0 -> Llm_player_io.gpt_4o_mini
          | 1 -> Llm_player_io.gpt_4o_mini
          | 2 -> Llm_player_io.gpt_4o_mini
          | 3 -> Llm_player_io.gpt_4o_mini
          | 4 -> Llm_player_io.gpt_4o
          | _ -> Llm_player_io.o3_mini
        in
        let%map player_io =
          Player_io.llm id ~card_1 ~card_2 ~other_players ~model
        in
        { Player.id; coins = 2; player_io; hand = Hand.Both (card_1, card_2) }
    | _ ->
        let%map player_io = Player_io.cli id in
        { Player.id; coins = 2; player_io; hand = Hand.Both (card_1, card_2) }

  let init ?create_ws_player_io () =
    let deck =
      Random.self_init ();
      let rand = fun () -> Random.bits () in
      List.sort ~compare:(fun _ _ -> (rand () mod 3) - 1) sorted_deck
    in
    let paired_deck, _always_none =
      List.fold deck ~init:([], None) ~f:(fun (pairs_acc, prev) card ->
          match prev with
          | None -> (pairs_acc, Some card)
          | Some prev -> ((card, prev) :: pairs_acc, None))
    in
    let player_cards, remaining_cards = List.split_n paired_deck num_players in
    let%map players =
      Deferred.List.mapi ~how:`Sequential player_cards
        ~f:(fun id (card_1, card_2) ->
          create_player ?create_ws_player_io (Player_id.of_int id) card_1 card_2)
    in
    let deck =
      List.concat_map remaining_cards ~f:(fun (carrd_1, card_2) ->
          [ carrd_1; card_2 ])
    in
    { players; deck }
end

(* Actions *)

let take_income game_state =
  Game_state.modify_active_player game_state ~f:(fun active_player ->
      { active_player with coins = active_player.coins + 1 })

let game_over = Deferred.Result.fail

(* maybe return [Game_state.t * Player_id.t option] *)
let lose_influence game_state target_player_id =
  let%bind.Deferred.Result new_game_state, revealed_card =
    let player = Game_state.get_player_exn game_state target_player_id in
    match player.hand with
    | Hand.Both (card_1, card_2) ->
        let%map.Deferred.Result revealed_card_choice =
          Player_io.reveal_card player.player_io
            ~visible_game_state:
              (Game_state.to_visible_game_state game_state player.id)
            ~card_1 ~card_2
          >>| Result.return
        in
        let new_hand, revealed_card =
          match revealed_card_choice with
          | `Card_1 -> (Hand.One { hidden = card_2; revealed = card_1 }, card_1)
          | `Card_2 -> (Hand.One { hidden = card_1; revealed = card_2 }, card_2)
        in
        let new_game_state =
          Game_state.modify_player game_state target_player_id ~f:(fun player ->
              { player with hand = new_hand })
        in
        (new_game_state, revealed_card)
    | Hand.One { hidden; revealed = _ } -> (
        let remaining_players =
          List.filter game_state.players ~f:(fun player ->
              not (Player_id.equal player.id target_player_id))
        in
        match List.length remaining_players with
        | 1 -> game_over { game_state with players = remaining_players }
        | _ ->
            Deferred.Result.return
              ({ game_state with players = remaining_players }, hidden))
  in
  let%map.Deferred.Result () =
    Game_state.players new_game_state
    |> List.map ~f:(fun player ->
           Player_io.notify_of_lost_influence player.player_io target_player_id
             revealed_card)
    |> Deferred.all_unit >>| Result.return
  in
  new_game_state

let randomly_get_new_card game_state active_player_id card_to_replace =
  let deck_with_replacement = card_to_replace :: Game_state.deck game_state in
  let shuffled_deck = List.permute deck_with_replacement in
  let replacement_card = List.hd_exn shuffled_deck in
  let remaining_deck = List.tl_exn shuffled_deck in
  let new_game_state =
    Game_state.modify_player game_state active_player_id ~f:(fun player ->
        let new_hand =
          match player.hand with
          | Hand.Both (card_1, card_2) -> (
              match Card.equal card_1 card_to_replace with
              | true -> Hand.Both (replacement_card, card_2)
              | false -> Hand.Both (card_1, replacement_card))
          | Hand.One { hidden = _; revealed } ->
              Hand.One { hidden = replacement_card; revealed }
        in
        { player with hand = new_hand })
  in
  let%map () =
    Player_io.notify_of_new_card
      (Game_state.get_player_exn new_game_state active_player_id).player_io
      replacement_card
  in
  { new_game_state with deck = remaining_deck }

let handle_response_race game_state acting_player_id ~f =
  (* TODO: there's probably a memory leak in here *)
  let other_player_responded = Ivar.create () in
  let responses =
    Game_state.players game_state
    |> List.filter ~f:(fun player ->
           not (Player_id.equal player.id acting_player_id))
    |> List.map ~f:(fun player ->
           f player (Ivar.read other_player_responded) >>| fun response ->
           (player.id, response))
  in
  let blocked =
    responses
    |> List.map
         ~f:
           (Deferred.bind ~f:(fun ((player_id : Player_id.t), response) ->
                match response with
                | `Allow -> Deferred.never ()
                | `Block ->
                    Ivar.fill_if_empty other_player_responded
                      (Cancelled_reason.Other_player_responded player_id);
                    return (`Blocked_by player_id)))
    |> Deferred.any
  in
  let allowed =
    responses
    |> List.map
         ~f:
           (Deferred.bind ~f:(fun ((_ : Player_id.t), response) ->
                match response with
                | `Allow -> return `Allow
                | `Block -> Deferred.never ()))
    |> Deferred.all
    |> Deferred.map ~f:(fun _ -> `Allow)
  in
  Deferred.any [ blocked; allowed ]

let handle_challenge game_state acting_player_id (action : Challengable.t) =
  let%bind challenge_result =
    handle_response_race game_state acting_player_id
      ~f:(fun player cancelled_reason ->
        Player_io.offer_challenge player.player_io
          ~visible_game_state:
            (Game_state.to_visible_game_state game_state player.id)
          acting_player_id action ~cancelled_reason
        >>| function
        | `No_challenge -> `Allow
        | `Challenge -> `Block)
  in
  match challenge_result with
  | `Allow -> Deferred.Result.return (`Failed_or_no_challenge game_state)
  | `Blocked_by challenger_player_id -> (
      let required_card = Challengable.required_card action in
      let has_required_card =
        Game_state.has_card game_state acting_player_id required_card
      in
      let%bind.Deferred.Result () =
        Player_io.notify_of_challenge
          (Game_state.get_player_exn game_state acting_player_id).player_io
          ~challenging_player_id:challenger_player_id ~has_required_card
        >>| Result.return
      in
      match has_required_card with
      | false ->
          let%map.Deferred.Result new_game_state =
            (* TODO don't know who challenged *)
            lose_influence game_state acting_player_id
          in
          `Successfully_challenged new_game_state
      | true ->
          let%bind.Deferred.Result game_state_after_new_card =
            (* TODO don't know who challenged *)
            randomly_get_new_card game_state acting_player_id required_card
            >>| Result.return
          in
          let%map.Deferred.Result new_game_state =
            lose_influence game_state_after_new_card challenger_player_id
          in
          `Failed_or_no_challenge new_game_state)

let assassinate game_state active_player_id target_player_id =
  let game_state =
    Game_state.modify_active_player game_state ~f:(fun player ->
        { player with coins = player.coins - 3 })
  in
  match%bind.Deferred.Result
    handle_challenge game_state active_player_id (`Assassinate target_player_id)
  with
  | `Successfully_challenged post_challenge_game_state ->
      Deferred.Result.return post_challenge_game_state
  | `Failed_or_no_challenge post_challenge_game_state -> (
      match
        Game_state.get_player_if_exists post_challenge_game_state
          target_player_id
      with
      | None -> Deferred.Result.return post_challenge_game_state
      | Some target_player -> (
          match%bind.Deferred.Result
            Player_io.choose_assasination_response target_player.player_io
              ~visible_game_state:
                (Game_state.to_visible_game_state game_state target_player_id)
              ~asassinating_player_id:active_player_id
            >>| Result.return
          with
          | `Allow -> lose_influence post_challenge_game_state target_player_id
          | `Block -> (
              match%bind.Deferred.Result
                handle_challenge post_challenge_game_state target_player_id
                  `Block_assassination
              with
              | `Successfully_challenged post_second_challenge_game_state ->
                  if
                    Game_state.player_in_game post_second_challenge_game_state
                      target_player_id
                  then
                    lose_influence post_second_challenge_game_state
                      target_player_id
                  else Deferred.Result.return post_second_challenge_game_state
              | `Failed_or_no_challenge post_second_challenge_game_state ->
                  Deferred.Result.return post_second_challenge_game_state)))

let take_foreign_aid game_state =
  let take_two_coins game_state =
    Game_state.modify_active_player game_state ~f:(fun player ->
        { player with coins = player.coins + 2 })
  in
  match%bind
    handle_response_race game_state (Game_state.get_active_player game_state).id
      ~f:(fun player cancelled_reason ->
        Player_io.choose_foreign_aid_response player.player_io
          ~visible_game_state:
            (Game_state.to_visible_game_state game_state player.id)
          () ~cancelled_reason
        >>| function
        | `Allow -> `Allow
        | `Block -> `Block)
  with
  | `Blocked_by blocking_player_id -> (
      match%bind.Deferred.Result
        handle_challenge game_state blocking_player_id `Block_foreign_aid
      with
      | `Successfully_challenged post_second_challenge_game_state ->
          Deferred.Result.return
            (take_two_coins post_second_challenge_game_state)
      | `Failed_or_no_challenge post_second_challenge_game_state ->
          Deferred.Result.return post_second_challenge_game_state)
  | `Allow -> Deferred.Result.return (take_two_coins game_state)

let coup game_state target_player_id =
  let game_state =
    Game_state.modify_active_player game_state ~f:(fun player ->
        { player with coins = player.coins - 7 })
  in
  lose_influence game_state target_player_id

let take_tax game_state =
  match%map.Deferred.Result
    handle_challenge game_state (Game_state.get_active_player game_state).id
      `Tax
  with
  | `Successfully_challenged post_challenge_game_state ->
      post_challenge_game_state
  | `Failed_or_no_challenge post_challenge_game_state ->
      Game_state.modify_active_player post_challenge_game_state
        ~f:(fun player -> { player with coins = player.coins + 3 })

let steal game_state target_player_id =
  let steal_two_coins game_state =
    let target_player = Game_state.get_player_exn game_state target_player_id in
    let num_coins_to_steal = Int.min 2 target_player.coins in
    Game_state.modify_player game_state target_player_id ~f:(fun player ->
        { player with coins = player.coins - num_coins_to_steal })
    |> Game_state.modify_active_player ~f:(fun player ->
           { player with coins = player.coins + num_coins_to_steal })
  in
  match%bind.Deferred.Result
    handle_challenge game_state (Game_state.get_active_player game_state).id
      (`Steal target_player_id)
  with
  | `Successfully_challenged post_challenge_game_state ->
      Deferred.Result.return post_challenge_game_state
  | `Failed_or_no_challenge post_challenge_game_state -> (
      match
        Game_state.get_player_if_exists post_challenge_game_state
          target_player_id
      with
      | None -> Deferred.Result.return post_challenge_game_state
      | Some target_player -> (
          match%bind.Deferred.Result
            Player_io.choose_steal_response target_player.player_io
              ~visible_game_state:
                (Game_state.to_visible_game_state post_challenge_game_state
                   target_player.id)
              ~stealing_player_id:
                (Game_state.get_active_player post_challenge_game_state).id
            >>| Result.return
          with
          | `Allow ->
              steal_two_coins post_challenge_game_state
              |> Deferred.Result.return
          | `Block blocking_card -> (
              match%map.Deferred.Result
                handle_challenge post_challenge_game_state target_player_id
                  (`Block_steal blocking_card)
              with
              | `Successfully_challenged post_second_challenge_game_state ->
                  post_second_challenge_game_state
              | `Failed_or_no_challenge post_second_challenge_game_state -> (
                  match
                    Game_state.get_player_if_exists
                      post_second_challenge_game_state target_player_id
                  with
                  | None -> post_second_challenge_game_state
                  | Some _target_player ->
                      steal_two_coins post_second_challenge_game_state))))

let exchange game_state =
  let active_player = Game_state.get_active_player game_state in
  match Game_state.deck game_state with
  | card_choice_1 :: card_choice_2 :: rest ->
      let%map.Deferred returned_card_1, returned_card_2 =
        Player_io.choose_cards_to_return active_player.player_io
          ~visible_game_state:
            (Game_state.to_visible_game_state game_state active_player.id)
          card_choice_1 card_choice_2 active_player.hand
      in
      let remove_cards cards_chosen_from =
        List.fold cards_chosen_from
          ~init:(`Both (returned_card_1, returned_card_2), [])
          ~f:(fun (remaining_to_remove_acc, hand_acc) card ->
            match remaining_to_remove_acc with
            | `Both (card_to_remove_1, card_to_remove_2) ->
                if Card.equal card card_to_remove_1 then
                  (`One card_to_remove_2, hand_acc)
                else if Card.equal card card_to_remove_2 then
                  (`One card_to_remove_1, hand_acc)
                else
                  (`Both (card_to_remove_1, card_to_remove_2), card :: hand_acc)
            | `One card_to_remove ->
                if Card.equal card card_to_remove then (`None, hand_acc)
                else (`One card_to_remove, card :: hand_acc)
            | `None -> (`None, card :: hand_acc))
        |> Tuple2.get2
      in
      let new_hand =
        match active_player.hand with
        | Hand.One { hidden; revealed } ->
            let new_cards =
              remove_cards [ card_choice_1; card_choice_2; hidden ]
            in
            Hand.One { hidden = List.hd_exn new_cards; revealed }
        | Hand.Both (card_1, card_2) ->
            let new_cards =
              remove_cards [ card_choice_1; card_choice_2; card_1; card_2 ]
            in
            Hand.Both
              (List.hd_exn new_cards, List.hd_exn (List.tl_exn new_cards))
      in
      let cards_back_in_deck =
        {
          game_state with
          deck = List.permute (returned_card_1 :: returned_card_2 :: rest);
        }
      in
      Game_state.modify_player cards_back_in_deck active_player.id
        ~f:(fun player -> { player with hand = new_hand })
  | _ -> failwith "BUG: Too few cards in the deck"

let take_turn_result game_state =
  let active_player = Game_state.get_active_player game_state in
  let%bind.Deferred.Result action =
    Deferred.repeat_until_finished () (fun () ->
        let%map action =
          Player_io.choose_action active_player.player_io
            ~visible_game_state:
              (Game_state.to_visible_game_state game_state active_player.id)
        in
        match Game_state.is_valid_action game_state active_player.id action with
        | true -> `Finished action
        | false ->
            print_endline "Invalid action";
            `Repeat ())
    >>| Result.return
  in
  match (action : Action.t) with
  | `Income ->
      let%map.Deferred.Result () =
        Game_state.players game_state
        |> List.map ~f:(fun player ->
               Player_io.notify_of_action_choice player.player_io
                 active_player.id action)
        |> Deferred.all_unit >>| Result.return
      in
      take_income game_state
  | `Assassinate target_player_id ->
      assassinate game_state active_player.id target_player_id
  | `Foreign_aid -> take_foreign_aid game_state
  | `Coup target_player_id ->
      let%bind.Deferred.Result () =
        Game_state.players game_state
        |> List.map ~f:(fun player ->
               Player_io.notify_of_action_choice player.player_io
                 active_player.id action)
        |> Deferred.all_unit >>| Result.return
      in
      coup game_state target_player_id
  | `Tax -> take_tax game_state
  | `Steal target_player_id -> steal game_state target_player_id
  | `Exchange ->
      let%bind.Deferred.Result () =
        Game_state.players game_state
        |> List.map ~f:(fun player ->
               Player_io.notify_of_action_choice player.player_io
                 active_player.id action)
        |> Deferred.all_unit >>| Result.return
      in
      exchange game_state >>| Result.return

let take_turn game_state =
  (* print_s [%sexp (game_state : Game_state.t)]; *)
  print_endline (Game_state.to_string_pretty game_state);
  let _ = Game_state.to_string_pretty in
  match%bind take_turn_result game_state with
  | Ok game_state' -> return (`Repeat (Game_state.end_turn game_state'))
  | Error final_game_state -> return (`Finished final_game_state)

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
