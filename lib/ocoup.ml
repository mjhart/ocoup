open! Core
open! Async

(* TODO:
   - timeout for player action
   - validate responses from player
   - give more state to players
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
end

module Player_id : sig
  type t [@@deriving sexp, equal, compare]

  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
end = struct
  type t = int [@@deriving sexp, equal, compare]

  let of_int = Fn.id
  let to_int = Fn.id
  let to_string = Int.to_string
end

(* Actions a player may choose. Some actions have a target (represented by a player id). *)
module Action = struct
  type t =
    | Income (* take 1 coin from the Treasury *)
    | ForeignAid (* take 2 coins (subject to blocking) *)
    | Coup of
        Player_id.t (* pay 7 coins to launch a coup against target player *)
    | Tax (* Duke: take 3 coins from the Treasury *)
    | Assassinate of
        Player_id.t (* Assassin: pay 3 coins to assassinate target *)
    | Steal of Player_id.t (* Captain: steal 1 or 2 coins from target *)
    | Exchange (* Ambassador: exchange cards with the Court deck *)
  [@@deriving sexp]

  let valid_actions coins =
    let cost = function
      | Income -> 0
      | ForeignAid -> 0
      | Coup _ -> 7
      | Tax -> 3
      | Assassinate _ -> 3
      | Steal _ -> 2
      | Exchange -> 0
    in
    let all_actions =
      [
        Income;
        ForeignAid;
        Coup (Player_id.of_int (-1));
        Tax;
        Assassinate (Player_id.of_int (-1));
        Steal (Player_id.of_int (-1));
        Exchange;
      ]
    in
    List.filter all_actions ~f:(fun action -> coins >= cost action)

  let to_string = function
    | Income -> "Income"
    | ForeignAid -> "ForeignAid"
    | Coup target_player_id ->
        sprintf "Coup %d" (Player_id.to_int target_player_id)
    | Tax -> "Tax"
    | Assassinate target_player_id ->
        sprintf "Assassinate %d" (Player_id.to_int target_player_id)
    | Steal target_player_id ->
        sprintf "Steal %d" (Player_id.to_int target_player_id)
    | Exchange -> "Exchange"
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

module type Player_io_S = sig
  type t

  val choose_action : t -> Action.t Deferred.t

  val choose_assasination_response :
    t -> asassinating_player_id:Player_id.t -> [ `Allow | `Block ] Deferred.t

  val choose_foreign_aid_response :
    t ->
    unit ->
    cancelled_reason:Cancelled_reason.t Deferred.t ->
    [ `Allow | `Block ] Deferred.t

  val choose_steal_response :
    t ->
    stealing_player_id:Player_id.t ->
    [ `Allow | `Block of [ `Captain | `Ambassador ] ] Deferred.t

  val choose_cards_to_return :
    t -> Card.t -> Card.t -> Hand.t -> (Card.t * Card.t) Deferred.t

  val reveal_card :
    t -> card_1:Card.t -> card_2:Card.t -> [ `Card_1 | `Card_2 ] Deferred.t

  val offer_challenge :
    t ->
    Player_id.t ->
    Card.t ->
    cancelled_reason:Cancelled_reason.t Deferred.t ->
    [ `No_challenge | `Challenge ] Deferred.t

  val notify_of_action_choice : t -> Player_id.t -> Action.t -> unit Deferred.t
  val notify_of_lost_influence : t -> Player_id.t -> Card.t -> unit Deferred.t
  val notify_of_new_card : t -> Card.t -> unit Deferred.t
  (* 
          what to players have to learn about:
          - their initial cards
          - resolutions
            - steal
          - non-challengable or blockable actions
            - coup
          - game end
          *)
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

  let print_s t message =
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

  let choose_action t =
    with_stdin t ~f:(fun stdin ->
        print_endline t "Choose action (I/FA/A n/C n/T/S n/E)";
        match%bind Reader.read_line stdin with
        | `Eof -> unexpected_eof ()
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "I" ] -> return Action.Income
            | [ "FA" ] -> return Action.ForeignAid
            | [ "A"; n ] ->
                return (Action.Assassinate (Player_id.of_int (Int.of_string n)))
            | [ "C"; n ] ->
                return (Action.Coup (Player_id.of_int (Int.of_string n)))
            | [ "T" ] -> return Action.Tax
            | [ "S"; n ] ->
                return (Action.Steal (Player_id.of_int (Int.of_string n)))
            | [ "E" ] -> return Action.Exchange
            | _ ->
                print_endline t "Invalid action";
                return Action.Income))

  let choose_assasination_response t ~asassinating_player_id =
    with_stdin t ~f:(fun stdin ->
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

  let choose_steal_response t ~stealing_player_id =
    with_stdin t ~f:(fun stdin ->
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

  let choose_foreign_aid_response t () ~cancelled_reason =
    upon cancelled_reason (function
        | Cancelled_reason.Other_player_responded player_id ->
        print_endline t
          [%string
            "Player %{player_id#Player_id} has already blocked the foreign aid."]);
    with_stdin t ~f:(fun stdin ->
        match Deferred.is_determined cancelled_reason with
        | true -> return `Allow
        | false -> (
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

  let choose_cards_to_return t card_1 card_2 hand =
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
        print_endline t
          [%string
            "Cards to choose from:\n\
             %{cards_string}\n\
             Choose 2 cards to return by entering the indices of the cards you \
             want to return."];
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

  let reveal_card t ~card_1 ~card_2 =
    let cards_string =
      [ card_1; card_2 ]
      |> List.mapi ~f:(fun i card -> [%string "%{i#Int}: %{card#Card}"])
      |> String.concat ~sep:"\n"
    in
    with_stdin t ~f:(fun stdin ->
        print_endline t
          [%string
            "Cards to choose from:\n\
             %{cards_string}\n\
             Which card do you want to reveal? (1/2)"];
        match%map Reader.read_line stdin with
        | `Eof -> unexpected_eof ()
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "1" ] -> `Card_1
            | [ "2" ] -> `Card_2
            | _ ->
                print_endline t "Invalid action";
                `Card_1))

  let offer_challenge t acting_player_id action ~cancelled_reason =
    (* TODO: Don't know who [action] is targeting *)
    upon cancelled_reason (function
        | Cancelled_reason.Other_player_responded player_id ->
        print_endline t
          [%string "Player %{player_id#Player_id} has challenged the action."]);
    with_stdin t ~f:(fun stdin ->
        match Deferred.is_determined cancelled_reason with
        | true -> return `No_challenge
        | false -> (
            print_s t
              [%message
                "Challenge action? (Y/N)"
                  (action : Card.t)
                  (acting_player_id : Player_id.t)];
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
    print_s t
      [%message
        "Player chose action" (player_id : Player_id.t) (action : Action.t)]
    |> return

  let notify_of_new_card t card =
    print_s t [%message "Got new card" (card : Card.t)] |> return

  let notify_of_lost_influence t player_id card =
    print_s t
      [%message
        "Player lost influence" (player_id : Player_id.t) (card : Card.t)]
    |> return
end

module Llm_player_io : sig
  include Player_io_S

  val create :
    Player_id.t ->
    card_1:Card.t ->
    card_2:Card.t ->
    other_players:Player_id.t list ->
    t Deferred.t
end = struct
  type role = Developer | Assistant

  let role_to_string = function
    | Developer -> "developer"
    | Assistant -> "assistant"

  type t = {
    events : (role * string) Queue.t;
    player_id : Player_id.t;
    writer : Writer.t;
  }

  let create player_id ~card_1 ~card_2 ~other_players =
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
    { events; player_id; writer }

  let print_endline t message =
    ignore t.player_id;
    Writer.write_line t.writer message;
    ()

  let headers =
    Lazy.from_fun (fun () ->
        Http.Header.of_list
          [
            ("Content-Type", "application/json");
            ( "Authorization",
              "Bearer " ^ (Sys.getenv "OPENAI_API_KEY" |> Option.value_exn) );
          ])

  let send_request t prompt =
    let open Cohttp_async in
    Queue.enqueue t.events (Developer, prompt);
    Queue.iter t.events ~f:(fun (role, content) ->
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
            ("model", `String "gpt-4o-mini");
            ("messages", `List messages);
            ("response_format", `Assoc [ ("type", `String "json_object") ]);
          ]
      in
      Body.of_string (Yojson.Basic.to_string json)
    in
    (* print_s [%sexp (post_body : Body.t)]; *)
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

  let choose_action t =
    let prompt =
      "Choose action: Income | ForeignAid | Assassinate player_id | Coup \
       target_player_id | Tax | Steal target_player_id | Exchange. Respond \
       with a json object with the key 'action' and the value being the action \
       you want to take. If the action requires a target player, additionally \
       provide the key 'target_player_id' and the value will be the the \
       player_id of the target player."
    in

    let%map response = send_request t prompt in
    let action = Yojson.Basic.Util.member "action" response in
    let target_player_id =
      Yojson.Basic.Util.member "target_player_id" response
    in
    match (action, target_player_id) with
    | `String "Income", `Null -> Action.Income
    | `String "ForeignAid", `Null -> Action.ForeignAid
    | `String "Assassinate", `Int target_player_id ->
        Action.Assassinate (Player_id.of_int target_player_id)
    | `String "Coup", `Int target_player_id ->
        Action.Coup (Player_id.of_int target_player_id)
    | `String "Tax", `Null -> Action.Tax
    | `String "Steal", `Int target_player_id ->
        Action.Steal (Player_id.of_int target_player_id)
    | `String "Exchange", `Null -> Action.Exchange
    | _ ->
        print_endline t "Invalid action";
        Action.Income

  let choose_assasination_response t ~asassinating_player_id =
    let prompt =
      [%string
        "You are being assasinated by player \
         %{asassinating_player_id#Player_id}. Respond with a json object with \
         the key 'response' and the value being 'Allow' or 'Block'."]
    in
    let%map response = send_request t prompt in
    let response = Yojson.Basic.Util.member "response" response in
    match response with
    | `String "Allow" -> `Allow
    | `String "Block" -> `Block
    | _ ->
        print_endline t "Invalid response";
        `Allow

  let choose_foreign_aid_response t () ~cancelled_reason:_ =
    let prompt =
      [%string
        "Player is attempting to take foreign aid. Respond with a json object \
         with the key 'response' and the value being 'Allow' or 'Block'."]
    in
    let%map response = send_request t prompt in
    let response = Yojson.Basic.Util.member "response" response in
    match response with
    | `String "Allow" -> `Allow
    | `String "Block" -> `Block
    | _ ->
        print_endline t "Invalid response";
        `Allow

  let choose_steal_response t ~stealing_player_id =
    let prompt =
      [%string
        "Player %{stealing_player_id#Player_id} is attempting to steal from \
         you. Respond with a json object with the key 'response' and the value \
         being 'Allow' or 'Block'. If you choose to block, additionally \
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

  let choose_cards_to_return t card_1 card_2 hand =
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
        "You are exchanging cards. The cards available to you are \
         [%{cards_string}]. Choose two cards to return to the deck. Respond \
         with a json array of size exactly 2 containing the indices of the \
         cards you want to return. The indices are 0-indexed."]
    in
    let%map response = send_request t prompt in
    let indices = Yojson.Basic.Util.member "response" response in
    match Yojson.Basic.Util.to_list indices with
    | [ `Int index_1; `Int index_2 ] ->
        (List.nth_exn cards index_1, List.nth_exn cards index_2)
    | _ ->
        print_endline t "Invalid response";
        (card_1, card_2)

  let reveal_card t ~card_1 ~card_2 =
    let cards_string =
      [ card_1; card_2 ] |> List.map ~f:Card.to_string |> String.concat
    in
    let prompt =
      [%string
        "You are revealing a card. The cards available to you are \
         %{cards_string}. Which card do you want to reveal? (1/2)"]
    in
    let%map response = send_request t prompt in
    let response = Yojson.Basic.Util.member "response" response in
    match response with
    | `String "1" -> `Card_1
    | `String "2" -> `Card_2
    | _ ->
        print_endline t "Invalid response";
        `Card_1

  let offer_challenge t acting_player_id card ~cancelled_reason:_ =
    let prompt =
      [%string
        "Player %{acting_player_id#Player_id} is claiming card %{card#Card}. \
         Respond with a json object with the key 'response' and the value \
         being 'No_challenge' or 'Challenge'."]
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
end

module Player_io : sig
  type t = Cli of Cli_player_io.t | Llm of Llm_player_io.t

  include Player_io_S with type t := t
end = struct
  type t = Cli of Cli_player_io.t | Llm of Llm_player_io.t

  let choose_action t =
    match t with
    | Cli cli -> Cli_player_io.choose_action cli
    | Llm llm -> Llm_player_io.choose_action llm

  let choose_assasination_response t ~asassinating_player_id =
    match t with
    | Cli cli ->
        Cli_player_io.choose_assasination_response cli ~asassinating_player_id
    | Llm llm ->
        Llm_player_io.choose_assasination_response llm ~asassinating_player_id

  let choose_foreign_aid_response t () ~cancelled_reason =
    match t with
    | Cli cli ->
        Cli_player_io.choose_foreign_aid_response cli () ~cancelled_reason
    | Llm llm ->
        Llm_player_io.choose_foreign_aid_response llm () ~cancelled_reason

  let choose_steal_response t ~stealing_player_id =
    match t with
    | Cli cli -> Cli_player_io.choose_steal_response cli ~stealing_player_id
    | Llm llm -> Llm_player_io.choose_steal_response llm ~stealing_player_id

  let choose_cards_to_return t card_1 card_2 _hand =
    match t with
    | Cli cli -> Cli_player_io.choose_cards_to_return cli card_1 card_2 _hand
    | Llm llm -> Llm_player_io.choose_cards_to_return llm card_1 card_2 _hand

  let reveal_card t ~card_1 ~card_2 =
    match t with
    | Cli cli -> Cli_player_io.reveal_card cli ~card_1 ~card_2
    | Llm llm -> Llm_player_io.reveal_card llm ~card_1 ~card_2

  let offer_challenge t acting_player_id card ~cancelled_reason =
    match t with
    | Cli cli ->
        Cli_player_io.offer_challenge cli acting_player_id card
          ~cancelled_reason
    | Llm llm ->
        Llm_player_io.offer_challenge llm acting_player_id card
          ~cancelled_reason

  let notify_of_action_choice t player_id action =
    match t with
    | Cli cli -> Cli_player_io.notify_of_action_choice cli player_id action
    | Llm llm -> Llm_player_io.notify_of_action_choice llm player_id action

  let notify_of_lost_influence t player_id card =
    match t with
    | Cli cli -> Cli_player_io.notify_of_lost_influence cli player_id card
    | Llm llm -> Llm_player_io.notify_of_lost_influence llm player_id card

  let notify_of_new_card t card =
    match t with
    | Cli cli -> Cli_player_io.notify_of_new_card cli card
    | Llm llm -> Llm_player_io.notify_of_new_card llm card
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
  let _get_active_player_id t = get_active_player t |> Player.id

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
      | Action.Income | ForeignAid | Tax | Exchange -> (0, None)
      | Coup target_player_id -> (7, Some target_player_id)
      | Assassinate target_player_id -> (3, Some target_player_id)
      | Steal target_player_id -> (0, Some target_player_id)
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

  let _valid_actions t active_player_id =
    let player = get_player_exn t active_player_id in
    Action.valid_actions player.coins

  let sorted_deck =
    [ 1; 2; 3; 4 ]
    |> List.concat_map ~f:(fun _i ->
           [ Card.Duke; Assassin; Captain; Ambassador; Contessa ])

  let num_players = 3

  let create_player id card_1 card_2 =
    if Player_id.to_int id = 1 || Player_id.to_int id = 2 then
      let other_players =
        List.range 0 (num_players - 1)
        |> List.filter ~f:(fun i -> i <> Player_id.to_int id)
        |> List.map ~f:Player_id.of_int
      in
      let%map llm_player_io =
        Llm_player_io.create id ~card_1 ~card_2 ~other_players
      in
      let player_io = Player_io.Llm llm_player_io in

      { Player.id; coins = 2; player_io; hand = Hand.Both (card_1, card_2) }
    else
      let%map cli_player_io = Cli_player_io.cli_player id in
      let player_io = Player_io.Cli cli_player_io in
      { Player.id; coins = 2; player_io; hand = Hand.Both (card_1, card_2) }

  let init () =
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
          create_player (Player_id.of_int id) card_1 card_2)
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

let lose_influence game_state target_player_id =
  let%bind.Deferred.Result new_game_state, revealed_card =
    let player = Game_state.get_player_exn game_state target_player_id in
    match player.hand with
    | Hand.Both (card_1, card_2) ->
        let%map.Deferred.Result revealed_card_choice =
          Player_io.reveal_card player.player_io ~card_1 ~card_2
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

let handle_challenge game_state acting_player_id (action : Card.t) =
  let%bind challenge_result =
    handle_response_race game_state acting_player_id
      ~f:(fun player cancelled_reason ->
        Player_io.offer_challenge player.player_io acting_player_id action
          ~cancelled_reason
        >>| function
        | `No_challenge -> `Allow
        | `Challenge -> `Block)
  in
  match challenge_result with
  | `Allow -> Deferred.Result.return (`Failed_or_no_challenge game_state)
  | `Blocked_by challenger_player_id -> (
      let card_to_replace = action in
      match Game_state.has_card game_state acting_player_id card_to_replace with
      | false ->
          let%map.Deferred.Result new_game_state =
            lose_influence game_state acting_player_id
          in
          `Successfully_challenged new_game_state
      | true ->
          let%bind.Deferred.Result game_state_after_new_card =
            randomly_get_new_card game_state acting_player_id card_to_replace
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
    handle_challenge game_state active_player_id Assassin
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
              ~asassinating_player_id:active_player_id
            >>| Result.return
          with
          | `Allow -> lose_influence post_challenge_game_state target_player_id
          | `Block -> (
              match%bind.Deferred.Result
                handle_challenge post_challenge_game_state target_player_id
                  Contessa
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
        Player_io.choose_foreign_aid_response player.player_io ()
          ~cancelled_reason
        >>| function
        | `Allow -> `Allow
        | `Block -> `Block)
  with
  | `Blocked_by blocking_player_id -> (
      match%bind.Deferred.Result
        handle_challenge game_state blocking_player_id Duke
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
      Duke
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
      Captain
  with
  | `Successfully_challenged post_challenge_game_state ->
      Deferred.Result.return post_challenge_game_state
  | `Failed_or_no_challenge post_challenge_game_state -> (
      (* TODO make sure target still alive *)
      match
        Game_state.get_player_if_exists post_challenge_game_state
          target_player_id
      with
      | None -> Deferred.Result.return post_challenge_game_state
      | Some target_player -> (
          match%bind.Deferred.Result
            Player_io.choose_steal_response target_player.player_io
              ~stealing_player_id:(Game_state.get_active_player game_state).id
            >>| Result.return
          with
          | `Allow ->
              steal_two_coins post_challenge_game_state
              |> Deferred.Result.return
          | `Block blocking_card -> (
              match%map.Deferred.Result
                handle_challenge post_challenge_game_state target_player_id
                  (match blocking_card with
                  | `Ambassador -> Ambassador
                  | `Captain -> Captain)
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
        Player_io.choose_cards_to_return active_player.player_io card_choice_1
          card_choice_2 active_player.hand
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
        let%map action = Player_io.choose_action active_player.player_io in
        match Game_state.is_valid_action game_state active_player.id action with
        | true -> `Finished action
        | false ->
            print_endline "Invalid action";
            `Repeat ())
    >>| Result.return
  in
  match (action : Action.t) with
  | Income ->
      let%map.Deferred.Result () =
        Game_state.players game_state
        |> List.map ~f:(fun player ->
               Player_io.notify_of_action_choice player.player_io
                 active_player.id action)
        |> Deferred.all_unit >>| Result.return
      in
      take_income game_state
  | Assassinate target_player_id ->
      assassinate game_state active_player.id target_player_id
  | ForeignAid -> take_foreign_aid game_state
  | Coup target_player_id ->
      let%bind.Deferred.Result () =
        Game_state.players game_state
        |> List.map ~f:(fun player ->
               Player_io.notify_of_action_choice player.player_io
                 active_player.id action)
        |> Deferred.all_unit >>| Result.return
      in
      coup game_state target_player_id
  | Tax -> take_tax game_state
  | Steal target_player_id -> steal game_state target_player_id
  | Exchange -> exchange game_state >>| Result.return

let take_turn game_state =
  print_newline ();
  (* print_s [%sexp (game_state : Game_state.t)]; *)
  print_endline (Game_state.to_string_pretty game_state);
  match%bind take_turn_result game_state with
  | Ok game_state' -> return (`Repeat (Game_state.end_turn game_state'))
  | Error final_game_state -> return (`Finished final_game_state)

let run_game () =
  let%bind game_state = Game_state.init () in
  let%map final_game_state =
    Deferred.repeat_until_finished game_state take_turn
  in
  print_s [%sexp (final_game_state : Game_state.t)]
