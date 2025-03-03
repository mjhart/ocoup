open! Core
open! Async

(* TODO:
   - implement other actions
   - timeout for player action
   - implement players
   - tell players about stuff
   - better game state printing
 *)

(* The characters (and cards) available in the game *)
module Card = struct
  type t = Duke | Assassin | Captain | Ambassador | Contessa
  [@@deriving equal, sexp]
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
  (* module Challengable_actions = struct
    type t = Tax | Assassinate | Steal | Exchange
  end *)

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
  val choose_assasination_response : t -> unit -> [ `Allow | `Block ] Deferred.t

  val choose_foreign_aid_response :
    t ->
    unit ->
    cancelled_reason:Cancelled_reason.t Deferred.t ->
    [ `Allow | `Block ] Deferred.t

  val choose_steal_response :
    t -> unit -> [ `Allow | `Block of [ `Captain | `Ambassador ] ] Deferred.t

  val choose_cards_to_return :
    t -> Card.t -> Card.t -> Hand.t -> (Card.t * Card.t) Deferred.t

  val reveal_card : t -> unit -> [ `Card_1 | `Card_2 ] Deferred.t

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

  (* TODO: implement *)
  let choose_action t =
    (* TODO: implement for real. Make sure they have enough coins *)
    with_stdin t ~f:(fun stdin ->
        print_endline t "Choose action (I/FA/C/T/S/E)";
        match%bind Reader.read_line stdin with
        | `Eof -> failwith "EOF"
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
            | _ -> failwith "Invalid action"))

  let choose_assasination_response t () =
    with_stdin t ~f:(fun stdin ->
        print_endline t "Block assassination? (Y/N)";
        match%map Reader.read_line stdin with
        | `Eof -> failwith "EOF"
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "N" ] -> `Allow
            | [ "Y" ] -> `Block
            | _ -> failwith "Invalid action"))

  let choose_steal_response t () =
    with_stdin t ~f:(fun stdin ->
        print_endline t "Block steal? (C/A/N)";
        match%map Reader.read_line stdin with
        | `Eof -> failwith "EOF"
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "C" ] -> `Block `Captain
            | [ "A" ] -> `Block `Ambassador
            | [ "N" ] -> `Allow
            | _ -> failwith "Invalid action"))

  let choose_foreign_aid_response t () ~cancelled_reason =
    with_stdin t ~f:(fun stdin ->
        match Deferred.is_determined cancelled_reason with
        | true -> return `Allow
        | false -> (
            print_endline t "Block foreign aid? (Y/N)";
            match%map Reader.read_line stdin with
            | `Eof -> failwith "EOF"
            | `Ok action_str -> (
                match String.split action_str ~on:' ' with
                | [ "Y" ] -> `Block
                | [ "N" ] -> `Allow
                | _ -> failwith "Invalid action")))

  let choose_cards_to_return t card_1 card_2 _hand =
    (* TODO: implement *)
    with_stdin t ~f:(fun stdin ->
        print_endline t "Choose cards to return (1/2)";
        match%map Reader.read_line stdin with
        | `Eof -> failwith "EOF"
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "1" ] -> (card_1, card_2)
            | [ "2" ] -> (card_2, card_1)
            | _ -> failwith "Invalid action"))

  let reveal_card _t () = return `Card_1
  (* TODO: implement *)

  let offer_challenge t acting_player_id action ~cancelled_reason =
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
            | `Eof -> failwith "EOF"
            | `Ok action_str -> (
                match String.split action_str ~on:' ' with
                | [ "N" ] -> `No_challenge
                | [ "Y" ] -> `Challenge
                | _ -> failwith "Invalid action")))

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

  (* TODO: needs other player info *)
  val create : Player_id.t -> card_1:Card.t -> card_2:Card.t -> t
end = struct
  type t = {
    player_id : Player_id.t;
    hand : Hand.t;
    mutable events : string list;
  }

  let create player_id ~card_1 ~card_2 =
    { player_id; hand = Hand.Both (card_1, card_2); events = [] }

  let headers =
    Http.Header.of_list
      [ ("Content-Type", "application/json"); ("Authorization", "Bearer TODO") ]

  let choose_action t =
    let open Cohttp_async in
    let state = [%sexp (t.hand : Hand.t)] in
    let post_body =
      let json =
        `Assoc
          [
            ("model", `String "gpt-4o-mini");
            ( "messages",
              `List
                [
                  `Assoc
                    [
                      ("role", `String "developer");
                      ("content", `String "%{Rules.rules}");
                    ];
                  `Assoc
                    [
                      ("role", `String "developer");
                      ( "content",
                        `String
                          [%string
                            "The game is starting. You are player \
                             %{t.player_id#Player_id}. Your cards are \
                             %{state#Sexp}. You have 2 coins. Players 1 and 2 \
                             are your opponents. "] );
                    ];
                  `Assoc
                    [
                      ("role", `String "developer");
                      ( "content",
                        `String
                          "Other players: 1, 2. Hand: Duke, Contessa. Coins: \
                           7. Choose action: Income | ForeignAid | Assasinate \
                           player_id | Coup target_player_id | Tax | Steal \
                           target_player_id | Exchange. Respond with a json \
                           object with the key 'action' and the value being \
                           the action you want to take. If the action requires \
                           a target player, additionally provide the key \
                           'target_player_id' and the value will be the the \
                           player_id of the target player." );
                    ];
                ] );
            ("response_format", `Assoc [ ("type", `String "json_object") ]);
          ]
      in
      Body.of_string (Yojson.Basic.to_string json)
    in
    let%bind response, body =
      Client.post ~headers
        (Uri.of_string "https://api.openai.com/v1/chat/completions")
        ~body:post_body
    in
    let%map body = Body.to_string body in
    let code =
      response |> Cohttp.Response.status |> Cohttp.Code.code_of_status
    in
    print_s [%sexp (code : int)];
    print_endline body;
    let json = Yojson.Basic.from_string body in
    let content =
      Yojson.Basic.Util.member "choices" json
      |> Yojson.Basic.Util.to_list |> List.hd_exn
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.member "content"
      |> function
      | `String content -> Yojson.Basic.from_string content
      | _ -> failwith "Invalid content"
    in
    let action = Yojson.Basic.Util.member "action" content in

    let target_player_id = Yojson.Basic.Util.member "target_player_id" json in
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
    | _ -> failwith "Invalid action"

  let choose_assasination_response _t () = return `Allow
  let choose_foreign_aid_response _t () ~cancelled_reason:_ = return `Allow
  let choose_steal_response _t () = return `Allow
  let choose_cards_to_return _t _card_1 _card_2 _hand = failwith "TODO"
  let reveal_card _t () = return `Card_1

  let offer_challenge _t _acting_player_id _action ~cancelled_reason:_ =
    return `No_challenge

  let notify_of_action_choice t player_id action =
    t.events <-
      t.events
      @ [
          [%string
            "Player %{player_id#Player_id} chose action %{action#Action}"];
        ];
    return ()

  let notify_of_lost_influence _t _player_id _card = failwith "TODO"
  let notify_of_new_card _t _card = failwith "TODO"
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

  let choose_assasination_response t () =
    match t with
    | Cli cli -> Cli_player_io.choose_assasination_response cli ()
    | Llm llm -> Llm_player_io.choose_assasination_response llm ()

  let choose_foreign_aid_response t () ~cancelled_reason =
    match t with
    | Cli cli ->
        Cli_player_io.choose_foreign_aid_response cli () ~cancelled_reason
    | Llm llm ->
        Llm_player_io.choose_foreign_aid_response llm () ~cancelled_reason

  let choose_steal_response t () =
    match t with
    | Cli cli -> Cli_player_io.choose_steal_response cli ()
    | Llm llm -> Llm_player_io.choose_steal_response llm ()

  let choose_cards_to_return t card_1 card_2 _hand =
    match t with
    | Cli cli -> Cli_player_io.choose_cards_to_return cli card_1 card_2 _hand
    | Llm llm -> Llm_player_io.choose_cards_to_return llm card_1 card_2 _hand

  let reveal_card t () =
    match t with
    | Cli cli -> Cli_player_io.reveal_card cli ()
    | Llm llm -> Llm_player_io.reveal_card llm ()

  let offer_challenge t _acting_player_id _action ~cancelled_reason =
    match t with
    | Cli cli ->
        Cli_player_io.offer_challenge cli _acting_player_id _action
          ~cancelled_reason
    | Llm llm ->
        Llm_player_io.offer_challenge llm _acting_player_id _action
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

  let get_player t id =
    List.find_exn t.players ~f:(fun p -> Player_id.equal p.id id)

  let is_valid_action t active_player_id action =
    let player = get_player t active_player_id in
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
    let player = get_player t id in
    match player.hand with
    | Hand.Both (card_1, card_2) ->
        List.mem [ card_1; card_2 ] card ~equal:Card.equal
    | Hand.One { hidden; revealed = _ } -> Card.equal hidden card

  let _valid_actions t active_player_id =
    let player = get_player t active_player_id in
    Action.valid_actions player.coins

  let sorted_deck =
    [ 1; 2; 3; 4 ]
    |> List.concat_map ~f:(fun _i ->
           [ Card.Duke; Assassin; Captain; Ambassador; Contessa ])

  let create_player id card_1 card_2 =
    if Player_id.to_int id = 2 then
      let llm_player_io = Llm_player_io.create id ~card_1 ~card_2 in
      let player_io = Player_io.Llm llm_player_io in
      return
        { Player.id; coins = 2; player_io; hand = Hand.Both (card_1, card_2) }
    else
      let%map cli_player_io = Cli_player_io.cli_player id in
      let player_io = Player_io.Cli cli_player_io in
      { Player.id; coins = 2; player_io; hand = Hand.Both (card_1, card_2) }

  let num_players = 3

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

let _take_foreign_aid game_state =
  (* TODO: check if the action is blocked *)
  Game_state.modify_active_player game_state ~f:(fun active_player ->
      { active_player with coins = active_player.coins + 2 })

(* let init () = Game_state.init () *)
let game_over = Deferred.Result.fail

let lose_influence game_state target_player_id =
  let%bind.Deferred.Result new_game_state, revealed_card =
    let player = Game_state.get_player game_state target_player_id in
    match player.hand with
    | Hand.Both (card_1, card_2) ->
        let%map.Deferred.Result revealed_card =
          Player_io.reveal_card player.player_io () >>| Result.return
        in
        let new_game_state =
          Game_state.modify_player game_state target_player_id ~f:(fun player ->
              let new_hand =
                match revealed_card with
                | `Card_1 -> Hand.One { hidden = card_2; revealed = card_1 }
                | `Card_2 -> Hand.One { hidden = card_1; revealed = card_2 }
              in
              { player with hand = new_hand })
        in
        (* TODO fix *)
        (new_game_state, Card.Contessa)
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
  print_s [%message "LOST INFLUENCE: " (target_player_id : Player_id.t)];
  print_s [%sexp (new_game_state : Game_state.t)];
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
      (Game_state.get_player new_game_state active_player_id).player_io
      replacement_card
  in
  { new_game_state with deck = remaining_deck }

let _required_card_for_action = function
  | `Assassinate -> Card.Assassin
  | `Steal -> Captain
  | `Exchange -> Ambassador
  | `Tax -> Duke
  | `Block_assassination -> Contessa
  | `Block_foreign_aid -> Duke

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
  | `Allow -> Deferred.Result.return (`No_challenge game_state)
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
          `Failed_challenge new_game_state)

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
  | `No_challenge post_challenge_game_state
  | `Failed_challenge post_challenge_game_state -> (
      let target_player =
        Game_state.get_player post_challenge_game_state target_player_id
      in
      match%bind.Deferred.Result
        Player_io.choose_assasination_response target_player.player_io ()
        >>| Result.return
      with
      | `Allow -> lose_influence post_challenge_game_state target_player_id
      | `Block -> (
          match%bind.Deferred.Result
            handle_challenge post_challenge_game_state target_player_id Contessa
          with
          | `Successfully_challenged post_second_challenge_game_state ->
              lose_influence post_second_challenge_game_state target_player_id
          | `No_challenge post_second_challenge_game_state
          | `Failed_challenge post_second_challenge_game_state ->
              Deferred.Result.return post_second_challenge_game_state))

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
      | `No_challenge post_second_challenge_game_state
      | `Failed_challenge post_second_challenge_game_state ->
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
  | `No_challenge post_challenge_game_state
  | `Failed_challenge post_challenge_game_state ->
      Game_state.modify_active_player post_challenge_game_state
        ~f:(fun player -> { player with coins = player.coins + 3 })

let steal game_state target_player_id =
  let steal_two_coins game_state =
    let target_player = Game_state.get_player game_state target_player_id in
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
  | `No_challenge post_challenge_game_state
  | `Failed_challenge post_challenge_game_state -> (
      let target_player =
        Game_state.get_player post_challenge_game_state target_player_id
      in
      match%bind.Deferred.Result
        Player_io.choose_steal_response target_player.player_io ()
        >>| Result.return
      with
      | `Allow ->
          steal_two_coins post_challenge_game_state |> Deferred.Result.return
      | `Block blocking_card -> (
          match%bind.Deferred.Result
            handle_challenge post_challenge_game_state target_player_id
              (match blocking_card with
              | `Ambassador -> Ambassador
              | `Captain -> Captain)
          with
          | `Successfully_challenged post_second_challenge_game_state ->
              Deferred.Result.return post_second_challenge_game_state
          | `No_challenge post_second_challenge_game_state
          | `Failed_challenge post_second_challenge_game_state ->
              steal_two_coins post_second_challenge_game_state
              |> Deferred.Result.return))

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
            print_s
              [%message
                (remaining_to_remove_acc
                  : [ `Both of Card.t * Card.t | `One of Card.t | `None ])
                  (hand_acc : Card.t list)
                  (card : Card.t)];
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
            print_s [%sexp (new_cards : Card.t list)];
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
  print_s [%sexp (action : Action.t)];
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

let make_http_request () =
  let open Cohttp_async in
  let headers =
    Http.Header.of_list
      [ ("Content-Type", "application/json"); ("Authorization", "Bearer BLAH") ]
  in
  let post_body =
    let json =
      `Assoc
        [
          ("model", `String "gpt-4o-mini");
          ( "messages",
            `List
              [
                `Assoc
                  [
                    ("role", `String "developer");
                    ("content", `String "%{Rules.rules}");
                  ];
                `Assoc
                  [
                    ("role", `String "developer");
                    ( "content",
                      `String
                        "Other players: 1, 2.Hand: Duke, Contessa. Coins: 7. \
                         Choose action: Income | ForeignAid | Assasinate \
                         player_id | Coup target_player_id | Tax | Steal \
                         target_player_id | Exchange. Respond with a json \
                         object with the key 'action' and the value being the \
                         action you want to take. If the action requires a \
                         target player, additionally provide the key \
                         'target_player_id' and the value will be the the \
                         player_id of the target player." );
                  ];
              ] );
          ("response_format", `Assoc [ ("type", `String "json_object") ]);
        ]
    in
    Body.of_string (Yojson.Basic.to_string json)
  in
  let%bind response, body =
    Client.post ~headers
      (Uri.of_string "https://api.openai.com/v1/chat/completions")
      ~body:post_body
  in
  let%map body = Body.to_string body in
  let code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  print_s [%sexp (code : int)];
  print_endline body
