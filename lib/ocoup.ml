open! Core
open! Async

(* TODO:
   - implement other actions
   - timeout for player action
   - implement players
   - tell players about stuff
 *)

(* The characters (and cards) available in the game *)
module Card = struct
  type t = Duke | Assassin | Captain | Ambassador | Contessa
  [@@deriving equal, sexp]
end

module Player_id : sig
  type t [@@deriving sexp, equal]

  val of_int : int -> t
  val to_int : t -> int
end = struct
  type t = int [@@deriving sexp, equal]

  let of_int = Fn.id
  let to_int = Fn.id
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

module Player_io : sig
  type t

  val choose_action : t -> Action.t Deferred.t
  val choose_assasination_response : t -> unit -> [ `Allow | `Block ] Deferred.t

  val choose_foreign_aid_response :
    t ->
    unit ->
    cancelled_reason:Cancelled_reason.t Deferred.t ->
    [ `Allow | `Block ] Deferred.t

  val reveal_card : t -> unit -> [ `Card_1 | `Card_2 ] Deferred.t

  val offer_challenge :
    t ->
    Player_id.t ->
    [ `Assassinate
    | `Block_assassination
    | `Block_foreign_aid
    | `Exchange
    | `Steal
    | `Tax ] ->
    cancelled_reason:Cancelled_reason.t Deferred.t ->
    [ `No_challenge | `Challenge ] Deferred.t

  val cli_player : Player_id.t -> t
  val notify_of_action_choice : t -> Player_id.t -> Action.t -> unit Deferred.t
  val notify_of_lost_influence : t -> Player_id.t -> Card.t -> unit Deferred.t
  (* 
          what to players have to learn about:
          - their initial cards
          - getting cards
          - player choices
            - challenges
            - blocking
          - losing influence
          - game end
          *)
end = struct
  let notify_of_lost_influence _t player_id card =
    print_s
      [%message
        "Player lost influence" (player_id : Player_id.t) (card : Card.t)]
    |> return

  let stdin_throttle =
    Lazy.from_fun (fun () ->
        Throttle.Sequencer.create ~continue_on_error:false ())

  type t = { player_id : Player_id.t }

  let cli_player player_id = { player_id }

  let with_stdin t ~f =
    Throttle.enqueue (Lazy.force stdin_throttle) (fun () ->
        print_endline
          (sprintf "You are player %d" (Player_id.to_int t.player_id));
        f (Lazy.force Reader.stdin))

  (* TODO: implement *)
  let choose_action t =
    (* TODO: implement for real. Make sure they have enough coins *)
    with_stdin t ~f:(fun stdin ->
        print_endline "Choose action (I/FA/C/T/S/E)";
        match%bind Reader.read_line stdin with
        | `Eof -> failwith "EOF"
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "I" ] -> return Action.Income
            | [ "FA" ] -> return Action.ForeignAid
            | [ "A"; n ] ->
                return (Action.Assassinate (Player_id.of_int (Int.of_string n)))
            | _ -> failwith "Invalid action"))

  let choose_assasination_response t () =
    with_stdin t ~f:(fun stdin ->
        print_endline "Block assassination? (Y/N)";
        match%map Reader.read_line stdin with
        | `Eof -> failwith "EOF"
        | `Ok action_str -> (
            match String.split action_str ~on:' ' with
            | [ "N" ] -> `Allow
            | [ "Y" ] -> `Block
            | _ -> failwith "Invalid action"))

  let choose_foreign_aid_response t () ~cancelled_reason =
    with_stdin t ~f:(fun stdin ->
        match Deferred.is_determined cancelled_reason with
        | true -> return `Allow
        | false -> (
            print_endline "Block foreign aid? (Y/N)";
            match%map Reader.read_line stdin with
            | `Eof -> failwith "EOF"
            | `Ok action_str -> (
                match String.split action_str ~on:' ' with
                | [ "Y" ] -> `Block
                | [ "N" ] -> `Allow
                | _ -> failwith "Invalid action")))

  let reveal_card _t () = return `Card_1
  (* TODO: implement *)

  let offer_challenge t acting_player_id action ~cancelled_reason =
    with_stdin t ~f:(fun stdin ->
        match Deferred.is_determined cancelled_reason with
        | true -> return `No_challenge
        | false -> (
            print_s
              [%message
                "Challenge action? (Y/N)"
                  (action
                    : [< `Assassinate
                      | `Block_assassination
                      | `Block_foreign_aid
                      | `Exchange
                      | `Steal
                      | `Tax ])
                  (acting_player_id : Player_id.t)];
            match%map Reader.read_line stdin with
            | `Eof -> failwith "EOF"
            | `Ok action_str -> (
                match String.split action_str ~on:' ' with
                | [ "N" ] -> `No_challenge
                | [ "Y" ] -> `Challenge
                | _ -> failwith "Invalid action")))

  let notify_of_action_choice _t player_id action =
    print_s
      [%message
        "Player chose action" (player_id : Player_id.t) (action : Action.t)]
    |> return
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
    {
      Player.id;
      coins = 2;
      player_io = Player_io.cli_player id;
      hand = Hand.Both (card_1, card_2);
    }

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
    let players =
      List.mapi player_cards ~f:(fun id (card_1, card_2) ->
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

let init () = Game_state.init ()
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
  (* TODO make sure to emit an event with new card info *)
  (* let new_card = List.random_element_exn game_state.deck in *)
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
  { new_game_state with deck = remaining_deck }

let required_card_for_action = function
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

let handle_challenge game_state acting_player_id action =
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
      let card_to_replace = required_card_for_action action in
      match Game_state.has_card game_state acting_player_id card_to_replace with
      | false ->
          let%map.Deferred.Result new_game_state =
            lose_influence game_state acting_player_id
          in
          `Successfully_challenged new_game_state
      | true ->
          let game_state_after_new_card =
            randomly_get_new_card game_state acting_player_id card_to_replace
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
    handle_challenge game_state active_player_id `Assassinate
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
            handle_challenge post_challenge_game_state target_player_id
              `Block_assassination
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
        handle_challenge game_state blocking_player_id `Block_foreign_aid
      with
      | `Successfully_challenged post_second_challenge_game_state ->
          Deferred.Result.return
            (take_two_coins post_second_challenge_game_state)
      | `No_challenge post_second_challenge_game_state
      | `Failed_challenge post_second_challenge_game_state ->
          Deferred.Result.return post_second_challenge_game_state)
  | `Allow -> Deferred.Result.return (take_two_coins game_state)

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
  | Coup _ | Tax | Steal _ | Exchange -> Deferred.Result.return game_state

let take_turn game_state =
  print_newline ();
  print_s [%sexp (game_state : Game_state.t)];
  match%bind take_turn_result game_state with
  | Ok game_state' -> return (`Repeat (Game_state.end_turn game_state'))
  | Error final_game_state -> return (`Finished final_game_state)

let run_game () =
  let%map final_game_state =
    Deferred.repeat_until_finished (Game_state.init ()) take_turn
  in
  print_s [%sexp (final_game_state : Game_state.t)]
