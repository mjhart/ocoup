open! Core

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

module Hand = struct
  type t =
    | Both of Card.t * Card.t
    | One of { hidden : Card.t; revealed : Card.t }
  [@@deriving sexp]
end

(* A player has an identifier, a name, a coin count, and two sets of cards:
   - 'hand': face-down cards that represent their current influence.
   - 'revealed': face-up cards that have been lost. *)
type player = { id : Player_id.t; coins : int; hand : Hand.t } [@@deriving sexp]

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
end

module Game_state = struct
  type t = {
    players : player list;  (** the first player in the list is active *)
    deck : Card.t list;
  }
  [@@deriving sexp]

  let deck t = t.deck

  let modify_player t id ~f =
    let new_players =
      List.map t.players ~f:(fun player ->
          if Player_id.equal player.id id then f player else player)
    in
    { t with players = new_players }

  let get_active_player_id t = List.hd_exn t.players |> fun player -> player.id

  let modify_active_player t ~f =
    let active_player = List.hd_exn t.players in

    { t with players = f active_player :: List.tl_exn t.players }

  let end_turn t =
    { t with players = List.tl_exn t.players @ [ List.hd_exn t.players ] }

  let get_player t id =
    List.find_exn t.players ~f:(fun p -> Player_id.equal p.id id)

  let has_card t id card =
    let player = get_player t id in
    match player.hand with
    | Hand.Both (card_1, card_2) ->
        List.mem [ card_1; card_2 ] card ~equal:Card.equal
    | Hand.One { hidden; revealed = _ } -> Card.equal hidden card

  let sorted_deck =
    [ 1; 2; 3; 4 ]
    |> List.concat_map ~f:(fun _i ->
           [ Card.Duke; Assassin; Captain; Ambassador; Contessa ])

  let create_player id card_1 card_2 =
    { id; coins = 2; hand = Hand.Both (card_1, card_2) }

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
    let player_cards, remaining_cards = List.split_n paired_deck 4 in
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

open Async

module Player : sig
  val choose_action : Game_state.t -> Player_id.t -> Action.t Deferred.t
  val choose_assasination_response : unit -> [ `Allow | `Block ] Deferred.t
  val reveal_card : unit -> [ `Card_1 | `Card_2 ] Deferred.t
end = struct
  (* TODO: implement *)
  let choose_action _game_state active_player_id =
    (* TODO: implement for real. Make sure they have enough coins *)
    (* return
      (List.random_element_exn
         [
           Action.Income;
           Action.Assassinate
             ( Game_state.get_players game_state
             |> List.find_exn ~f:(fun player ->
                    not (Player_id.equal player.id active_player_id))
             |> fun player -> player.id );
         ]) *)
    print_endline
      (sprintf "You are player %d" (Player_id.to_int active_player_id));
    print_endline "Choose action (I/FA/C/T/S/E)";
    match%bind Reader.read_line (Lazy.force Reader.stdin) with
    | `Eof -> failwith "EOF"
    | `Ok action_str -> (
        match String.split action_str ~on:' ' with
        | [ "I" ] -> return Action.Income
        | [ "A"; n ] ->
            return (Action.Assassinate (Player_id.of_int (Int.of_string n)))
        | _ -> failwith "Invalid action")

  let choose_assasination_response () =
    print_endline "Assassinate? (A/B)";
    match%bind Reader.read_line (Lazy.force Reader.stdin) with
    | `Eof -> failwith "EOF"
    | `Ok action_str -> (
        match String.split action_str ~on:' ' with
        | [ "A" ] -> return `Allow
        | [ "B" ] -> return `Block
        | _ -> failwith "Invalid action")

  let reveal_card () = return `Card_1
end

let game_over = Deferred.Result.fail

let lose_influence game_state target_player_id =
  let player = Game_state.get_player game_state target_player_id in
  match player.hand with
  | Hand.Both (card_1, card_2) ->
      let%map.Deferred.Result revealed_card =
        Player.reveal_card () >>| Result.return
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
      new_game_state
  | Hand.One _ -> (
      let remaining_players =
        List.filter game_state.players ~f:(fun player ->
            not (Player_id.equal player.id target_player_id))
      in
      match List.length remaining_players with
      | 1 -> game_over { game_state with players = remaining_players }
      | _ ->
          Deferred.Result.return { game_state with players = remaining_players }
      )

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

(* module Challenge_result = struct *)
let handle_challenge game_state acting_player_id action =
  let offer_challenge _acting_player_id =
    (* TODO: implement *)
    print_endline "Offer challenge? (N/C <player_id>)";
    match%bind Reader.read_line (Lazy.force Reader.stdin) with
    | `Eof -> failwith "EOF"
    | `Ok action_str -> (
        match String.split action_str ~on:' ' with
        | [ "N" ] -> return `No_challenge
        | [ "C"; n ] -> return (`Challenge (Player_id.of_int (Int.of_string n)))
        | _ -> failwith "Invalid action")
  in
  match%bind offer_challenge acting_player_id with
  | `No_challenge -> Deferred.Result.return (`No_challenge game_state)
  | `Challenge challenger_player_id -> (
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
  match%bind.Deferred.Result
    handle_challenge game_state active_player_id `Assassinate
  with
  | `Successfully_challenged post_challenge_game_state ->
      Deferred.Result.return post_challenge_game_state
  | `No_challenge post_challenge_game_state
  | `Failed_challenge post_challenge_game_state -> (
      match%bind.Deferred.Result
        Player.choose_assasination_response () >>| Result.return
      with
      | `Allow -> lose_influence game_state target_player_id
      | `Block -> (
          match%bind.Deferred.Result
            handle_challenge post_challenge_game_state target_player_id
              `Block_assassination
          with
          | `Successfully_challenged post_challenge_game_state ->
              Deferred.Result.return post_challenge_game_state
          | `No_challenge post_challenge_game_state
          | `Failed_challenge post_challenge_game_state ->
              lose_influence post_challenge_game_state target_player_id))

let take_turn_result game_state =
  let active_player_id = Game_state.get_active_player_id game_state in
  let%bind.Deferred.Result () =
    after Time_float.Span.second >>| Result.return
  in
  let%bind.Deferred.Result action =
    Player.choose_action game_state active_player_id >>| Result.return
  in
  print_s [%sexp (action : Action.t)];
  match (action : Action.t) with
  | Income -> take_income game_state |> Deferred.Result.return
  | Assassinate target_player_id ->
      assassinate game_state active_player_id target_player_id
  | ForeignAid | Coup _ | Tax | Steal _ | Exchange ->
      Deferred.Result.return game_state

let take_turn game_state =
  print_s [%sexp (game_state : Game_state.t)];
  match%bind take_turn_result game_state with
  | Ok game_state' -> return (`Repeat (Game_state.end_turn game_state'))
  | Error final_game_state -> return (`Finished final_game_state)

let run_game () =
  let%map final_game_state =
    Deferred.repeat_until_finished (Game_state.init ()) take_turn
  in
  print_s [%sexp (final_game_state : Game_state.t)]
