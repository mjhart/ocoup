open! Core
open Async
open Ocoup.For_testing.Types
module Game = Ocoup.For_testing.Game
module Game_state = Game.Game_state

module Response = struct
  type t =
    | Choose_action of Action.t
    | Choose_assasination_response of [ `Allow | `Block ]
    | Choose_foreign_aid_response of [ `Allow | `Block ]
    | Choose_steal_response of [ `Allow | `Block of [ `Ambassador | `Captain ] ]
    | Choose_cards_to_return
    | Reveal_card of [ `Card_1 | `Card_2 ]
    | Offer_challenge of [ `Challenge | `No_challenge ]

  let to_string = function
    | Choose_action action -> [%string "Choose_action: %{action#Action}"]
    | Choose_assasination_response _ -> "Choose_assasination_response"
    | Choose_foreign_aid_response _ -> "Choose_foreign_aid_response"
    | Choose_steal_response _ -> "Choose_steal_response"
    | Choose_cards_to_return -> "Choose_cards_to_return"
    | Reveal_card _ -> "Reveal_card"
    | Offer_challenge _ -> "Offer_challenge"
end

module Test_player_io : sig
  include Player_io_S

  val create : int -> (int * Response.t) Queue.t -> bool -> t
end = struct
  type t = {
    player_index : int;
    responses : (int * Response.t) Queue.t;
    print_notifications : bool;
  }

  let create player_index responses print_notifications =
    { player_index; responses; print_notifications }

  let dequeue_exn t =
    let i, choice =
      match Queue.dequeue t.responses with
      | Some x -> x
      | None -> raise_s [%message "No more actions" (t.player_index : int)]
    in
    if i <> t.player_index then
      raise_s
        [%message "Player going out of turn" (i : int) (t.player_index : int)];
    print_endline [%string "Player %{i#Int}: %{choice#Response}"];
    choice

  let choose_action t ~visible_game_state:_ =
    let response = dequeue_exn t in
    match response with
    | Response.Choose_action action -> return action
    | _ -> failwith "Expected Choose_action response"

  let choose_assasination_response t ~visible_game_state:_
      ~asassinating_player_id:_ =
    let response = dequeue_exn t in
    match response with
    | Response.Choose_assasination_response response -> return response
    | _ -> failwith "Expected Choose_assasination_response response"

  let choose_foreign_aid_response t ~visible_game_state:_ () ~cancelled_reason:_
      =
    let response = dequeue_exn t in
    match response with
    | Response.Choose_foreign_aid_response response -> return response
    | _ -> failwith "Expected Choose_foreign_aid_response response"

  let choose_steal_response t ~visible_game_state:_ ~stealing_player_id:_ =
    let response = dequeue_exn t in
    match response with
    | Response.Choose_steal_response response -> return response
    | _ -> failwith "Expected Choose_steal_response response"

  let choose_cards_to_return t ~visible_game_state:_ card_1 card_2 _hand =
    let response = dequeue_exn t in
    match response with
    | Response.Choose_cards_to_return -> return (card_1, card_2)
    | _ -> failwith "Expected Choose_cards_to_return response"

  let reveal_card t ~visible_game_state:_ ~card_1:_ ~card_2:_ =
    let response = dequeue_exn t in
    match response with
    | Response.Reveal_card card -> return card
    | _ -> failwith "Expected Reveal_card response"

  let offer_challenge t ~visible_game_state:_ _acting_player_id _challengable
      ~cancelled_reason:_ =
    let response = dequeue_exn t in
    match response with
    | Response.Offer_challenge challenge -> return challenge
    | _ -> failwith "Expected Offer_challenge response"

  let notify_of_game_start t ~visible_game_state:_ =
    if t.print_notifications then
      print_endline [%string "Player %{t.player_index#Int}: Game started"];
    return ()

  let notify_of_action_choice t player_id action =
    if t.print_notifications then
      print_endline
        [%string
          "Player %{t.player_index#Int}: Player %{player_id#Player_id} chose \
           %{action#Action}"];
    return ()

  let notify_of_lost_influence t player_id card =
    if t.print_notifications then
      print_endline
        [%string
          "Player %{t.player_index#Int}: Player %{player_id#Player_id} lost \
           influence (%{card#Card})"];
    return ()

  let notify_of_new_card t card =
    if t.print_notifications then
      print_endline
        [%string
          "Player %{t.player_index#Int}: Received new card (%{card#Card})"];
    return ()

  let notify_of_challenge t ~challenging_player_id ~has_required_card =
    if t.print_notifications then
      print_endline
        [%string
          "Player %{t.player_index#Int}: Player \
           %{challenging_player_id#Player_id} challenged (has_required_card: \
           %{has_required_card#Bool})"];
    return ()
end

let run_test ?print_notifications ~starting_cards moves_list =
  let print_notifications = Option.is_some print_notifications in
  let moves = Queue.of_list moves_list in
  let game_state =
    let open Game in
    {
      Game_state.players =
        List.mapi starting_cards ~f:(fun i (card_1, card_2) ->
            {
              Player.id = Player_id.of_int i;
              player_io =
                Ocoup.For_testing.Player_ios.Player_io.create
                  (module Test_player_io)
                  (Test_player_io.create i moves print_notifications);
              coins = 2;
              hand = Hand.Both (card_1, card_2);
            });
      deck =
        (let card_counts =
           Map.of_key_set
             (Set.of_list
                (module Card)
                [ Card.Duke; Assassin; Captain; Ambassador; Contessa ])
             ~f:(Fn.const 3)
         in
         let new_card_counts =
           List.fold
             (List.concat_map starting_cards ~f:(fun (card_1, card_2) ->
                  [ card_1; card_2 ]))
             ~init:card_counts
             ~f:(fun acc card ->
               Map.update acc card ~f:(function
                 | Some count -> count - 1
                 | None -> 0))
         in
         Map.to_alist new_card_counts
         |> List.concat_map ~f:(fun (card, count) ->
                List.init count ~f:(Fn.const card)));
    }
  in
  print_endline "Initial game state:";
  print_s [%sexp (game_state : Game_state.t)];
  print_newline ();
  let%map result =
    Deferred.repeat_until_finished game_state (fun game_state ->
        match Queue.is_empty moves with
        | true -> return (`Finished game_state)
        | false -> (
            match%map Game.take_turn game_state with
            | `Finished game_state -> `Finished game_state
            | `Repeat game_state -> `Repeat game_state))
  in
  print_newline ();
  print_endline "Game state after all moves:";
  print_s [%sexp (result : Game_state.t)]

module Default_action_player_io : Player_io_S with type t = unit = struct
  type t = unit

  let choose_action _t ~visible_game_state =
    let Visible_game_state.{ coins; other_players; _ } = visible_game_state in
    let Visible_game_state.Other_player.{ player_id = target; _ } =
      List.hd_exn other_players
    in
    return (if coins >= 7 then `Coup target else `Income)

  let choose_assasination_response _t ~visible_game_state:_
      ~asassinating_player_id:_ =
    return `Allow

  let choose_foreign_aid_response _t ~visible_game_state:_ ()
      ~cancelled_reason:_ =
    return `Allow

  let choose_steal_response _t ~visible_game_state:_ ~stealing_player_id:_ =
    return `Allow

  let choose_cards_to_return _t ~visible_game_state:_ card_1 card_2 _hand =
    return (card_1, card_2)

  let reveal_card _t ~visible_game_state:_ ~card_1:_ ~card_2:_ = return `Card_1

  let offer_challenge _t ~visible_game_state:_ _acting_player_id _challengable
      ~cancelled_reason:_ =
    return `No_challenge

  let notify_of_game_start _t ~visible_game_state:_ = return ()
  let notify_of_action_choice _t _player_id _action = return ()
  let notify_of_lost_influence _t _player_id _card = return ()
  let notify_of_new_card _t _card = return ()

  let notify_of_challenge _t ~challenging_player_id:_ ~has_required_card:_ =
    return ()
end
