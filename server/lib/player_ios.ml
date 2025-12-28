open! Core
open! Async
open Types

type t = Packed : (module Player_io_S with type t = 'a) * 'a -> t

let create (type a) (module M : Player_io_S with type t = a) implementation =
  Packed ((module M), implementation)

let timeout_span = Time_float.Span.of_sec 60.0

let with_timeout_default ~default deferred =
  match%map with_timeout timeout_span deferred with
  | `Result result -> result
  | `Timeout -> default

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
  let implementation = Gemini_player_io.create id ~model in
  Packed ((module M), implementation)

let cli id =
  let (module M) =
    (module Cli_player_io : Player_io_S with type t = Cli_player_io.t)
  in
  let%map implementation = Cli_player_io.cli_player id in
  Packed ((module M), implementation)

let choose_action (Packed ((module M), implementation)) ~visible_game_state =
  let default =
    let coins = visible_game_state.Visible_game_state.coins in
    let other_players = visible_game_state.Visible_game_state.other_players in
    if coins >= 7 && List.length other_players > 0 then
      let target_player_id =
        (List.hd_exn other_players).Visible_game_state.Other_player.player_id
      in
      `Coup target_player_id
    else `Income
  in
  with_timeout_default ~default
    (M.choose_action implementation ~visible_game_state)

let choose_assasination_response (Packed ((module M), implementation))
    ~visible_game_state ~asassinating_player_id =
  with_timeout_default ~default:`Allow
    (M.choose_assasination_response implementation ~visible_game_state
       ~asassinating_player_id)

let choose_foreign_aid_response (Packed ((module M), implementation))
    ~visible_game_state () ~cancelled_reason =
  with_timeout_default ~default:`Allow
    (M.choose_foreign_aid_response implementation ~visible_game_state ()
       ~cancelled_reason)

let choose_steal_response (Packed ((module M), implementation))
    ~visible_game_state ~stealing_player_id =
  with_timeout_default ~default:`Allow
    (M.choose_steal_response implementation ~visible_game_state
       ~stealing_player_id)

let choose_cards_to_return (Packed ((module M), implementation))
    ~visible_game_state card1 card2 hand =
  let default = (card1, card2) in
  with_timeout_default ~default
    (M.choose_cards_to_return implementation ~visible_game_state card1 card2
       hand)

let reveal_card (Packed ((module M), implementation)) ~visible_game_state
    ~card_1 ~card_2 =
  with_timeout_default ~default:`Card_1
    (M.reveal_card implementation ~visible_game_state ~card_1 ~card_2)

let offer_challenge (Packed ((module M), implementation)) ~visible_game_state
    player_id challengable ~cancelled_reason =
  with_timeout_default ~default:`No_challenge
    (M.offer_challenge implementation ~visible_game_state player_id challengable
       ~cancelled_reason)

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
