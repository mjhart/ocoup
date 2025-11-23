open! Core
open! Async
open Types

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
  let implementation = Gemini_player_io.create id ~model in
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
