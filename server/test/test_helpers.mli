open! Core
open Async
open Ocoup.For_testing.Types
module Game = Ocoup.For_testing.Game
module Game_state = Game.Game_state

module Response : sig
  type t =
    | Choose_action of Action.t
    | Choose_assasination_response of [ `Allow | `Block ]
    | Choose_foreign_aid_response of [ `Allow | `Block ]
    | Choose_steal_response of [ `Allow | `Block of [ `Ambassador | `Captain ] ]
    | Choose_cards_to_return
    | Reveal_card of [ `Card_1 | `Card_2 ]
    | Offer_challenge of [ `Challenge | `No_challenge ]
end

val run_test :
  ?print_notifications:unit ->
  starting_cards:(Card.t * Card.t) list ->
  (int * Response.t) list ->
  unit Deferred.t
