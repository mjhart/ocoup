open! Core
open Types

(** Protocol module for JSON serialization/deserialization of game messages.
    This module defines the wire format for communication between server and
    clients. *)

(** JSON conversion for Card type *)
module Card : sig
  include module type of Card

  val yojson_of_t : t -> Yojson.Safe.t
end

(** JSON conversion for Action type *)
module Action : sig
  include module type of Action

  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

(** Allow or Block response for certain actions *)
module Allow_or_block : sig
  type t = [ `Allow | `Block ]

  val t_of_yojson : Yojson.Safe.t -> t
end

(** JSON conversion for Challengable type *)
module Challengable : sig
  include module type of Challengable

  val yojson_of_t : t -> Yojson.Safe.t
end

(** JSON conversion for Visible_game_state *)
module Visible_game_state : sig
  include module type of Visible_game_state

  val yojson_of_t : t -> Yojson.Safe.t
end

(** Game start notification sent to player *)
module Game_start : sig
  val query :
    player_id:Player_id.t ->
    visible_game_state:Visible_game_state.t ->
    Yojson.Safe.t
end

(** Choose action query/response *)
module Choose_action : sig
  val query : visible_game_state:Visible_game_state.t -> Yojson.Safe.t
  val response_of_yojson : Yojson.Safe.t -> Action.t
end

(** Choose assassination response query/response *)
module Choose_assasination_response : sig
  val create_query :
    Player_id.t -> visible_game_state:Visible_game_state.t -> Yojson.Safe.t

  val response_of_yojson : Yojson.Safe.t -> Allow_or_block.t
end

(** Choose foreign aid response query/response *)
module Choose_foreign_aid_response : sig
  val create_query :
    visible_game_state:Visible_game_state.t -> unit -> Yojson.Safe.t

  val response_of_yojson : Yojson.Safe.t -> Allow_or_block.t
end

(** Choose steal response query/response *)
module Choose_steal_response : sig
  type response = [ `Allow | `Block of [ `Ambassador | `Captain ] ]

  val create_query :
    Player_id.t -> visible_game_state:Visible_game_state.t -> Yojson.Safe.t

  val response_of_yojson : Yojson.Safe.t -> response
end

(** Choose cards to return (for Exchange) query/response *)
module Choose_cards_to_return : sig
  val create_query :
    Card.t list -> visible_game_state:Visible_game_state.t -> Yojson.Safe.t

  val response_of_yojson : Yojson.Safe.t -> Card.t * Card.t
end

(** Reveal card query/response (when losing influence) *)
module Reveal_card : sig
  val create_query :
    Card.t -> Card.t -> visible_game_state:Visible_game_state.t -> Yojson.Safe.t

  val response_of_yojson : Yojson.Safe.t -> [ `Card_1 | `Card_2 ]
end

(** Offer challenge query/response *)
module Offer_challenge : sig
  val create_query :
    Player_id.t ->
    Challengable.t ->
    visible_game_state:Visible_game_state.t ->
    Yojson.Safe.t

  val response_of_yojson : Yojson.Safe.t -> [ `Challenge | `No_challenge ]
end

(** Notification: a player chose an action *)
module Action_choice_notification : sig
  val create_query : Player_id.t -> Action.t -> Yojson.Safe.t
end

(** Notification: a player lost influence (revealed a card) *)
module Lost_influence_notification : sig
  val create_query : Player_id.t -> Card.t -> Yojson.Safe.t
end

(** Notification: player received a new card *)
module New_card_notification : sig
  val create_query : Card.t -> Yojson.Safe.t
end

(** Notification: a challenge occurred *)
module Challenge_notification : sig
  val create_query : Player_id.t -> bool -> Yojson.Safe.t
end

(** Notification: another player has responded (cancels pending response) *)
module Player_responded_notification : sig
  val create_query : Player_id.t -> Yojson.Safe.t
end

(** Server API: Create game request parsing *)
module Create_game_request : sig
  type t = { bot_players : string list }

  val of_yojson : Yojson.Safe.t -> t
end

(** Server API: Create game response *)
module Create_game_response : sig
  val create : game_id:string -> num_bot_players:int -> Yojson.Safe.t
end

(** Server API: Create tournament request parsing *)
module Create_tournament_request : sig
  type t = { max_players : int; bot_players : string list }

  val of_yojson : Yojson.Safe.t -> t
end

(** Server API: Create tournament response *)
module Create_tournament_response : sig
  val create : tournament_id:string -> num_bot_players:int -> Yojson.Safe.t
end

(** Server API: Tournament registration response *)
module Tournament_registration_response : sig
  val registered : player_id:int -> Yojson.Safe.t
end

(** Server API: Tournament results for start tournament response *)
module Tournament_results : sig
  (** A single game result within a round *)
  type game_result =
    | Completed of { winners : Player_id.t list; eliminated : Player_id.t list }
    | Error of string

  val game_result_to_yojson : game_idx:int -> game_result -> Yojson.Safe.t
  (** Convert a game result to JSON *)

  val results_to_yojson : game_result list list -> Yojson.Safe.t
  (** Convert tournament results to JSON *)

  val scores_to_yojson : int Player_id.Map.t -> Yojson.Safe.t
  (** Convert scores map to JSON *)

  val create_response :
    scores:int Player_id.Map.t -> results:game_result list list -> Yojson.Safe.t
  (** Create full start tournament response *)
end

(** Server API: Error response *)
module Error_response : sig
  val create : string -> Yojson.Safe.t
end
