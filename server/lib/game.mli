open! Core
open Async
open Types

module Player : sig
  type t = {
    id : Player_id.t;
    player_io : Player_ios.t;
    coins : int;
    hand : Hand.t;
  }
end

module Game_state : sig
  type t = {
    players : Player.t list;
    deck : Card.t list;
    eliminated_players : Player.t list;
        (** Eliminated players with most recently eliminated at head, earliest
            at tail *)
  }
  [@@deriving sexp_of]

  val players : t -> Player.t list
  val to_visible_game_state : t -> Player_id.t -> Visible_game_state.t

  val init :
    (Player_id.t -> Player_ios.t Deferred.t) list -> t Deferred.Or_error.t

  val init' :
    (Player_id.t * (unit -> Player_ios.t Deferred.t)) list ->
    t Deferred.Or_error.t
  (** Allow client to provide [Playe_id.t]s *)
end

val take_turn :
  Game_state.t ->
  [ `Finished of Game_state.t | `Repeat of Game_state.t ] Deferred.t

val run_game : game_state:Game_state.t -> Game_state.t Deferred.t
