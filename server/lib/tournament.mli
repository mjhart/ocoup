open! Core
open Async

type closed
type registration
type in_progress

type 'a t =
  | Closed : closed t
  | Registration : {
      players : (Types.Player_id.t * Player_ios.Player_io.t) list;
      max_players : int;
    }
      -> registration t
  | In_progress : in_progress t

val create_rounds : 'a list -> 'a list list list
val score_results : (Game.Game_state.t, exn) result list list -> int
val create : max_players:int -> registration t

val _register :
  registration t -> Player_ios.Player_io.t -> registration t Or_error.t

val _start :
  registration t -> (Game.Game_state.t, exn) result list list Deferred.t

val _finish : in_progress t -> closed t
