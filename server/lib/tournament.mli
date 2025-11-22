open! Core
open Async

type t

val create_rounds : 'a list -> 'a list list list
val score_results : (Game.Game_state.t, exn) result list list -> int
val create : max_players:int -> t
val register : t -> Player_ios.Player_io.t -> t Or_error.t
val start : t -> Game.Game_state.t Or_error.t list list Deferred.t
