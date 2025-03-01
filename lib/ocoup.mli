open! Core

module Game_state : sig
  type t [@@deriving sexp]
end

val init : unit -> Game_state.t
val run_game : unit -> unit Async.Deferred.t
