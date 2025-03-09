open! Core
open! Async

module Game_state : sig
  type t [@@deriving sexp_of]
end

val run_game : ?game_state:Game_state.t -> unit -> unit Deferred.t
val run_server : unit -> unit Deferred.t
(* val create_with_pipe_player : unit *)
