open! Core
open! Async

module Game_state : sig
  type t [@@deriving sexp_of]
end

(* val init : unit -> Game_state.t D *)
val run_game : unit -> unit Deferred.t
val make_http_request : unit -> unit Deferred.t
