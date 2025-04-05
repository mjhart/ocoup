open! Core
open! Async

val run_game : string list -> unit Deferred.t
val run_server : port:int -> unit Deferred.t
