open! Core
open! Async
open Types
include Player_io_S

val create : (module Player_io_S with type t = 'a) -> 'a -> t

(* Some useful player_io impls *)

val llm : Player_id.t -> model:string -> t Deferred.t
val cli : Player_id.t -> t Deferred.t
val gemini : Player_id.t -> model:string -> t
