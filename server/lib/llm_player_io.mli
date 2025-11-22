open! Core
open! Async
open Types
include Player_io_S

val gpt_4o : string
val gpt_4o_mini : string
val o3_mini : string
val create : Player_id.t -> model:string -> t Deferred.t
