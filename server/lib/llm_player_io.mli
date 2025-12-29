open! Core
open! Async
open Types
include Player_io_S

val gpt_5_mini : string
val gpt_5_nano : string
val o3_mini : string
val create : Player_id.t -> model:string -> t Deferred.t
