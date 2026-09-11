open! Core
open! Async
open Types
include Player_io_S

val claude_opus_5 : string
val claude_sonnet_5 : string
val claude_haiku_4_5 : string
val create : Player_id.t -> model:string -> t Deferred.t
