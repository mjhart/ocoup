open! Core
open! Async
open Types
include Player_io_S

val gemini_2_5_pro_exp_03_25 : string
val create : Player_id.t -> model:string -> t
