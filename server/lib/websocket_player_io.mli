open! Core
open! Async
open Types
include Player_io_S

val create :
  player_id:Player_id.t ->
  reader:Yojson.Safe.t Pipe.Reader.t ->
  writer:string Pipe.Writer.t ->
  t
