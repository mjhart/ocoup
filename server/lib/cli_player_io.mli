open! Core
open! Async
open Types
include Player_io_S

val cli_player : Player_id.t -> t Deferred.t
