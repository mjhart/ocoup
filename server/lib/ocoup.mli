open! Core
open! Async

val run_game : string list -> unit Deferred.t
val run_server : port:int -> unit Deferred.t

module For_testing : sig
  module Game : module type of Game
  module Types : module type of Types
  module Player_ios : module type of Player_ios
  module Tournament : module type of Tournament
end
