open! Core
open! Async

(* The characters (and cards) available in the game *)
module Card : sig
  type t = Duke | Assassin | Captain | Ambassador | Contessa
  [@@deriving equal, sexp, compare]

  include Comparable.S_plain with type t := t
  include Stringable.S with type t := t
end

module Player_id : sig
  type t [@@deriving sexp, equal, compare, yojson]

  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
end

(* Actions that cannot be challenged by other players *)
type non_challengable_actions =
  [ `Income (* take 1 coin from the Treasury *)
  | `Foreign_aid (* take 2 coins (subject to blocking) *)
  | `Coup of
    Player_id.t (* pay 7 coins to launch a coup against target player *) ]

(* Actions that can be challenged by other players *)
type challengable_actions =
  [ `Tax (* Duke: take 3 coins from the Treasury *)
  | `Assassinate of
    Player_id.t (* Assassin: pay 3 coins to assassinate target *)
  | `Steal of Player_id.t
  | `Exchange (* Ambassador: exchange cards with the Court deck *) ]

(* Responses that can be challenged by other players *)
type challengable_responses =
  [ `Block_assassination
  | `Block_foreign_aid
  | `Block_steal of [ `Ambassador | `Captain ] ]

module Challengable : sig
  type t = [ challengable_actions | challengable_responses ]

  val required_card : t -> Card.t
  val to_string : t -> string
end

(* Actions a player may choose. Some actions have a target (represented by a player id). *)
module Action : sig
  type t = [ challengable_actions | non_challengable_actions ]

  val to_string : t -> string
end

module Hand : sig
  type t =
    | Both of Card.t * Card.t
    | One of { hidden : Card.t; revealed : Card.t }
  [@@deriving sexp]
end

module Cancelled_reason : sig
  type t = Other_player_responded of Player_id.t [@@deriving sexp]
end

module Visible_game_state : sig
  module Other_player : sig
    type t = {
      player_id : Player_id.t;
      visible_card : Card.t option;
      coins : int;
    }
    [@@deriving sexp]
  end

  type t = {
    hand : Hand.t;
    coins : int;
    other_players : Other_player.t list;
    active_player_id : Player_id.t;
  }
  [@@deriving sexp]

  val to_string_pretty : t -> string
end

module type Player_io_S = sig
  type t

  val choose_action :
    t -> visible_game_state:Visible_game_state.t -> Action.t Deferred.t

  val choose_assasination_response :
    t ->
    visible_game_state:Visible_game_state.t ->
    asassinating_player_id:Player_id.t ->
    [ `Allow | `Block ] Deferred.t

  val choose_foreign_aid_response :
    t ->
    visible_game_state:Visible_game_state.t ->
    unit ->
    cancelled_reason:Cancelled_reason.t Deferred.t ->
    [ `Allow | `Block ] Deferred.t

  val choose_steal_response :
    t ->
    visible_game_state:Visible_game_state.t ->
    stealing_player_id:Player_id.t ->
    [ `Allow | `Block of [ `Ambassador | `Captain ] ] Deferred.t

  val choose_cards_to_return :
    t ->
    visible_game_state:Visible_game_state.t ->
    Card.t ->
    Card.t ->
    Hand.t ->
    (Card.t * Card.t) Deferred.t

  val reveal_card :
    t ->
    visible_game_state:Visible_game_state.t ->
    card_1:Card.t ->
    card_2:Card.t ->
    [ `Card_1 | `Card_2 ] Deferred.t

  val offer_challenge :
    t ->
    visible_game_state:Visible_game_state.t ->
    Player_id.t ->
    Challengable.t ->
    cancelled_reason:Cancelled_reason.t Deferred.t ->
    [ `Challenge | `No_challenge ] Deferred.t

  val notify_of_game_start :
    t -> visible_game_state:Visible_game_state.t -> unit Deferred.t

  val notify_of_action_choice : t -> Player_id.t -> Action.t -> unit Deferred.t
  val notify_of_lost_influence : t -> Player_id.t -> Card.t -> unit Deferred.t
  val notify_of_new_card : t -> Card.t -> unit Deferred.t

  val notify_of_challenge :
    t ->
    challenging_player_id:Player_id.t ->
    has_required_card:bool ->
    unit Deferred.t

  (* TODO notify_of_game_end ? *)
end
