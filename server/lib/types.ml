open! Core
open! Async

module Card = struct
  module T = struct
    type t = Duke | Assassin | Captain | Ambassador | Contessa
    [@@deriving equal, sexp, compare]
  end

  include T
  include Comparable.Make_plain (T)

  let to_string = function
    | Duke -> "Duke"
    | Assassin -> "Assassin"
    | Captain -> "Captain"
    | Ambassador -> "Ambassador"
    | Contessa -> "Contessa"

  let of_string = function
    | "Duke" -> Duke
    | "Assassin" -> Assassin
    | "Captain" -> Captain
    | "Ambassador" -> Ambassador
    | "Contessa" -> Contessa
    | _ -> failwith "Invalid card"
end

module Player_id : sig
  type t [@@deriving sexp, equal, compare, yojson]

  include Comparable.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
end = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  module T = struct
    type t = int [@@deriving sexp, equal, compare, yojson]
  end

  include T
  include Comparable.Make (T)

  let of_int = Fn.id
  let to_int = Fn.id
  let to_string = Int.to_string
end

type non_challengable_actions = [ `Income | `Foreign_aid | `Coup of Player_id.t ]

type challengable_actions =
  [ `Tax | `Assassinate of Player_id.t | `Steal of Player_id.t | `Exchange ]

type challengable_responses =
  [ `Block_assassination
  | `Block_steal of [ `Captain | `Ambassador ]
  | `Block_foreign_aid ]

module Challengable = struct
  type t = [ challengable_actions | challengable_responses ]

  let required_card = function
    | `Tax -> Card.Duke
    | `Assassinate _ -> Assassin
    | `Steal _ -> Captain
    | `Exchange -> Ambassador
    | `Block_assassination -> Contessa
    | `Block_steal `Captain -> Captain
    | `Block_steal `Ambassador -> Ambassador
    | `Block_foreign_aid -> Duke

  let to_string = function
    | `Tax -> "Tax"
    | `Assassinate target_player_id ->
        sprintf "Assassinate %d" (Player_id.to_int target_player_id)
    | `Steal target_player_id ->
        sprintf "Steal from %d" (Player_id.to_int target_player_id)
    | `Exchange -> "Exchange"
    | `Block_assassination -> "Block assassination"
    | `Block_steal `Captain -> "Block steal with Captain"
    | `Block_steal `Ambassador -> "Block steal with Ambassador"
    | `Block_foreign_aid -> "Block foreign aid"
end

(* Actions a player may choose. Some actions have a target (represented by a player id). *)
module Action = struct
  type t = [ challengable_actions | non_challengable_actions ]

  let to_string = function
    | `Income -> "Income"
    | `Foreign_aid -> "Foreign aid"
    | `Coup target_player_id ->
        sprintf "Coup %d" (Player_id.to_int target_player_id)
    | `Tax -> "Tax"
    | `Assassinate target_player_id ->
        sprintf "Assassinate %d" (Player_id.to_int target_player_id)
    | `Steal target_player_id ->
        sprintf "Steal %d" (Player_id.to_int target_player_id)
    | `Exchange -> "Exchange"
end

module Hand = struct
  type t =
    | Both of Card.t * Card.t
    | One of { hidden : Card.t; revealed : Card.t }
  [@@deriving sexp]
end

module Cancelled_reason = struct
  type t = Other_player_responded of Player_id.t [@@deriving sexp]
end

module Visible_game_state = struct
  module Other_player = struct
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

  let to_string_pretty t =
    let cards =
      match t.hand with
      | Hand.Both (card_1, card_2) ->
          [%string "%{card_1#Card} (hidden) %{card_2#Card} (hidden)"]
      | Hand.One { hidden; revealed } ->
          [%string "%{hidden#Card} (hidden) %{revealed#Card} (revealed)"]
    in
    let coins = [%string "You - Coins: %{t.coins#Int} Hand: %{cards}"] in
    let other_players =
      List.map t.other_players ~f:(fun other_player ->
          let revealed_string =
            match other_player.visible_card with
            | Some card -> [%string " Revealed: %{card#Card}"]
            | None -> ""
          in
          [%string
            "Player %{other_player.player_id#Player_id} - Coins: \
             %{other_player.coins#Int} %{revealed_string}"])
    in
    String.concat_lines (coins :: other_players)
end

open Async

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
    [ `Allow | `Block of [ `Captain | `Ambassador ] ] Deferred.t

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
    [ `No_challenge | `Challenge ] Deferred.t

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
end
