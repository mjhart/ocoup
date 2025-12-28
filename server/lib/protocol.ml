open! Core
open Types
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Card = struct
  include Card

  let yojson_of_t card = `String (to_string card)
end

module Action = struct
  include Action

  let yojson_of_t : t -> Yojson.Safe.t = function
    | `Income -> `Assoc [ ("type", `String "Income") ]
    | `Foreign_aid -> `Assoc [ ("type", `String "Foreign_aid") ]
    | `Assassinate player_id ->
        `Assoc
          [
            ("type", `String "Assassinate");
            ("player_id", `Int (Player_id.to_int player_id));
          ]
    | `Coup player_id ->
        `Assoc
          [
            ("type", `String "Coup");
            ("player_id", `Int (Player_id.to_int player_id));
          ]
    | `Exchange -> `Assoc [ ("type", `String "Exchange") ]
    | `Steal player_id ->
        `Assoc
          [
            ("type", `String "Steal");
            ("player_id", `Int (Player_id.to_int player_id));
          ]
    | `Tax -> `Assoc [ ("type", `String "Tax") ]

  let t_of_yojson : Yojson.Safe.t -> t = function
    | `Assoc [ ("type", `String "Income") ] -> `Income
    | `Assoc [ ("type", `String "Tax") ] -> `Tax
    | `Assoc [ ("type", `String "Foreign_aid") ] -> `Foreign_aid
    | `Assoc [ ("type", `String "Assassinate"); ("player_id", `Int player_id) ]
      ->
        `Assassinate (Player_id.of_int player_id)
    | `Assoc [ ("type", `String "Coup"); ("player_id", `Int player_id) ] ->
        `Coup (Player_id.of_int player_id)
    | `Assoc [ ("type", `String "Exchange") ] -> `Exchange
    | `Assoc [ ("type", `String "Steal"); ("player_id", `Int player_id) ] ->
        `Steal (Player_id.of_int player_id)
    | _ -> `Income
end

module Allow_or_block = struct
  type t = [ `Allow | `Block ]

  let t_of_yojson : Yojson.Safe.t -> t = function
    | `Assoc [ ("type", `String "Allow") ] -> `Allow
    | `Assoc [ ("type", `String "Block") ] -> `Block
    | _ -> `Allow
end

module Challengable = struct
  include Challengable

  let yojson_of_t : t -> Yojson.Safe.t = function
    | `Tax -> `Assoc [ ("type", `String "Tax") ]
    | `Assassinate player_id ->
        `Assoc
          [
            ("type", `String "Assassinate");
            ("player_id", `Int (Player_id.to_int player_id));
          ]
    | `Exchange -> `Assoc [ ("type", `String "Exchange") ]
    | `Steal player_id ->
        `Assoc
          [
            ("type", `String "Steal");
            ("player_id", `Int (Player_id.to_int player_id));
          ]
    | `Block_assassination -> `Assoc [ ("type", `String "Block_assassination") ]
    | `Block_steal blocking_card ->
        let blocking_card =
          match blocking_card with
          | `Ambassador -> "Ambassador"
          | `Captain -> "Captain"
        in
        `Assoc
          [
            ("type", `String "Block_steal");
            ("blocking_card", `String blocking_card);
          ]
    | `Block_foreign_aid -> `Assoc [ ("type", `String "Block_foreign_aid") ]
end

module Visible_game_state = struct
  include Visible_game_state

  let yojson_of_t : t -> Yojson.Safe.t =
   fun { hand; coins; other_players; active_player_id } ->
    let hand =
      match hand with
      | Hand.Both (card_1, card_2) ->
          `List
            [
              `Assoc
                [ ("card", Card.yojson_of_t card_1); ("revealed", `Bool false) ];
              `Assoc
                [ ("card", Card.yojson_of_t card_2); ("revealed", `Bool false) ];
            ]
      | Hand.One { hidden; revealed } ->
          `List
            [
              `Assoc
                [ ("card", Card.yojson_of_t hidden); ("revealed", `Bool false) ];
              `Assoc
                [
                  ("card", Card.yojson_of_t revealed); ("revealed", `Bool true);
                ];
            ]
    in
    let other_players =
      List.map other_players
        ~f:(fun { Other_player.player_id; visible_card; coins } ->
          `Assoc
            [
              ("player_id", Player_id.yojson_of_t player_id);
              ("visible_card", [%yojson_of: Card.t option] visible_card);
              ("coins", `Int coins);
            ])
    in
    `Assoc
      [
        ("hand", hand);
        ("coins", `Int coins);
        ("other_players", `List other_players);
        ("active_player_id", Player_id.yojson_of_t active_player_id);
      ]
end

module Game_start = struct
  let query ~player_id ~visible_game_state =
    `Assoc
      [
        ("type", `String "Game_start");
        ("visible_game_state", Visible_game_state.yojson_of_t visible_game_state);
        ("self_player_id", Player_id.yojson_of_t player_id);
      ]
end

module Choose_action = struct
  let query ~visible_game_state =
    `Assoc
      [
        ("type", `String "Choose_action");
        ("visible_game_state", Visible_game_state.yojson_of_t visible_game_state);
      ]

  let response_of_yojson = Action.t_of_yojson
end

module Choose_assasination_response = struct
  let create_query player_id ~visible_game_state =
    `Assoc
      [
        ("type", `String "Choose_assasination_response");
        ("player_id", Player_id.yojson_of_t player_id);
        ("visible_game_state", Visible_game_state.yojson_of_t visible_game_state);
      ]

  let response_of_yojson = Allow_or_block.t_of_yojson
end

module Choose_foreign_aid_response = struct
  let create_query ~visible_game_state () =
    `Assoc
      [
        ("type", `String "Choose_foreign_aid_response");
        ("visible_game_state", Visible_game_state.yojson_of_t visible_game_state);
      ]

  let response_of_yojson = Allow_or_block.t_of_yojson
end

module Choose_steal_response = struct
  type response = [ `Allow | `Block of [ `Ambassador | `Captain ] ]

  let create_query stealing_player_id ~visible_game_state =
    `Assoc
      [
        ("type", `String "Choose_steal_response");
        ("player_id", Player_id.yojson_of_t stealing_player_id);
        ("visible_game_state", Visible_game_state.yojson_of_t visible_game_state);
      ]

  let response_of_yojson : Yojson.Safe.t -> response = function
    | `Assoc [ ("type", `String "Allow") ] -> `Allow
    | `Assoc [ ("type", `String "Block"); ("card", `String "Ambassador") ] ->
        `Block `Ambassador
    | `Assoc [ ("type", `String "Block"); ("card", `String "Captain") ] ->
        `Block `Captain
    | _ -> `Allow
end

module Choose_cards_to_return = struct
  let create_query cards ~visible_game_state =
    `Assoc
      [
        ("type", `String "Choose_cards_to_return");
        ("cards", `List (List.map ~f:Card.yojson_of_t cards));
        ("visible_game_state", Visible_game_state.yojson_of_t visible_game_state);
      ]

  let response_of_yojson = function
    | `List [ `String card_1; `String card_2 ] ->
        (Card.of_string card_1, Card.of_string card_2)
    | _ -> failwith "Invalid choose cards to return response"
end

module Reveal_card = struct
  let create_query card_1 card_2 ~visible_game_state =
    `Assoc
      [
        ("type", `String "Reveal_card");
        ("card_1", Card.yojson_of_t card_1);
        ("card_2", Card.yojson_of_t card_2);
        ("visible_game_state", Visible_game_state.yojson_of_t visible_game_state);
      ]

  let response_of_yojson = function
    | `Assoc [ ("type", `String "Card_1") ] -> `Card_1
    | `Assoc [ ("type", `String "Card_2") ] -> `Card_2
    | _ -> `Card_1
end

module Offer_challenge = struct
  let create_query acting_player_id challengable ~visible_game_state =
    `Assoc
      [
        ("type", `String "Offer_challenge");
        ("acting_player_id", Player_id.yojson_of_t acting_player_id);
        ("action", Challengable.yojson_of_t challengable);
        ("visible_game_state", Visible_game_state.yojson_of_t visible_game_state);
      ]

  let response_of_yojson = function
    | `Assoc [ ("type", `String "No_challenge") ] -> `No_challenge
    | `Assoc [ ("type", `String "Challenge") ] -> `Challenge
    | _ -> `No_challenge
end

module Action_choice_notification = struct
  let create_query player_id action =
    `Assoc
      [
        ("type", `String "Action_chosen");
        ("player_id", Player_id.yojson_of_t player_id);
        ("action", Action.yojson_of_t action);
      ]
end

module Lost_influence_notification = struct
  let create_query player_id card =
    `Assoc
      [
        ("type", `String "Lost_influence");
        ("player_id", Player_id.yojson_of_t player_id);
        ("card", Card.yojson_of_t card);
      ]
end

module New_card_notification = struct
  let create_query card =
    `Assoc [ ("type", `String "New_card"); ("card", Card.yojson_of_t card) ]
end

module Challenge_notification = struct
  let create_query challenging_player_id has_required_card =
    `Assoc
      [
        ("type", `String "Challenge");
        ("player_id", Player_id.yojson_of_t challenging_player_id);
        ("has_required_card", `Bool has_required_card);
      ]
end

module Player_responded_notification = struct
  let create_query player_id =
    `Assoc
      [
        ("type", `String "Player_responded");
        ("player_id", Player_id.yojson_of_t player_id);
      ]
end

module Create_game_request = struct
  type t = { bot_players : string list }

  let of_yojson : Yojson.Safe.t -> t = function
    | `Assoc fields ->
        let bot_players =
          match List.Assoc.find fields ~equal:String.equal "bot_players" with
          | Some (`List bots) ->
              List.filter_map bots ~f:(function
                | `String s -> Some s
                | _ -> None)
          | _ -> []
        in
        { bot_players }
    | _ -> { bot_players = [] }
end

module Create_game_response = struct
  let create ~game_id ~num_bot_players =
    `Assoc
      [
        ("game_id", `String game_id);
        ("updates_url", `String [%string "/games/%{game_id}/updates"]);
        ("player_url", `String [%string "/games/%{game_id}/player"]);
        ("num_bot_players", `Int num_bot_players);
      ]
end

module Create_tournament_request = struct
  type t = { max_players : int; bot_players : string list }

  let of_yojson : Yojson.Safe.t -> t = function
    | `Assoc fields ->
        let max_players =
          match List.Assoc.find fields ~equal:String.equal "max_players" with
          | Some (`Int n) -> n
          | _ -> 12
        in
        let bot_players =
          match List.Assoc.find fields ~equal:String.equal "bot_players" with
          | Some (`List bots) ->
              List.filter_map bots ~f:(function
                | `String s -> Some s
                | _ -> None)
          | _ -> []
        in
        { max_players; bot_players }
    | _ -> { max_players = 12; bot_players = [] }
end

module Create_tournament_response = struct
  let create ~tournament_id ~num_bot_players =
    `Assoc
      [
        ("tournament_id", `String tournament_id);
        ( "register_url",
          `String [%string "/tournaments/%{tournament_id}/register"] );
        ("num_bot_players", `Int num_bot_players);
      ]
end

module Tournament_registration_response = struct
  let registered ~player_id =
    `Assoc [ ("status", `String "registered"); ("player_id", `Int player_id) ]
end

module Tournament_results = struct
  type game_result =
    | Completed of { winners : Player_id.t list; eliminated : Player_id.t list }
    | Error of string

  let game_result_to_yojson ~game_idx = function
    | Completed { winners; eliminated } ->
        `Assoc
          [
            ("game", `Int (game_idx + 1));
            ("status", `String "completed");
            ( "winners",
              `List
                (List.map winners ~f:(fun id ->
                     `String (Player_id.to_string id))) );
            ( "eliminated",
              `List
                (List.map eliminated ~f:(fun id ->
                     `String (Player_id.to_string id))) );
          ]
    | Error err ->
        `Assoc
          [
            ("game", `Int (game_idx + 1));
            ("status", `String "error");
            ("error", `String err);
          ]

  let results_to_yojson results =
    `List
      (List.mapi results ~f:(fun round_idx round ->
           `Assoc
             [
               ("round", `Int (round_idx + 1));
               ( "games",
                 `List
                   (List.mapi round ~f:(fun game_idx game_result ->
                        game_result_to_yojson ~game_idx game_result)) );
             ]))

  let scores_to_yojson scores =
    Map.to_alist scores
    |> List.map ~f:(fun (player_id, score) ->
           (Player_id.to_string player_id, `Int score))
    |> fun assoc -> `Assoc assoc

  let create_response ~scores ~results =
    `Assoc
      [
        ("status", `String "completed");
        ("scores", scores_to_yojson scores);
        ("results", results_to_yojson results);
      ]
end

module Error_response = struct
  let create message = `Assoc [ ("error", `String message) ]
end
