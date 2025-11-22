open! Core
open! Async
open Types

module Protocol = struct
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
      | `Assoc
          [ ("type", `String "Assassinate"); ("player_id", `Int player_id) ] ->
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
                  [
                    ("card", Card.yojson_of_t card_1); ("revealed", `Bool false);
                  ];
                `Assoc
                  [
                    ("card", Card.yojson_of_t card_2); ("revealed", `Bool false);
                  ];
              ]
        | Hand.One { hidden; revealed } ->
            `List
              [
                `Assoc
                  [
                    ("card", Card.yojson_of_t hidden); ("revealed", `Bool false);
                  ];
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
          ( "visible_game_state",
            Visible_game_state.yojson_of_t visible_game_state );
          ("self_player_id", Player_id.yojson_of_t player_id);
        ]
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
      | `Block_assassination ->
          `Assoc [ ("type", `String "Block_assassination") ]
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

  module Choose_action = struct
    let query ~visible_game_state =
      `Assoc
        [
          ("type", `String "Choose_action");
          ( "visible_game_state",
            Visible_game_state.yojson_of_t visible_game_state );
        ]

    let response_of_yojson = Action.t_of_yojson
  end

  (* TODO can get [Player_id.t] from visible game state *)
  module Choose_assasination_response = struct
    let create_query player_id ~visible_game_state =
      `Assoc
        [
          ("type", `String "Choose_assasination_response");
          ("player_id", Player_id.yojson_of_t player_id);
          ( "visible_game_state",
            Visible_game_state.yojson_of_t visible_game_state );
        ]

    let response_of_yojson = Allow_or_block.t_of_yojson
  end

  module Choose_foreign_aid_response = struct
    let create_query ~visible_game_state () =
      `Assoc
        [
          ("type", `String "Choose_foreign_aid_response");
          ( "visible_game_state",
            Visible_game_state.yojson_of_t visible_game_state );
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
          ( "visible_game_state",
            Visible_game_state.yojson_of_t visible_game_state );
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
          ( "visible_game_state",
            Visible_game_state.yojson_of_t visible_game_state );
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
          ( "visible_game_state",
            Visible_game_state.yojson_of_t visible_game_state );
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
          ( "visible_game_state",
            Visible_game_state.yojson_of_t visible_game_state );
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
end

type t = {
  player_id : Player_id.t;
  to_client : string Pipe.Writer.t;
  from_client : Yojson.Safe.t Pipe.Reader.t;
  mutable expected_message_handler : Yojson.Safe.t Ivar.t;
}

let create ~player_id ~reader ~writer =
  let t =
    {
      player_id;
      to_client = writer;
      from_client = reader;
      expected_message_handler = Ivar.create ();
    }
  in
  don't_wait_for
    (Pipe.iter t.from_client ~f:(fun message ->
         (match Ivar.peek t.expected_message_handler with
         | Some prior_message ->
             Log.Global.info_s
               [%message
                 "Received unexpected message"
                   ~prior_message:(Yojson.Safe.to_string prior_message)
                   ~message:(Yojson.Safe.to_string message)]
         | None -> Ivar.fill_exn t.expected_message_handler message);
         return ()));
  t

let write_to_client t query =
  Pipe.write t.to_client (Yojson.Safe.to_string query)

let read_from_client t f =
  let ivar = Ivar.create () in
  t.expected_message_handler <- ivar;
  let%map message = Ivar.read ivar in
  f message

let upon_next_message' t ~cancelled_reason ~on_cancel f =
  let ivar = Ivar.create () in
  t.expected_message_handler <- ivar;
  choose
    [ choice (Ivar.read ivar) f; choice cancelled_reason (Fn.const on_cancel) ]

let choose_action t ~visible_game_state =
  let response = read_from_client t Protocol.Choose_action.response_of_yojson in
  let%bind () =
    write_to_client t (Protocol.Choose_action.query ~visible_game_state)
  in
  response

let choose_assasination_response t ~visible_game_state ~asassinating_player_id =
  let query =
    Protocol.Choose_assasination_response.create_query asassinating_player_id
      ~visible_game_state
  in
  let response =
    read_from_client t Protocol.Choose_assasination_response.response_of_yojson
  in
  let%bind () = write_to_client t query in
  response

let choose_foreign_aid_response t ~visible_game_state () ~cancelled_reason =
  let query =
    Protocol.Choose_foreign_aid_response.create_query ~visible_game_state ()
  in

  upon cancelled_reason (function
      | Cancelled_reason.Other_player_responded player_id ->
      let query =
        Protocol.Player_responded_notification.create_query player_id
      in
      write_to_client t query |> don't_wait_for);

  let f =
   fun message ->
    Protocol.Choose_foreign_aid_response.response_of_yojson message
  in
  let response = upon_next_message' t ~cancelled_reason ~on_cancel:`Allow f in
  let%bind () = write_to_client t query in
  response

let choose_steal_response t ~visible_game_state ~stealing_player_id =
  let query =
    Protocol.Choose_steal_response.create_query stealing_player_id
      ~visible_game_state
  in
  let response =
    read_from_client t Protocol.Choose_steal_response.response_of_yojson
  in
  let%bind () = write_to_client t query in
  response

let choose_cards_to_return t ~visible_game_state card_1 card_2 hand =
  let query =
    Protocol.Choose_cards_to_return.create_query
      ([ card_1; card_2 ]
      @
      match hand with
      | Hand.Both (card_1, card_2) -> [ card_1; card_2 ]
      | Hand.One { hidden; revealed = _ } -> [ hidden ])
      ~visible_game_state
  in
  let response =
    read_from_client t Protocol.Choose_cards_to_return.response_of_yojson
  in
  let%bind () = write_to_client t query in
  response

let reveal_card t ~visible_game_state ~card_1 ~card_2 =
  let query =
    Protocol.Reveal_card.create_query card_1 card_2 ~visible_game_state
  in
  let response = read_from_client t Protocol.Reveal_card.response_of_yojson in
  let%bind () = write_to_client t query in
  response

let offer_challenge t ~visible_game_state challenging_player_id challengable
    ~cancelled_reason =
  let query =
    Protocol.Offer_challenge.create_query challenging_player_id challengable
      ~visible_game_state
  in
  upon cancelled_reason (function
      | Cancelled_reason.Other_player_responded player_id ->
      let query =
        Protocol.Player_responded_notification.create_query player_id
      in
      write_to_client t query |> don't_wait_for);
  let response =
    upon_next_message' t ~cancelled_reason ~on_cancel:`No_challenge
      Protocol.Offer_challenge.response_of_yojson
  in
  let%bind () = write_to_client t query in
  response

let notify_of_action_choice t player_id action =
  let query =
    Protocol.Action_choice_notification.create_query player_id action
  in
  let%bind () = write_to_client t query in
  return ()

let notify_of_lost_influence t player_id card =
  let query =
    Protocol.Lost_influence_notification.create_query player_id card
  in
  let%bind () = write_to_client t query in
  return ()

let notify_of_new_card t card =
  let query = Protocol.New_card_notification.create_query card in
  let%bind () = write_to_client t query in
  return ()

let notify_of_challenge t ~challenging_player_id ~has_required_card =
  let query =
    Protocol.Challenge_notification.create_query challenging_player_id
      has_required_card
  in
  let%bind () = write_to_client t query in
  return ()

let notify_of_game_start t ~visible_game_state =
  let query =
    Protocol.Game_start.query ~player_id:t.player_id ~visible_game_state
  in
  write_to_client t query
