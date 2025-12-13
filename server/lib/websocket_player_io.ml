open! Core
open! Async
open Types

type t = {
  player_id : Player_id.t;
  to_client : string Pipe.Writer.t;
  mutable expected_message_handler : Yojson.Safe.t Ivar.t;
}

let create ~player_id ~reader ~writer =
  let t =
    { player_id; to_client = writer; expected_message_handler = Ivar.create () }
  in
  don't_wait_for
    (Pipe.map reader ~f:Yojson.Safe.from_string
    |> Pipe.iter ~f:(fun message ->
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
