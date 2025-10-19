open! Core
open Async
open Ocoup.For_testing.Types
module Game = Ocoup.For_testing.Game
module Game_state = Game.Game_state

module Response = struct
  type t =
    | Choose_action of Action.t
    | Choose_assasination_response of [ `Allow | `Block ]
    | Choose_foreign_aid_response of [ `Allow | `Block ]
    | Choose_steal_response of [ `Allow | `Block of [ `Ambassador | `Captain ] ]
    | Choose_cards_to_return
    | Reveal_card of [ `Card_1 | `Card_2 ]
    | Offer_challenge of [ `Challenge | `No_challenge ]

  let to_string = function
    | Choose_action action -> [%string "Choose_action: %{action#Action}"]
    | Choose_assasination_response _ -> "Choose_assasination_response"
    | Choose_foreign_aid_response _ -> "Choose_foreign_aid_response"
    | Choose_steal_response _ -> "Choose_steal_response"
    | Choose_cards_to_return -> "Choose_cards_to_return"
    | Reveal_card _ -> "Reveal_card"
    | Offer_challenge _ -> "Offer_challenge"
end

module Test_player_io : sig
  include Player_io_S

  val create : int -> (int * Response.t) Queue.t -> t
end = struct
  type t = { player_index : int; responses : (int * Response.t) Queue.t }

  let create player_index responses = { player_index; responses }

  let dequeue_exn t =
    let i, choice = Queue.dequeue_exn t.responses in
    if i <> t.player_index then
      raise_s
        [%message "Unexpected player action" (i : int) (t.player_index : int)];
    print_endline [%string "Player %{i#Int}: %{choice#Response}"];
    choice

  let choose_action t ~visible_game_state:_ =
    let response = dequeue_exn t in
    match response with
    | Response.Choose_action action -> return action
    | _ -> failwith "Expected Choose_action response"

  let choose_assasination_response t ~visible_game_state:_
      ~asassinating_player_id:_ =
    let response = dequeue_exn t in
    match response with
    | Response.Choose_assasination_response response -> return response
    | _ -> failwith "Expected Choose_assasination_response response"

  let choose_foreign_aid_response t ~visible_game_state:_ () ~cancelled_reason:_
      =
    let response = dequeue_exn t in
    match response with
    | Response.Choose_foreign_aid_response response -> return response
    | _ -> failwith "Expected Choose_foreign_aid_response response"

  let choose_steal_response t ~visible_game_state:_ ~stealing_player_id:_ =
    let response = dequeue_exn t in
    match response with
    | Response.Choose_steal_response response -> return response
    | _ -> failwith "Expected Choose_steal_response response"

  let choose_cards_to_return t ~visible_game_state:_ card_1 card_2 _hand =
    let response = dequeue_exn t in
    match response with
    | Response.Choose_cards_to_return -> return (card_1, card_2)
    | _ -> failwith "Expected Choose_cards_to_return response"

  let reveal_card t ~visible_game_state:_ ~card_1:_ ~card_2:_ =
    let response = dequeue_exn t in
    match response with
    | Response.Reveal_card card -> return card
    | _ -> failwith "Expected Reveal_card response"

  let offer_challenge t ~visible_game_state:_ _acting_player_id _challengable
      ~cancelled_reason:_ =
    let response = dequeue_exn t in
    match response with
    | Response.Offer_challenge challenge -> return challenge
    | _ -> failwith "Expected Offer_challenge response"

  let notify_of_game_start _t ~visible_game_state:_ = return ()
  let notify_of_action_choice _t _player_id _action = return ()
  let notify_of_lost_influence _t _player_id _card = return ()
  let notify_of_new_card _t _card = return ()

  let notify_of_challenge _t ~challenging_player_id:_ ~has_required_card:_ =
    return ()
end

let run_test moves_list =
  let moves = Queue.of_list moves_list in
  let game_state =
    let open Game in
    {
      Game_state.players =
        [
          {
            Player.id = Player_id.of_int 0;
            player_io =
              Ocoup.For_testing.Player_ios.Player_io.create
                (module Test_player_io)
                (Test_player_io.create 0 moves);
            coins = 2;
            hand = Hand.Both (Card.Duke, Card.Assassin);
          };
          {
            Player.id = Player_id.of_int 1;
            player_io =
              Ocoup.For_testing.Player_ios.Player_io.create
                (module Test_player_io)
                (Test_player_io.create 1 moves);
            coins = 2;
            hand = Hand.Both (Card.Ambassador, Card.Contessa);
          };
        ];
      deck =
        [
          Card.Ambassador;
          Contessa;
          Captain;
          Duke;
          Ambassador;
          Duke;
          Captain;
          Captain;
          Assassin;
          Contessa;
        ];
    }
  in
  let%map result =
    Deferred.repeat_until_finished game_state (fun game_state ->
        match Queue.is_empty moves with
        | true -> return (`Finished game_state)
        | false -> (
            match%map Game.take_turn game_state with
            | `Finished game_state -> `Finished game_state
            | `Repeat game_state -> `Repeat game_state))
  in
  print_endline "Game state after all moves:";
  print_s [%sexp (result : Game_state.t)]

let%expect_test _ =
  let%bind () =
    run_test
      [
        (0, Response.Choose_action `Income);
        (1, Choose_action `Tax);
        (0, Offer_challenge `No_challenge);
        (0, Choose_action `Exchange);
        (0, Choose_cards_to_return);
      ]
  in
  [%expect
    {|
    ->Player id: 0	Coins: 2	Hand: (Both Duke Assassin)
      Player id: 1	Coins: 2	Hand: (Both Ambassador Contessa)

    Player 0: Choose_action: Income
      Player id: 0	Coins: 3	Hand: (Both Duke Assassin)
    ->Player id: 1	Coins: 2	Hand: (Both Ambassador Contessa)

    Player 1: Choose_action: Tax
    Player 0: Offer_challenge
    ->Player id: 0	Coins: 3	Hand: (Both Duke Assassin)
      Player id: 1	Coins: 5	Hand: (Both Ambassador Contessa)

    Player 0: Choose_action: Exchange
    Player 0: Choose_cards_to_return
    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 5) (hand (Both Ambassador Contessa)))
       ((id 0) (player_io <opaque>) (coins 3) (hand (Both Assassin Duke)))))
     (deck
      (Captain Contessa Duke Ambassador Captain Captain Assassin Duke Ambassador
       Contessa)))
    |}];
  return ()
