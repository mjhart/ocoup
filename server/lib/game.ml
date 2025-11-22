open! Core
open! Async
open Types
open Player_ios

module Player = struct
  type t = {
    id : Player_id.t;
    player_io : (Player_io.t[@sexp.opaque]);
    coins : int;
    hand : Hand.t;
  }
  [@@deriving sexp_of]

  let id t = t.id
end

module Game_state = struct
  type t = {
    players : Player.t list;  (** the first player in the list is active *)
    deck : Card.t list;
  }
  [@@deriving sexp_of]

  let deck t = t.deck
  let players t = t.players

  let modify_player t id ~f =
    let new_players =
      List.map t.players ~f:(fun player ->
          if Player_id.equal player.id id then f player else player)
    in
    { t with players = new_players }

  let get_active_player t = List.hd_exn t.players
  let get_active_player_id t = get_active_player t |> Player.id

  let player_in_game t id =
    List.find t.players ~f:(fun player -> Player_id.equal id player.id)
    |> Option.is_some

  let to_string_pretty t =
    let sorted_players =
      List.sort t.players ~compare:(fun a b -> Player_id.compare a.id b.id)
    in
    List.map sorted_players ~f:(fun player ->
        let hand_sexp = [%sexp_of: Hand.t] player.hand in
        let body =
          [%string
            "Player id: %{player.id#Player_id}\tCoins: \
             %{player.coins#Int}\tHand: %{hand_sexp#Sexp}"]
        in
        let prefix =
          if Player_id.equal player.id (get_active_player t).id then "->"
          else "  "
        in
        prefix ^ body)
    |> String.concat_lines

  let modify_active_player t ~f =
    let active_player = get_active_player t in

    { t with players = f active_player :: List.tl_exn t.players }

  let end_turn t =
    { t with players = List.tl_exn t.players @ [ List.hd_exn t.players ] }

  let get_player_if_exists t id =
    List.find t.players ~f:(fun p -> Player_id.equal p.id id)

  let get_player_exn t id =
    List.find_exn t.players ~f:(fun p -> Player_id.equal p.id id)

  let is_valid_action t active_player_id action =
    let player = get_player_exn t active_player_id in
    let cost, target_player_id =
      match action with
      | `Income | `Foreign_aid | `Tax | `Exchange -> (0, None)
      | `Coup target_player_id -> (7, Some target_player_id)
      | `Assassinate target_player_id -> (3, Some target_player_id)
      | `Steal target_player_id -> (0, Some target_player_id)
    in
    let has_enough_coins = player.coins >= cost
    and is_valid_target =
      Option.value_map target_player_id ~default:true
        ~f:(fun target_player_id ->
          List.find_map t.players ~f:(fun p ->
              if Player_id.equal p.id target_player_id then Some p else None)
          |> Option.is_some)
    and not_targeting_self =
      Option.value_map target_player_id ~default:true
        ~f:(fun target_player_id ->
          not (Player_id.equal target_player_id active_player_id))
    and coup_if_required =
      match player.coins < 10 with
      | true -> true
      | false -> ( match action with `Coup _ -> true | _ -> false)
    in
    has_enough_coins && is_valid_target && not_targeting_self
    && coup_if_required

  let has_card t id card =
    let player = get_player_exn t id in
    match player.hand with
    | Hand.Both (card_1, card_2) ->
        List.mem [ card_1; card_2 ] card ~equal:Card.equal
    | Hand.One { hidden; revealed = _ } -> Card.equal hidden card

  let to_visible_game_state t player_id =
    let player = get_player_exn t player_id in
    let other_players =
      List.filter t.players ~f:(fun p -> not (Player_id.equal p.id player_id))
      |> List.map ~f:(fun p ->
             let visible_card =
               match p.hand with
               | Hand.Both _ -> None
               | Hand.One { hidden = _; revealed } -> Some revealed
             in
             {
               Visible_game_state.Other_player.player_id = p.id;
               visible_card;
               coins = p.coins;
             })
    in
    {
      Visible_game_state.hand = player.hand;
      coins = player.coins;
      other_players;
      active_player_id = get_active_player_id t;
    }

  let sorted_deck =
    [ 1; 2; 3 ]
    |> List.concat_map ~f:(fun _i ->
           [ Card.Duke; Assassin; Captain; Ambassador; Contessa ])

  let init' player_io_creators =
    let%bind.Deferred.Or_error num_players =
      match List.length player_io_creators with
      | 0 -> Deferred.Or_error.error_string "No players"
      | 1 -> Deferred.Or_error.error_string "Only one player"
      | num_players when num_players > 6 ->
          Deferred.Or_error.error_string "Too many players"
      | num_players -> Deferred.Or_error.return num_players
    in
    let deck =
      (* Random.self_init (); *)
      List.permute sorted_deck
    in
    let paired_deck, _always_none =
      List.fold deck ~init:([], None) ~f:(fun (pairs_acc, prev) card ->
          match prev with
          | None -> (pairs_acc, Some card)
          | Some prev -> ((card, prev) :: pairs_acc, None))
    in
    let player_cards, remaining_cards = List.split_n paired_deck num_players in
    let%map players =
      List.zip_exn player_cards player_io_creators
      |> Deferred.List.map ~how:`Sequential
           ~f:(fun ((card_1, card_2), (id, player_io_creator)) ->
             let%map player_io = player_io_creator () in
             {
               Player.id;
               coins = 2;
               player_io;
               hand = Hand.Both (card_1, card_2);
             })
    in
    let deck =
      List.concat_map remaining_cards ~f:(fun (carrd_1, card_2) ->
          [ carrd_1; card_2 ])
    in
    Ok { players; deck }

  let init player_io_creators =
    let player_io_creators' =
      List.mapi player_io_creators ~f:(fun i player_io_creator ->
          let id = Player_id.of_int i in
          (id, fun () -> player_io_creator id))
    in
    init' player_io_creators'
end

(* Actions *)

let take_income game_state =
  Game_state.modify_active_player game_state ~f:(fun active_player ->
      { active_player with coins = active_player.coins + 1 })

let game_over = Deferred.Result.fail

(* maybe return [Game_state.t * Player_id.t option] *)
let lose_influence game_state target_player_id =
  let%bind.Deferred.Result new_game_state, revealed_card =
    let player = Game_state.get_player_exn game_state target_player_id in
    match player.hand with
    | Hand.Both (card_1, card_2) ->
        let%map.Deferred.Result revealed_card_choice =
          Player_io.reveal_card player.player_io
            ~visible_game_state:
              (Game_state.to_visible_game_state game_state player.id)
            ~card_1 ~card_2
          >>| Result.return
        in
        let new_hand, revealed_card =
          match revealed_card_choice with
          | `Card_1 -> (Hand.One { hidden = card_2; revealed = card_1 }, card_1)
          | `Card_2 -> (Hand.One { hidden = card_1; revealed = card_2 }, card_2)
        in
        let new_game_state =
          Game_state.modify_player game_state target_player_id ~f:(fun player ->
              { player with hand = new_hand })
        in
        (new_game_state, revealed_card)
    | Hand.One { hidden; revealed = _ } -> (
        let remaining_players =
          List.filter game_state.players ~f:(fun player ->
              not (Player_id.equal player.id target_player_id))
        in
        match List.length remaining_players with
        | 1 -> game_over { game_state with players = remaining_players }
        | _ ->
            Deferred.Result.return
              ({ game_state with players = remaining_players }, hidden))
  in
  let%map.Deferred.Result () =
    Game_state.players new_game_state
    |> List.map ~f:(fun player ->
           Player_io.notify_of_lost_influence player.player_io target_player_id
             revealed_card)
    |> Deferred.all_unit >>| Result.return
  in
  new_game_state

let randomly_get_new_card game_state active_player_id card_to_replace =
  let deck_with_replacement = card_to_replace :: Game_state.deck game_state in
  let shuffled_deck = List.permute deck_with_replacement in
  let replacement_card = List.hd_exn shuffled_deck in
  let remaining_deck = List.tl_exn shuffled_deck in
  let new_game_state =
    Game_state.modify_player game_state active_player_id ~f:(fun player ->
        let new_hand =
          match player.hand with
          | Hand.Both (card_1, card_2) -> (
              match Card.equal card_1 card_to_replace with
              | true -> Hand.Both (replacement_card, card_2)
              | false -> Hand.Both (card_1, replacement_card))
          | Hand.One { hidden = _; revealed } ->
              Hand.One { hidden = replacement_card; revealed }
        in
        { player with hand = new_hand })
  in
  let%map () =
    Player_io.notify_of_new_card
      (Game_state.get_player_exn new_game_state active_player_id).player_io
      replacement_card
  in
  { new_game_state with deck = remaining_deck }

let handle_response_race game_state acting_player_id ~f =
  (* TODO: there's probably a memory leak in here *)
  let other_player_responded = Ivar.create () in
  let responses =
    Game_state.players game_state
    |> List.filter ~f:(fun player ->
           not (Player_id.equal player.id acting_player_id))
    |> List.map ~f:(fun player ->
           f player (Ivar.read other_player_responded) >>| fun response ->
           (player.id, response))
  in
  let blocked =
    responses
    |> List.map
         ~f:
           (Deferred.bind ~f:(fun ((player_id : Player_id.t), response) ->
                match response with
                | `Allow -> Deferred.never ()
                | `Block ->
                    Ivar.fill_if_empty other_player_responded
                      (Cancelled_reason.Other_player_responded player_id);
                    return (`Blocked_by player_id)))
    |> Deferred.any
  in
  let allowed =
    responses
    |> List.map
         ~f:
           (Deferred.bind ~f:(fun ((_ : Player_id.t), response) ->
                match response with
                | `Allow -> return `Allow
                | `Block -> Deferred.never ()))
    |> Deferred.all
    |> Deferred.map ~f:(fun _ -> `Allow)
  in
  Deferred.any [ blocked; allowed ]

let handle_challenge game_state acting_player_id (action : Challengable.t) =
  let%bind challenge_result =
    handle_response_race game_state acting_player_id
      ~f:(fun player cancelled_reason ->
        Player_io.offer_challenge player.player_io
          ~visible_game_state:
            (Game_state.to_visible_game_state game_state player.id)
          acting_player_id action ~cancelled_reason
        >>| function
        | `No_challenge -> `Allow
        | `Challenge -> `Block)
  in
  match challenge_result with
  | `Allow -> Deferred.Result.return (`Failed_or_no_challenge game_state)
  | `Blocked_by challenger_player_id -> (
      let required_card = Challengable.required_card action in
      let has_required_card =
        Game_state.has_card game_state acting_player_id required_card
      in
      let%bind.Deferred.Result () =
        Game_state.players game_state
        (* it's ok to do this sequntially. There's no race upon receiving this information (I think). *)
        |> Deferred.List.iter ~how:`Sequential ~f:(fun player ->
               Player_io.notify_of_challenge player.player_io
                 ~challenging_player_id:challenger_player_id ~has_required_card)
        >>| Result.return
      in
      match has_required_card with
      | false ->
          let%map.Deferred.Result new_game_state =
            lose_influence game_state acting_player_id
          in
          `Successfully_challenged new_game_state
      | true ->
          let%bind.Deferred.Result game_state_after_new_card =
            randomly_get_new_card game_state acting_player_id required_card
            >>| Result.return
          in
          let%map.Deferred.Result new_game_state =
            lose_influence game_state_after_new_card challenger_player_id
          in
          `Failed_or_no_challenge new_game_state)

let assassinate game_state active_player_id target_player_id =
  let game_state =
    Game_state.modify_active_player game_state ~f:(fun player ->
        { player with coins = player.coins - 3 })
  in
  match%bind.Deferred.Result
    handle_challenge game_state active_player_id (`Assassinate target_player_id)
  with
  | `Successfully_challenged post_challenge_game_state ->
      Deferred.Result.return post_challenge_game_state
  | `Failed_or_no_challenge post_challenge_game_state -> (
      match
        Game_state.get_player_if_exists post_challenge_game_state
          target_player_id
      with
      | None -> Deferred.Result.return post_challenge_game_state
      | Some target_player -> (
          match%bind.Deferred.Result
            Player_io.choose_assasination_response target_player.player_io
              ~visible_game_state:
                (Game_state.to_visible_game_state game_state target_player_id)
              ~asassinating_player_id:active_player_id
            >>| Result.return
          with
          | `Allow -> lose_influence post_challenge_game_state target_player_id
          | `Block -> (
              match%bind.Deferred.Result
                handle_challenge post_challenge_game_state target_player_id
                  `Block_assassination
              with
              | `Successfully_challenged post_second_challenge_game_state ->
                  if
                    Game_state.player_in_game post_second_challenge_game_state
                      target_player_id
                  then
                    lose_influence post_second_challenge_game_state
                      target_player_id
                  else Deferred.Result.return post_second_challenge_game_state
              | `Failed_or_no_challenge post_second_challenge_game_state ->
                  Deferred.Result.return post_second_challenge_game_state)))

let take_foreign_aid game_state =
  let take_two_coins game_state =
    Game_state.modify_active_player game_state ~f:(fun player ->
        { player with coins = player.coins + 2 })
  in
  match%bind
    handle_response_race game_state (Game_state.get_active_player game_state).id
      ~f:(fun player cancelled_reason ->
        Player_io.choose_foreign_aid_response player.player_io
          ~visible_game_state:
            (Game_state.to_visible_game_state game_state player.id)
          () ~cancelled_reason
        >>| function
        | `Allow -> `Allow
        | `Block -> `Block)
  with
  | `Blocked_by blocking_player_id -> (
      match%bind.Deferred.Result
        handle_challenge game_state blocking_player_id `Block_foreign_aid
      with
      | `Successfully_challenged post_second_challenge_game_state ->
          Deferred.Result.return
            (take_two_coins post_second_challenge_game_state)
      | `Failed_or_no_challenge post_second_challenge_game_state ->
          Deferred.Result.return post_second_challenge_game_state)
  | `Allow -> Deferred.Result.return (take_two_coins game_state)

let coup game_state target_player_id =
  let game_state =
    Game_state.modify_active_player game_state ~f:(fun player ->
        { player with coins = player.coins - 7 })
  in
  lose_influence game_state target_player_id

let take_tax game_state =
  match%map.Deferred.Result
    handle_challenge game_state (Game_state.get_active_player game_state).id
      `Tax
  with
  | `Successfully_challenged post_challenge_game_state ->
      post_challenge_game_state
  | `Failed_or_no_challenge post_challenge_game_state ->
      Game_state.modify_active_player post_challenge_game_state
        ~f:(fun player -> { player with coins = player.coins + 3 })

let steal game_state target_player_id =
  let steal_two_coins game_state =
    let target_player = Game_state.get_player_exn game_state target_player_id in
    let num_coins_to_steal = Int.min 2 target_player.coins in
    Game_state.modify_player game_state target_player_id ~f:(fun player ->
        { player with coins = player.coins - num_coins_to_steal })
    |> Game_state.modify_active_player ~f:(fun player ->
           { player with coins = player.coins + num_coins_to_steal })
  in
  match%bind.Deferred.Result
    handle_challenge game_state (Game_state.get_active_player game_state).id
      (`Steal target_player_id)
  with
  | `Successfully_challenged post_challenge_game_state ->
      Deferred.Result.return post_challenge_game_state
  | `Failed_or_no_challenge post_challenge_game_state -> (
      match
        Game_state.get_player_if_exists post_challenge_game_state
          target_player_id
      with
      | None -> Deferred.Result.return post_challenge_game_state
      | Some target_player -> (
          match%bind.Deferred.Result
            Player_io.choose_steal_response target_player.player_io
              ~visible_game_state:
                (Game_state.to_visible_game_state post_challenge_game_state
                   target_player.id)
              ~stealing_player_id:
                (Game_state.get_active_player post_challenge_game_state).id
            >>| Result.return
          with
          | `Allow ->
              steal_two_coins post_challenge_game_state
              |> Deferred.Result.return
          | `Block blocking_card -> (
              match%map.Deferred.Result
                handle_challenge post_challenge_game_state target_player_id
                  (`Block_steal blocking_card)
              with
              | `Successfully_challenged post_second_challenge_game_state -> (
                  match
                    Game_state.get_player_if_exists
                      post_second_challenge_game_state target_player_id
                  with
                  | None -> post_second_challenge_game_state
                  | Some _target_player ->
                      steal_two_coins post_second_challenge_game_state)
              | `Failed_or_no_challenge post_second_challenge_game_state ->
                  post_second_challenge_game_state)))

let exchange game_state =
  let active_player = Game_state.get_active_player game_state in
  match Game_state.deck game_state with
  | card_choice_1 :: card_choice_2 :: rest ->
      let%map.Deferred returned_card_1, returned_card_2 =
        Player_io.choose_cards_to_return active_player.player_io
          ~visible_game_state:
            (Game_state.to_visible_game_state game_state active_player.id)
          card_choice_1 card_choice_2 active_player.hand
      in
      let remove_cards cards_chosen_from =
        List.fold cards_chosen_from
          ~init:(`Both (returned_card_1, returned_card_2), [])
          ~f:(fun (remaining_to_remove_acc, hand_acc) card ->
            match remaining_to_remove_acc with
            | `Both (card_to_remove_1, card_to_remove_2) ->
                if Card.equal card card_to_remove_1 then
                  (`One card_to_remove_2, hand_acc)
                else if Card.equal card card_to_remove_2 then
                  (`One card_to_remove_1, hand_acc)
                else
                  (`Both (card_to_remove_1, card_to_remove_2), card :: hand_acc)
            | `One card_to_remove ->
                if Card.equal card card_to_remove then (`None, hand_acc)
                else (`One card_to_remove, card :: hand_acc)
            | `None -> (`None, card :: hand_acc))
        |> Tuple2.get2
      in
      let new_hand =
        match active_player.hand with
        | Hand.One { hidden; revealed } ->
            let new_cards =
              remove_cards [ card_choice_1; card_choice_2; hidden ]
            in
            Hand.One { hidden = List.hd_exn new_cards; revealed }
        | Hand.Both (card_1, card_2) ->
            let new_cards =
              remove_cards [ card_choice_1; card_choice_2; card_1; card_2 ]
            in
            Hand.Both
              (List.hd_exn new_cards, List.hd_exn (List.tl_exn new_cards))
      in
      let cards_back_in_deck =
        {
          game_state with
          deck = List.permute (returned_card_1 :: returned_card_2 :: rest);
        }
      in
      Game_state.modify_player cards_back_in_deck active_player.id
        ~f:(fun player -> { player with hand = new_hand })
  | _ -> failwith "BUG: Too few cards in the deck"

let take_turn_result game_state =
  let active_player = Game_state.get_active_player game_state in
  let%bind.Deferred.Result action =
    Deferred.repeat_until_finished 2 (fun retries ->
        let%map action =
          Player_io.choose_action active_player.player_io
            ~visible_game_state:
              (Game_state.to_visible_game_state game_state active_player.id)
        in
        match Game_state.is_valid_action game_state active_player.id action with
        | true -> `Finished action
        | false -> (
            (* TODO this should tell client *)
            print_endline "Invalid action";
            match retries with
            (* this does not work in case of coup *)
            | 0 -> `Finished `Income
            | _ -> `Repeat (retries - 1)))
    >>| Result.return
  in
  match (action : Action.t) with
  | `Income ->
      let%map.Deferred.Result () =
        Game_state.players game_state
        |> List.map ~f:(fun player ->
               Player_io.notify_of_action_choice player.player_io
                 active_player.id action)
        |> Deferred.all_unit >>| Result.return
      in
      take_income game_state
  | `Assassinate target_player_id ->
      assassinate game_state active_player.id target_player_id
  | `Foreign_aid -> take_foreign_aid game_state
  | `Coup target_player_id ->
      let%bind.Deferred.Result () =
        Game_state.players game_state
        |> List.map ~f:(fun player ->
               Player_io.notify_of_action_choice player.player_io
                 active_player.id action)
        |> Deferred.all_unit >>| Result.return
      in
      coup game_state target_player_id
  | `Tax -> take_tax game_state
  | `Steal target_player_id -> steal game_state target_player_id
  | `Exchange ->
      let%bind.Deferred.Result () =
        Game_state.players game_state
        |> List.map ~f:(fun player ->
               Player_io.notify_of_action_choice player.player_io
                 active_player.id action)
        |> Deferred.all_unit >>| Result.return
      in
      exchange game_state >>| Result.return

let take_turn game_state =
  (* print_s [%sexp (game_state : Game_state.t)]; *)
  (* print_endline (Game_state.to_string_pretty game_state); *)
  let _ = Game_state.to_string_pretty in
  match%bind take_turn_result game_state with
  | Ok game_state' -> return (`Repeat (Game_state.end_turn game_state'))
  | Error final_game_state -> return (`Finished final_game_state)

let run_game ~game_state =
  let%bind () =
    Game_state.players game_state
    |> List.map ~f:(fun player ->
           Player_io.notify_of_game_start player.player_io
             ~visible_game_state:
               (Game_state.to_visible_game_state game_state player.id))
    |> Deferred.all_unit
  in
  Deferred.repeat_until_finished game_state take_turn
