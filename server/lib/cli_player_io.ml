open! Core
open! Async
open Types

let unexpected_eof () = failwith "EOF"

let stdin_throttle =
  Lazy.from_fun (fun () ->
      Throttle.Sequencer.create ~continue_on_error:false ())

type t = { player_id : Player_id.t; writer : Writer.t }

let cli_player player_id =
  (* open a file for writing *)
  let%map writer =
    Writer.open_file [%string "player_%{player_id#Player_id}.txt"]
  in
  { player_id; writer }

let color_code t =
  let i = 34 + (Player_id.to_int t.player_id mod 6) in
  sprintf "\027[%dm" i

let print_endline t message =
  Writer.write_line t.writer message;
  print_string (color_code t);
  print_endline message;
  print_string "\027[0m";
  ()

let _print_s t message =
  Writer.write_sexp t.writer message;
  Async_unix.print_string (color_code t);
  print_s message;
  Async_unix.print_string "\027[0m";
  ()

let with_stdin t ~f =
  Throttle.enqueue (Lazy.force stdin_throttle) (fun () ->
      print_endline t
        (sprintf "You are player %d" (Player_id.to_int t.player_id));
      f (Lazy.force Reader.stdin))

let print_visible_game_state t visible_game_state =
  let visible_game_state_string =
    Visible_game_state.to_string_pretty visible_game_state
  in
  print_endline t visible_game_state_string

let choose_action t ~visible_game_state =
  with_stdin t ~f:(fun stdin ->
      print_visible_game_state t visible_game_state;
      print_endline t "Choose action (I/FA/A n/C n/T/S n/E)";
      match%bind Reader.read_line stdin with
      | `Eof -> unexpected_eof ()
      | `Ok action_str -> (
          match String.split action_str ~on:' ' with
          | [ "I" ] -> return `Income
          | [ "FA" ] -> return `Foreign_aid
          | [ "A"; n ] ->
              return (`Assassinate (Player_id.of_int (Int.of_string n)))
          | [ "C"; n ] -> return (`Coup (Player_id.of_int (Int.of_string n)))
          | [ "T" ] -> return `Tax
          | [ "S"; n ] -> return (`Steal (Player_id.of_int (Int.of_string n)))
          | [ "E" ] -> return `Exchange
          | _ ->
              print_endline t "Invalid action";
              return `Income))

let choose_assasination_response t ~visible_game_state ~asassinating_player_id =
  with_stdin t ~f:(fun stdin ->
      print_visible_game_state t visible_game_state;
      print_endline t
        [%string
          "Player %{asassinating_player_id#Player_id} is attempting to \
           assassinate you. Block assassination? (Y/N)"];
      match%map Reader.read_line stdin with
      | `Eof -> unexpected_eof ()
      | `Ok action_str -> (
          match String.split action_str ~on:' ' with
          | [ "N" ] -> `Allow
          | [ "Y" ] -> `Block
          | _ ->
              print_endline t "Invalid action";
              `Allow))

let choose_steal_response t ~visible_game_state ~stealing_player_id =
  with_stdin t ~f:(fun stdin ->
      print_visible_game_state t visible_game_state;
      print_endline t
        [%string
          "Player %{stealing_player_id#Player_id} is stealing from you. Block \
           steal? (C/A/N)"];
      match%map Reader.read_line stdin with
      | `Eof -> unexpected_eof ()
      | `Ok action_str -> (
          match String.split action_str ~on:' ' with
          | [ "C" ] -> `Block `Captain
          | [ "A" ] -> `Block `Ambassador
          | [ "N" ] -> `Allow
          | _ ->
              print_endline t "Invalid action";
              `Allow))

let choose_foreign_aid_response t ~visible_game_state () ~cancelled_reason =
  upon cancelled_reason (function
      | Cancelled_reason.Other_player_responded player_id ->
      print_endline t
        [%string
          "Player %{player_id#Player_id} has already blocked the foreign aid."]);
  with_stdin t ~f:(fun stdin ->
      match Deferred.is_determined cancelled_reason with
      | true -> return `Allow
      | false -> (
          print_visible_game_state t visible_game_state;
          print_endline t "Block foreign aid? (Y/N)";
          match%map Reader.read_line stdin with
          | `Eof -> unexpected_eof ()
          | `Ok action_str -> (
              match String.split action_str ~on:' ' with
              | [ "Y" ] -> `Block
              | [ "N" ] -> `Allow
              | _ ->
                  print_endline t "Invalid action";
                  `Allow)))

let choose_cards_to_return t ~visible_game_state card_1 card_2 hand =
  let cards =
    let cards_in_hand =
      match hand with
      | Hand.Both (card_1, card_2) -> [ card_1; card_2 ]
      | Hand.One { hidden; revealed = _ } -> [ hidden ]
    in
    [ card_1; card_2 ] @ cards_in_hand
  in
  let cards_string =
    cards
    |> List.mapi ~f:(fun i card -> [%string "%{i#Int}: %{card#Card}"])
    |> String.concat ~sep:" "
  in
  with_stdin t ~f:(fun stdin ->
      print_visible_game_state t visible_game_state;
      print_endline t
        [%string
          "Cards to choose from:\n\
           %{cards_string}\n\
           Choose 2 cards to return by entering the indices of the cards you \
           want to return, separated by a space and 0-indexed."];
      match%map Reader.read_line stdin with
      | `Eof -> unexpected_eof ()
      | `Ok action_str -> (
          match String.split action_str ~on:' ' with
          | [ i1; i2 ] ->
              ( List.nth_exn cards (Int.of_string i1),
                List.nth_exn cards (Int.of_string i2) )
          | _ ->
              print_endline t "Invalid action";
              (card_1, card_2)))

let reveal_card t ~visible_game_state ~card_1 ~card_2 =
  let cards_string =
    [ card_1; card_2 ]
    |> List.mapi ~f:(fun i card -> [%string "%{i#Int}: %{card#Card}"])
    |> String.concat ~sep:"\n"
  in
  with_stdin t ~f:(fun stdin ->
      print_visible_game_state t visible_game_state;
      print_endline t
        [%string
          "Cards to choose from:\n\
           %{cards_string}\n\
           Which card do you want to reveal? (0/1)"];
      match%map Reader.read_line stdin with
      | `Eof -> unexpected_eof ()
      | `Ok action_str -> (
          match String.split action_str ~on:' ' with
          | [ "0" ] -> `Card_1
          | [ "1" ] -> `Card_2
          | _ ->
              print_endline t "Invalid action";
              `Card_1))

let offer_challenge t ~visible_game_state acting_player_id challengable
    ~cancelled_reason =
  upon cancelled_reason (function
      | Cancelled_reason.Other_player_responded player_id ->
      print_endline t
        [%string "Player %{player_id#Player_id} has challenged the action."]);
  with_stdin t ~f:(fun stdin ->
      match Deferred.is_determined cancelled_reason with
      | true -> return `No_challenge
      | false -> (
          print_visible_game_state t visible_game_state;
          print_endline t
            [%string
              "Player %{acting_player_id#Player_id} is attempting to perform \
               %{challengable#Challengable}. Challenge? (Y/N)"];
          match%map Reader.read_line stdin with
          | `Eof -> unexpected_eof ()
          | `Ok action_str -> (
              match String.split action_str ~on:' ' with
              | [ "N" ] -> `No_challenge
              | [ "Y" ] -> `Challenge
              | _ ->
                  print_endline t "Invalid action";
                  `No_challenge)))

let notify_of_action_choice t player_id action =
  print_endline t
    [%string "Player %{player_id#Player_id} chose action %{action#Action}"]
  |> return

let notify_of_new_card t card =
  print_endline t [%string "Got new card %{card#Card}"] |> return

let notify_of_lost_influence t player_id card =
  print_endline t
    [%string "Player %{player_id#Player_id} lost influence %{card#Card}"]
  |> return

let notify_of_challenge t ~challenging_player_id ~has_required_card =
  let success =
    match has_required_card with
    | true -> "unsuccessfully"
    | false -> "successfully"
  in
  print_endline t
    [%string
      "Player %{challenging_player_id#Player_id} challenged you %{success}"]
  |> return

let notify_of_game_start _t ~visible_game_state:_ = return ()
