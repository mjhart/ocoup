open! Core
open Async
open Ocoup.For_testing.Tournament

let%expect_test "test create rounds" =
  Log.Global.set_output [];
  let players = List.init 11 ~f:Fn.id in
  let rounds = create_rounds players in
  List.iteri rounds ~f:(fun i round ->
      print_endline [%string "Round: %{i#Int}"];
      print_s [%sexp (round : int list list)]);
  [%expect
    {|
    Round: 0
    ((2 8 4 1 0) (6 9 7 5 3 10))
    Round: 1
    ((4 7 5 1 2) (0 9 6 3 10 8))
    Round: 2
    ((5 7 4 9 3) (2 6 8 0 10 1))
    Round: 3
    ((0 10 2 3 7) (8 5 4 1 6 9))
    Round: 4
    ((3 8 2 1 10) (7 5 6 9 4 0))
    |}];
  return ()

let%expect_test "tourament" =
  Log.Global.set_output [];
  let tourament = create ~max_players:7 in
  let tourament_with_players =
    List.fold (List.range 0 7) ~init:tourament ~f:(fun tournament_acc _i ->
        let player_io =
          Ocoup.For_testing.Player_ios.create
            (module Test_helpers.Default_action_player_io)
            ()
        in
        register tournament_acc player_io |> ok_exn)
  in
  let%bind result = start tourament_with_players in
  List.iteri result ~f:(fun i round ->
      print_endline [%string "Round %{i#Int}"];
      List.iteri round ~f:(fun j game ->
          let Ocoup.For_testing.Game.Player.{ id = winner; _ } =
            ok_exn game |> Ocoup.For_testing.Game.Game_state.players
            |> List.hd_exn
          in
          print_endline
            [%string
              "Game %{j#Int} winner: \
               %{winner#Ocoup.For_testing.Types.Player_id}"]));
  [%expect
    {|
    Round 0
    Game 0 winner: 2
    Game 1 winner: 5
    Round 1
    Game 0 winner: 2
    Game 1 winner: 3
    Round 2
    Game 0 winner: 0
    Game 1 winner: 5
    Round 3
    Game 0 winner: 4
    Game 1 winner: 6
    Round 4
    Game 0 winner: 3
    Game 1 winner: 0
    |}];
  let scores = score_results result in
  print_s [%sexp (scores : int Ocoup.For_testing.Types.Player_id.Map.t)];
  [%expect {| ((0 12) (1 3) (2 10) (3 9) (4 5) (5 9) (6 7)) |}];
  return ()
