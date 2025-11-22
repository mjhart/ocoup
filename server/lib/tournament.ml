open! Core
open Async
open Types

type t = { players : (Player_id.t * Player_ios.t) list; max_players : int }

let num_rounds = 5

let create_rounds players =
  let num_players = List.length players in
  let games_per_round = num_players // 6 |> Float.iround_exn ~dir:`Up in
  let max_game_size = num_players / games_per_round in
  let rec create_round = function
    | players when List.length players <= 6 -> [ players ]
    | players ->
        let player_in_game, remaining = List.split_n players max_game_size in
        player_in_game :: create_round remaining
  in
  List.init num_rounds ~f:(fun _i -> create_round (List.permute players))

let score_results (results : Game.Game_state.t Or_error.t list list) =
  let score_game game_state =
    let Game.Game_state.{ eliminated_players; players; _ } = game_state in
    let eliminated_in_order = List.rev eliminated_players in
    (* Score eliminated players: index is their score (0, 1, 2, ...) *)
    let eliminated_scores =
      List.mapi eliminated_in_order ~f:(fun i player ->
          (Game.Player.(player.id), i))
    in
    (* Winner gets points equal to num_eliminated + 1 bonus point *)
    let winner_scores =
      List.map players ~f:(fun player ->
          let num_eliminated = List.length eliminated_players in
          (Game.Player.(player.id), num_eliminated + 1))
    in
    eliminated_scores @ winner_scores
  in
  List.concat_map results ~f:(fun round ->
      List.concat_map round ~f:(fun game_result ->
          let game_state = Or_error.ok_exn game_result in
          score_game game_state))
  |> List.fold ~init:Player_id.Map.empty ~f:(fun acc (player_id, score) ->
         Map.update acc player_id ~f:(function
           | None -> score
           | Some existing -> existing + score))

let create ~max_players = { players = []; max_players }

let register (state : t) player_io =
  let { players; max_players } = state in
  let num_players = List.length players in
  if num_players < max_players then
    let player_id = Types.Player_id.of_int num_players in
    Ok { players = (player_id, player_io) :: players; max_players }
  else error_s [%message "Game is full"]

let start (state : t) =
  let { players; max_players = _ } = state in

  let rounds = create_rounds players in
  let%map results =
    (* Run rounds sequentially *)
    Deferred.List.map ~how:`Sequential rounds ~f:(fun round ->
        (* Run each game in round in parallel *)
        Deferred.List.map ~how:`Parallel round ~f:(fun players_in_game ->
            let%bind game_state =
              Game.Game_state.init'
                (List.map players_in_game ~f:(fun (player_id, player_io) ->
                     (player_id, fun () -> return player_io)))
              >>| Or_error.ok_exn
              (* This is fine - only fails if given too few players *)
            in
            Deferred.Or_error.try_with (fun () -> Game.run_game ~game_state)))
  in
  results
