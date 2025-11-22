open! Core
open Async
open Types

type t = {
  players : (Player_id.t * Player_ios.Player_io.t) list;
  max_players : int;
}

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

let score_results (_results : (Game.Game_state.t, exn) result list list) =
  (* TODO don't know the order in which players were eliminated. Maybe tail the game events? *)
  (* TODO need to correlate player_ids to some stable id for players *)
  0

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
