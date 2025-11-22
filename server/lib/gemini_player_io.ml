open! Core
open! Async
open Types

type role = Model | User

let gemini_2_5_pro_exp_03_25 = "gemini-2.5-pro-exp-03-25"
let role_to_string = function Model -> "model" | User -> "user"

type t = {
  events : (role * string) Queue.t;
  player_id : Player_id.t;
  writer : Writer.t;
  model : string;
}

let create player_id ~model =
  let events = Queue.create () in
  let%map writer =
    Writer.open_file [%string "player_%{player_id#Player_id}.txt"]
  in
  { events; player_id; writer; model }

let print_endline t message =
  ignore t.player_id;
  Writer.write_line t.writer message;
  ()

let headers =
  Lazy.from_fun (fun () ->
      Cohttp.Header.of_list [ ("Content-Type", "application/json") ])

let send_request t prompt =
  let open Cohttp_async in
  let events = Queue.to_list t.events @ [ (User, prompt) ] in
  events
  |> List.iter ~f:(fun (role, content) ->
         print_endline t
           [%string "Role: %{role_to_string role} Content: %{content}"]);
  let messages =
    events
    |> List.map ~f:(fun (role, content) ->
           `Assoc
             [
               ("role", `String (role_to_string role));
               ("parts", `List [ `Assoc [ ("text", `String content) ] ]);
             ])
  in
  let post_body =
    let json =
      `Assoc
        [
          ( "system_instruction",
            `Assoc
              [ ("parts", `List [ `Assoc [ ("text", `String Rules.rules) ] ]) ]
          );
          ("contents", `List messages);
          ( "generationConfig",
            `Assoc [ ("response_mime_type", `String "application/json") ] );
        ]
    in
    Body.of_string (Yojson.Basic.to_string json)
  in
  let%bind _response, body =
    let url =
      Uri.of_string
        [%string
          "https://generativelanguage.googleapis.com/v1beta/models/%{t.model}:generateContent"]
    in
    let url =
      Uri.add_query_param url
        ("key", [ Sys.getenv "GEMINI_API_KEY" |> Option.value_exn ])
    in

    Client.post url ~headers:(Lazy.force headers) ~body:post_body
  in
  let%map body = Body.to_string body in
  print_endline t body;
  let json = Yojson.Basic.from_string body in
  let content_string =
    Yojson.Basic.Util.member "candidates" json
    |> Yojson.Basic.Util.to_list |> List.hd_exn
    |> Yojson.Basic.Util.member "content"
    |> Yojson.Basic.Util.member "parts"
    |> Yojson.Basic.Util.to_list |> List.hd_exn
    |> Yojson.Basic.Util.member "text"
    |> function
    | `String content -> content
    | _ -> failwith "Invalid content"
  in
  Queue.enqueue t.events (User, prompt);
  Queue.enqueue t.events (Model, content_string);
  Yojson.Basic.from_string content_string

(* TODO: send initial data here *)
let notify_of_game_start t ~visible_game_state =
  (match visible_game_state with
  | {
   Visible_game_state.hand = Hand.Both (card_1, card_2);
   other_players;
   coins = _;
   active_player_id = _;
  } ->
      let other_players_string =
        other_players
        |> List.map ~f:(fun { Visible_game_state.Other_player.player_id; _ } ->
               [%string "Player id: %{player_id#Player_id}"])
        |> String.concat ~sep:", "
      in
      Queue.enqueue t.events
        ( User,
          [%string
            "The game is starting. You are player %{t.player_id#Player_id}. \
             Your starting cards are %{card_1#Card} and %{card_2#Card}. \
             Players %{other_players_string} are your opponents."] )
  | _ -> failwith "Expected player to have two cards at game start");
  return ()

let choose_action t ~visible_game_state =
  let visible_game_state_string =
    Visible_game_state.to_string_pretty visible_game_state
  in
  let prompt =
    [%string
      "%{visible_game_state_string}\n\
       Choose action: Income | Foreign_aid | Assassinate player_id | Coup \
       target_player_id | Tax | Steal target_player_id | Exchange. Respond \
       with a json object with the key 'reasoning' containing the reasoning \
       behind your choice as a string, and the key 'action' containing the \
       action you want to take. If the action requires a target player, \
       additionally provide the key 'target_player_id' and the value will be \
       the the player_id of the target player."]
  in

  let%map response = send_request t prompt in
  let action = Yojson.Basic.Util.member "action" response in
  let target_player_id = Yojson.Basic.Util.member "target_player_id" response in
  match (action, target_player_id) with
  | `String "Income", `Null -> `Income
  | `String "Foreign_aid", `Null -> `Foreign_aid
  | `String "Assassinate", `Int target_player_id ->
      `Assassinate (Player_id.of_int target_player_id)
  | `String "Coup", `Int target_player_id ->
      `Coup (Player_id.of_int target_player_id)
  | `String "Tax", `Null -> `Tax
  | `String "Steal", `Int target_player_id ->
      `Steal (Player_id.of_int target_player_id)
  | `String "Exchange", `Null -> `Exchange
  | _ ->
      print_endline t "Invalid action";
      `Income

let visible_game_state_prompt visible_game_state =
  String.concat_lines
    [
      "Game state, after the active player has paid for their action:";
      Visible_game_state.to_string_pretty visible_game_state;
    ]

let choose_assasination_response t ~visible_game_state ~asassinating_player_id =
  let visible_game_state_string =
    visible_game_state_prompt visible_game_state
  in
  let prompt =
    [%string
      "%{visible_game_state_string}\n\
       You are being assassinated by player \
       %{asassinating_player_id#Player_id}. Respond with a json object with \
       the key 'reasoning' containing the reasoning behind your choice as a \
       string, and the key 'response' containing 'Allow' or 'Block'."]
  in
  let%map response = send_request t prompt in
  let response = Yojson.Basic.Util.member "response" response in
  match response with
  | `String "Allow" -> `Allow
  | `String "Block" -> `Block
  | _ ->
      print_endline t "Invalid response";
      `Allow

let choose_foreign_aid_response t ~visible_game_state () ~cancelled_reason:_ =
  let visible_game_state_string =
    Visible_game_state.to_string_pretty visible_game_state
  in
  let prompt =
    [%string
      "%{visible_game_state_string}\n\
       Player is attempting to take foreign aid. Respond with a json object \
       with the key 'response' and the value being 'Allow' or 'Block'. Provide \
       the reasoning behind your choice as a string in the key 'reasoning'."]
  in
  let%map response = send_request t prompt in
  let response = Yojson.Basic.Util.member "response" response in
  match response with
  | `String "Allow" -> `Allow
  | `String "Block" -> `Block
  | _ ->
      print_endline t "Invalid response";
      `Allow

let choose_steal_response t ~visible_game_state ~stealing_player_id =
  let visible_game_state_string =
    Visible_game_state.to_string_pretty visible_game_state
  in
  let prompt =
    [%string
      "%{visible_game_state_string}\n\
       Player %{stealing_player_id#Player_id} is attempting to steal from you. \
       Respond with a json object with the key 'reasoning' containing the \
       reasoning behind your choice as a string, and the key 'response' \
       containing 'Allow' or 'Block'. If you choose to block, additionally \
       provide the card you are blocking with in the key 'card' with the value \
       being 'Ambassador' or 'Captain'."]
  in
  let%map response = send_request t prompt in
  let action = Yojson.Basic.Util.member "response" response in
  let card = Yojson.Basic.Util.member "card" response in
  match (action, card) with
  | `String "Allow", `Null -> `Allow
  | `String "Block", `String "Ambassador" -> `Block `Ambassador
  | `String "Block", `String "Captain" -> `Block `Captain
  | _ ->
      print_endline t "Invalid response";
      `Allow

let choose_cards_to_return t ~visible_game_state card_1 card_2 hand =
  let visible_game_state_string =
    Visible_game_state.to_string_pretty visible_game_state
  in
  let cards =
    let cards_in_hand =
      match hand with
      | Hand.Both (card_1, card_2) -> [ card_1; card_2 ]
      | Hand.One { hidden; revealed = _ } -> [ hidden ]
    in
    [ card_1; card_2 ] @ cards_in_hand
  in
  let cards_string =
    cards |> List.map ~f:Card.to_string |> String.concat ~sep:", "
  in
  let prompt =
    [%string
      "%{visible_game_state_string}\n\
       You are exchanging cards. The cards available to you are \
       [%{cards_string}]. Choose two cards to return to the deck. Respond with \
       a json object with the key 'reasoning' containing the reasoning behind \
       your choice as a string, and the key 'response' containing a json array \
       of size exactly 2 containing the indices of the cards you want to \
       return. The indices are 0-indexed."]
  in
  let%map response = send_request t prompt in
  let indices = Yojson.Basic.Util.member "response" response in
  match Yojson.Basic.Util.to_list indices with
  | [ `Int index_1; `Int index_2 ] ->
      (List.nth_exn cards index_1, List.nth_exn cards index_2)
  | _ ->
      print_endline t "Invalid response";
      (card_1, card_2)

let reveal_card t ~visible_game_state ~card_1 ~card_2 =
  let visible_game_state_string =
    Visible_game_state.to_string_pretty visible_game_state
  in
  let cards = [ card_1; card_2 ] in
  let cards_string =
    cards |> List.map ~f:Card.to_string |> String.concat ~sep:" "
  in
  let prompt =
    [%string
      "%{visible_game_state_string}\n\
       You have lost influence and must reveal a card. The cards available to \
       you are %{cards_string}. Which card do you want to reveal? Respond with \
       a json object with the key 'reasoning' containing the reasoning behind \
       your choice as a string, and the key 'response' containing the name of \
       the card you want to reveal, exactly as it appears in the list of \
       cards."]
  in
  let%map response = send_request t prompt in
  let response =
    Yojson.Basic.Util.member "response" response |> Yojson.Basic.Util.to_string
  in
  let n =
    List.findi cards ~f:(fun _ card ->
        String.equal (Card.to_string card) response)
  in
  match n with
  | Some (0, _) -> `Card_1
  | Some (1, _) -> `Card_2
  | _ ->
      print_endline t "Invalid response";
      `Card_1

let offer_challenge t ~visible_game_state acting_player_id challengable
    ~cancelled_reason:_ =
  let visible_game_state_string =
    Visible_game_state.to_string_pretty visible_game_state
  in
  let prompt =
    [%string
      "%{visible_game_state_string}\n\
       Player %{acting_player_id#Player_id} is attempting to perform \
       %{challengable#Challengable}. Respond with a json object with the key \
       'reasoning' containing the reasoning behind your choice as a string, \
       and the key 'response' containing 'No_challenge' or 'Challenge'."]
  in
  let%map response = send_request t prompt in
  let response = Yojson.Basic.Util.member "response" response in
  match response with
  | `String "No_challenge" -> `No_challenge
  | `String "Challenge" -> `Challenge
  | _ ->
      print_endline t "Invalid response";
      `No_challenge

let notify_of_action_choice t player_id action =
  Queue.enqueue t.events
    ( User,
      [%string "Player %{player_id#Player_id} chose action %{action#Action}"] );
  return ()

let notify_of_lost_influence t player_id card =
  Queue.enqueue t.events
    ( User,
      [%string "Player %{player_id#Player_id} lost influence card %{card#Card}"]
    );
  return ()

let notify_of_new_card t card =
  Queue.enqueue t.events (User, [%string "Received new card %{card#Card}"]);
  return ()

let notify_of_challenge t ~challenging_player_id ~has_required_card =
  let success =
    match has_required_card with
    | true -> "unsuccessfully"
    | false -> "successfully"
  in
  Queue.enqueue t.events
    ( User,
      [%string
        "Player %{challenging_player_id#Player_id} challenged you %{success}"]
    );
  return ()
