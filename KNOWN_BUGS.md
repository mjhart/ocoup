# Known Bugs

Findings from a code review of the server on 2026-09-09. The four most severe
game-logic bugs (dropped 15th card, unchallengeable Exchange, skipped turn after
self-elimination, missing assassination refund) were fixed in commit `71d9315`.
The remaining findings are listed below, most severe first. Line numbers refer
to the state of the tree at the time of writing.

## Game logic

### 1. Exchange does not validate the returned cards

`exchange` in `server/lib/game.ml:500` trusts that the two cards a player
returns came from the offered set (two drawn cards plus their hand).
`remove_cards` removes at most one matching card per returned card, so:

- A WebSocket client that sends card names not in the offered set removes
  nothing. The player keeps their original cards, and the bogus cards are
  added to the deck. Cards are invented.
- An LLM or CLI player that returns the same index twice (e.g. `[0, 0]`)
  removes only one card. One offered card vanishes and the duplicated card is
  added to the deck twice.

Either way the 15-card invariant is broken for the rest of the game.

**Fix:** validate the returned pair as a sub-multiset of the offered cards, and
fall back to returning the two drawn cards on invalid input.

### 2. Forced coup can be dodged with an invalid target

`is_valid_action` in `server/lib/game.ml:96`: when a player has 10+ coins and
submits `Coup` with an invalid target (self or a non-existent player),
`coup_if_required` is true, so control falls to the second branch and the
default action becomes `Income`. After the retries are exhausted the player
takes income with 10+ coins, violating the must-coup rule.

**Fix:** when coins are 10+, the default should always be a coup against a
valid target regardless of which check failed.

## Server and infrastructure

### 3. Tournament start crashes if any game errored or too few players registered

`Tournament.score_results` in `server/lib/tournament.ml:21` calls
`Or_error.ok_exn` on every game result. A single failed game raises out of the
`Start_tournament` handler before the `Error` case in the protocol mapping in
`ocoup.ml` is ever reached, so the client gets a dropped connection instead of
a results payload with one errored game.

Related edge cases in `create_rounds` (`server/lib/tournament.ml:9`):

- Zero registered players: `games_per_round` is 0, so `max_game_size`
  divides by zero.
- One registered player: `Game_state.init'` fails and the `ok_exn` in
  `Tournament.start` raises outside the `try_with`.

**Fix:** score only `Ok` games, and reject `start` with a 400 when fewer than
two players are registered.

### 4. Tournament registration race loses players

`tournament_register_ws_handler` in `server/lib/ocoup.ml:237` reads
`tournament_data` from the table when the HTTP request arrives, but the
registration happens inside the WebSocket callback after the handshake
completes. Two clients whose requests overlap both see the same snapshot,
compute the same `player_id`, and the second `Hashtbl.set` overwrites the
first registration. The `started` check uses the same stale snapshot.

**Fix:** re-read the tournament from `state.tournaments` inside the callback,
immediately before registering, and update it in the same synchronous step.

### 5. Untrusted client input can kill a game

- `Websocket_player_io.create` (`server/lib/websocket_player_io.ml:11`)
  parses incoming frames with `Yojson.Safe.from_string` inside `don't_wait_for`.
  Malformed JSON raises and permanently ends that player's read loop. Every
  later prompt for that player times out to the default action.
- `Protocol.Choose_cards_to_return.response_of_yojson`
  (`server/lib/protocol.ml:213`) and `Card.of_string` use `failwith`. The
  exception propagates into the game monitor and aborts the game for all
  players.

**Fix:** catch parse errors in the read loop and log them, and make every
`response_of_yojson` total with a safe default (as the other decoders already
do).

### 6. Late responses are misattributed to the next question

The protocol has no request ids. After the 60-second timeout in
`Player_ios.with_timeout_default` (`server/lib/player_ios.ml:10`) fires, the
next query installs a fresh ivar in `Websocket_player_io`. If the client's
late reply to the previous question arrives after that, it is parsed as the
answer to the new question. Since decoders default on mismatch this usually
degrades to `Income`/`Allow`/`No_challenge`, but it can also select a wrong
card in `Reveal_card`.

**Fix:** include a request id in each query and ignore responses that don't
match the outstanding id.

### 7. Spectator pipe can stall the player's game

`player_with_updates_ws_handler` in `server/lib/ocoup.ml:317` forks the
outgoing message pipe with ``~pushback_uses:`Both_consumers``. Writes to the
player block until the updates pipe has been drained by a spectator. If no
spectator connects to `/games/{id}/updates`, the first `notify_of_game_start`
write likely never completes and the game never starts. If a spectator
disconnects mid-game, behaviour depends on how the fork handles a closed
downstream reader. Not verified end to end.

**Fix:** use ``~pushback_uses:`Fast_consumer`` or write to the updates pipe
with `Pipe.write_without_pushback`.

## Smaller issues

- **No game-end notification.** The eliminated player is never told they lost
  their final influence (they are removed from `players` before the
  `notify_of_lost_influence` fan-out in `lose_influence`), and no player is
  told the game has ended. See the TODO in `server/lib/types.mli:152`.
- **Challenge notification omits the revealed card.** When a challenge fails,
  the rules say the card is shown to everyone. `notify_of_challenge` only
  carries `has_required_card`, and the CLI and LLM implementations phrase it
  as "challenged you" for every recipient, including bystanders.
- **JSON decoders depend on field order.** `Protocol.Action.t_of_yojson`
  (`server/lib/protocol.ml:38`) and the other decoders pattern-match on exact
  association-list order, so `{"player_id":1,"type":"Coup"}` silently decodes
  as `Income`. Use `List.Assoc.find` as `Create_game_request.of_yojson` does.
- **Games are never removed from the server state.** `State.add_game`
  (`server/lib/ocoup.ml:61`) inserts into `state.games` and nothing ever
  deletes, so the table and its pipes grow for the life of the process.
- **Handler-level JSON parse failures.** `Create_game.handle` and
  `Create_tournament.handle` call `Yojson.Safe.from_string` on the raw body;
  a malformed body raises and the client gets a dropped connection rather
  than a 400.
- **`handle_response_race` leaks pending deferreds.** Noted by the existing
  TODO at the top of the function. When one player blocks or challenges,
  the other players' pending `offer_challenge` deferreds are never resolved
  for LLM players, so the in-flight HTTP requests continue and their results
  are discarded.
