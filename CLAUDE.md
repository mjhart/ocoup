# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

OCaml implementation of the board game **Coup** with support for CLI, LLM-based players, and WebSocket-based multiplayer. The game includes a tournament system for running multiple rounds with various player configurations.

## Build & Development Commands

### Building
```bash
dune build
```

### Running Tests
```bash
# Run all tests
dune runtest

# Run tests in watch mode
dune runtest --watch

# Run a specific test file
dune runtest test/test_ocoup.ml
```

### Formatting
```bash
# Format all OCaml files
dune build @fmt --auto-promote
```

### Running the Game
```bash
# Run game with specific player types (CLI or LLM-based)
dune exec ocoup run cli cli

# Run with LLM players
dune exec ocoup run gpt-4o o3-mini

# Run WebSocket server (default port 8080)
dune exec ocoup server

# Run server on custom port
dune exec ocoup server --port 9000
```

## Architecture

### Core Module Structure

The codebase follows a layered architecture:

**Types Layer (`lib/types.mli/ml`)**
- Defines all core game types: `Card`, `Player_id`, `Action`, `Hand`, `Visible_game_state`
- `Player_io_S` signature: interface that all player implementations must satisfy
- Separates challengeable vs non-challengeable actions for game rule enforcement

**Game Layer (`lib/game.mli/ml`)**
- `Game_state`: maintains full game state (players, deck, coins)
- `take_turn`: executes a single turn with challenge/block mechanics
- `run_game`: main game loop that continues until one player remains
- **Key insight**: The first player in `Game_state.players` list is the active player; `end_turn` rotates this list

**Player IO Layer (`lib/player_ios.ml`)**
- Multiple player implementations via `Player_io` polymorphic wrapper:
  - `Cli_player_io`: Terminal-based human player with colored output
  - `Llm_player_io`: LLM-based players using OpenAI API (GPT-4o, O3-mini)
  - `Gemini_player_io`: Google Gemini-based players
  - `Websocket_player_io`: Network players via WebSocket protocol
- All implementations conform to `Player_io_S` signature from `types.mli`
- Player IO uses `Visible_game_state` - players only see their own cards and others' revealed cards

**Tournament Layer (`lib/tournament.mli/ml`)**
- `create_rounds`: Distributes players into multiple games across multiple rounds
- Runs rounds sequentially, games within rounds in parallel
- Supports variable player counts (splits into games of up to 6 players)

**Server Layer (`lib/ocoup.ml`)**
- WebSocket server for multiplayer games
- Endpoints:
  - `POST /games` - Create new game
  - `GET /games/{id}/player` - Join as player
  - `GET /games/{id}/updates` - Spectate game updates
  - `GET /new_game` - Quick start with default players

### Game Rules Implementation

**Challenge System**
- When a player claims a character action, others can challenge
- Challenge resolution handled in `game.ml` via `handle_challenge`
- If challenged successfully: challenger wins, claimer loses influence
- If challenge fails: challenger loses influence, claimer gets new card

**Block System**
- Certain actions can be blocked by character claims:
  - Foreign Aid → Duke
  - Assassination → Contessa
  - Steal → Captain or Ambassador
- Blocks can themselves be challenged

**Action Validation**
- `is_valid_action` in `game.ml` enforces:
  - Sufficient coins for paid actions
  - Valid target player (exists, not self)
  - Mandatory coup at 10+ coins

### Testing Architecture

**Test Helpers (`test/test_helpers.ml`)**
- `run_test`: Orchestrates test games with scripted player responses
- `Test_player_io`: Mock player IO that dequeues predetermined responses
- `Default_action_player_io`: Simple AI for integration tests (coups at 7 coins, else takes income)

**Testing Pattern**
Tests use inline `%expect_test` with explicit move sequences:
```ocaml
run_test
  ~starting_cards:[(Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador)]
  [
    (0, Response.Choose_action `Income);
    (1, Choose_action `Tax);
    (0, Offer_challenge `No_challenge);
  ]
```

Each tuple is `(player_index, response)` - tests fail if players go out of turn.

### Async Patterns

This codebase uses **Jane Street's Async** library throughout:
- All IO operations return `Deferred.t`
- Use `let%bind` and `let%map` for monadic operations
- `Pipe` for streaming data between WebSocket connections
- Sequential vs parallel execution controlled via `Deferred.List.map ~how:`

### Key Design Patterns

**Polymorphic Player IO**
- `Player_io.t` wraps any module implementing `Player_io_S`
- Enables mixing different player types in same game
- Factory functions in `ocoup.ml`: `Player_io.cli`, `Player_io.llm ~model:...`

**Visible vs Hidden State**
- `Visible_game_state`: what each player sees (their hand + others' revealed cards)
- `Game_state`: complete hidden state including deck and all hands
- `to_visible_game_state` converts for specific player's perspective

**Turn Structure**
- Active player chooses action
- Other players can respond (challenge/block) via `cancelled_reason` deferred
- Action resolves based on responses
- Turn ends and player list rotates

## Code Conventions

- All files use `open! Core` and `open! Async` for Jane Street libraries
- PPX extensions: `ppx_jane`, `ppx_yojson_conv` for JSON serialization
- Sexp everywhere for debugging: `[@@deriving sexp]` and `print_s`
- String interpolation: `[%string "Player %{id#Player_id}"]`
- Module signatures in `.mli` files, implementations in `.ml`

## Dependencies

Main libraries (see `dune-project`):
- `async` - Asynchronous programming
- `async_ssl` - SSL/TLS support
- `core` - Jane Street standard library replacement
- `cohttp-async` - HTTP client/server
- `cohttp_async_websocket` - WebSocket support
- `yojson` - JSON parsing
- `ppx_jane` - Common Jane Street PPX rewriters
- `ppx_yojson_conv` - JSON conversion derivers
