#!/usr/bin/env node

/**
 * OCoup Tournament CLI
 *
 * Usage:
 *   Tournament mode:
 *     tournament-cli.js [num_human_players] [server_url] [bot_player_types...]
 *
 *   Direct game mode:
 *     tournament-cli.js --url <websocket_url>
 *
 * Arguments (Tournament mode):
 *   num_human_players  - Number of human players to register (default: 4)
 *   server_url         - Server URL (default: http://localhost:9000)
 *   bot_player_types   - Bot player types to pre-register (optional)
 *
 * Arguments (Direct mode):
 *   --url <url>        - Connect directly to a game WebSocket URL
 *
 * Supported bot types:
 *   - gpt-5-mini           - OpenAI gpt-5-mini
 *   - o3-mini          - OpenAI O3-mini
 *   - gemini-2-5       - Google Gemini 2.5
 *   - cli              - Command-line player (not recommended for tournaments)
 *
 * Examples:
 *   # 4 human players (tournament mode)
 *   ./tournament-cli.js 4
 *
 *   # 2 human + 2 bot players
 *   ./tournament-cli.js 2 http://localhost:9000 gpt-5-mini o3-mini
 *
 *   # 3 bot players only
 *   ./tournament-cli.js 0 http://localhost:9000 gpt-5-mini o3-mini gemini-2-5
 *
 *   # Custom server with mixed players
 *   ./tournament-cli.js 1 https://example.com gpt-5-mini gpt-5-mini o3-mini
 *
 *   # Direct connection to a game URL
 *   ./tournament-cli.js --url ws://localhost:9000/games/abc123/player
 */

import WebSocket from 'ws';

// Parse command line arguments
const args = process.argv.slice(2);

// Handle help flag
if (args.includes('--help') || args.includes('-h')) {
  console.log(`
🎮 OCoup Tournament CLI

Usage:
  Tournament mode:
    tournament-cli.js [num_human_players] [server_url] [bot_player_types...]

  Direct game mode:
    tournament-cli.js --url <websocket_url>

Arguments (Tournament mode):
  num_human_players  - Number of human players to register (default: 4)
  server_url         - Server URL (default: http://localhost:9000)
  bot_player_types   - Bot player types to pre-register (optional)

Arguments (Direct mode):
  --url <url>        - Connect directly to a game WebSocket URL

Supported bot types:
  - gpt-5-mini           - OpenAI gpt-5-mini
  - o3-mini          - OpenAI O3-mini
  - gemini-2-5       - Google Gemini 2.5
  - cli              - Command-line player (not recommended for tournaments)

Examples:
  # 4 human players (tournament mode)
  ./tournament-cli.js 4

  # 2 human + 2 bot players
  ./tournament-cli.js 2 http://localhost:9000 gpt-5-mini o3-mini

  # 3 bot players only
  ./tournament-cli.js 0 http://localhost:9000 gpt-5-mini o3-mini gemini-2-5

  # Custom server with mixed players
  ./tournament-cli.js 1 https://example.com gpt-5-mini gpt-5-mini o3-mini

  # Direct connection to a game URL
  ./tournament-cli.js --url ws://localhost:9000/games/abc123/player
`);
  process.exit(0);
}

// Helper function to handle game messages following Default_action_player_io logic
function handleGameMessage(ws, message, playerNum) {
  const { type } = message;

  // Notifications (no response needed)
  if (['Action_chosen', 'Lost_influence', 'New_card', 'Challenge', 'Player_responded', 'Game_start'].includes(type)) {
    return;
  }

  let response;

  switch (type) {
    case 'Choose_action':
      // If coins >= 7, coup the first available player, otherwise take income
      const coins = message.visible_game_state.coins;
      const otherPlayers = message.visible_game_state.other_players;

      if (coins >= 7 && otherPlayers.length > 0) {
        const targetPlayerId = otherPlayers[0].player_id;
        response = { type: 'Coup', player_id: targetPlayerId };
      } else {
        response = { type: 'Income' };
      }
      break;

    case 'Choose_assasination_response':
      // Always allow assassination
      response = { type: 'Allow' };
      break;

    case 'Choose_foreign_aid_response':
      // Always allow foreign aid
      response = { type: 'Allow' };
      break;

    case 'Choose_steal_response':
      // Always allow steal
      response = { type: 'Allow' };
      break;

    case 'Choose_cards_to_return':
      // Return the first two cards from the list
      const cards = message.cards;
      response = [cards[0], cards[1]];
      break;

    case 'Reveal_card':
      // Always reveal Card_1
      response = { type: 'Card_1' };
      break;

    case 'Offer_challenge':
      // Never challenge
      response = { type: 'No_challenge' };
      break;

    default:
      console.log(`   ⚠️  Player ${playerNum}: Unknown message type: ${type}`);
      return;
  }

  // Send response
  ws.send(JSON.stringify(response));
}

// Direct game mode - connect to a WebSocket URL and play
function runDirectMode(gameUrl) {
  console.log(`\n🎮 OCoup Direct Game Mode\n`);
  console.log(`Connecting to: ${gameUrl}\n`);

  const ws = new WebSocket(gameUrl);

  ws.on('open', () => {
    console.log('✅ Connected to game\n');
  });

  ws.on('message', (data) => {
    const message = JSON.parse(data.toString());

    if (message.error) {
      console.log(`❌ Error: ${message.error}`);
      ws.close();
      process.exit(1);
      return;
    }

    // Log the message type for visibility
    if (message.type) {
      console.log(`📨 ${message.type}`);
    }

    // Handle game messages using the same logic as tournament mode
    handleGameMessage(ws, message, 'direct');
  });

  ws.on('error', (error) => {
    console.error(`❌ Connection error: ${error.message}`);
    process.exit(1);
  });

  ws.on('close', (code, reason) => {
    console.log(`\n🔌 Connection closed (code: ${code})`);
    if (reason) {
      console.log(`   Reason: ${reason.toString()}`);
    }
    console.log('\n✨ Done!\n');
    process.exit(0);
  });
}

// Tournament mode
function runTournamentMode(numHumanPlayers, serverUrl, botPlayers) {
  const wsProtocol = serverUrl.startsWith('https') ? 'wss' : 'ws';
  const wsUrl = serverUrl.replace(/^https?/, wsProtocol);
  const totalPlayers = numHumanPlayers + botPlayers.length;

  console.log(`\n🎮 OCoup Tournament Manager\n`);
  console.log(`Server: ${serverUrl}`);
  console.log(`Human Players: ${numHumanPlayers}`);
  if (botPlayers.length > 0) {
    console.log(`Bot Players: ${botPlayers.length} (${botPlayers.join(', ')})`);
  }
  console.log(`Total Players: ${totalPlayers}\n`);

  // Step 1: Create tournament
  async function createTournament() {
    console.log('📝 Creating tournament...');

    const requestBody = {
      max_players: totalPlayers
    };

    // Add bot players if specified
    if (botPlayers.length > 0) {
      requestBody.bot_players = botPlayers;
    }

    const response = await fetch(`${serverUrl}/tournaments`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(requestBody)
    });

    if (!response.ok) {
      throw new Error(`Failed to create tournament: ${response.statusText}`);
    }

    const data = await response.json();
    console.log(`✅ Tournament created: ${data.tournament_id}`);
    if (data.num_bot_players > 0) {
      console.log(`   ${data.num_bot_players} bot player(s) pre-registered`);
    }
    console.log();
    return data;
  }

  // Step 2: Register a player via WebSocket
  function registerPlayer(tournamentId, playerNum) {
    return new Promise((resolve, reject) => {
      const ws = new WebSocket(`${wsUrl}/tournaments/${tournamentId}/register`);
      let registered = false;

      ws.on('open', () => {
        console.log(`   Player ${playerNum}: Connecting...`);
      });

      ws.on('message', (data) => {
        const message = JSON.parse(data.toString());

        if (message.error) {
          console.log(`   ❌ Player ${playerNum}: ${message.error}`);
          ws.close();
          reject(new Error(message.error));
          return;
        }

        if (message.status === 'registered') {
          console.log(`   ✅ Player ${playerNum}: Registered (ID: ${message.player_id})`);
          registered = true;
          resolve({ ws, playerId: message.player_id });
          return;
        }

        // Handle game messages after registration
        handleGameMessage(ws, message, playerNum);
      });

      ws.on('error', (error) => {
        if (!registered) {
          console.log(`   ❌ Player ${playerNum}: Connection error`);
          reject(error);
        }
      });

      ws.on('close', () => {
        if (!registered) {
          reject(new Error('Connection closed before registration'));
        }
      });
    });
  }

  // Step 3: Register all players
  async function registerAllPlayers(tournamentId) {
    if (numHumanPlayers === 0) {
      console.log('ℹ️  No human players to register (bots only)\n');
      return [];
    }

    console.log(`👥 Registering ${numHumanPlayers} human player(s)...\n`);

    const players = [];
    for (let i = 0; i < numHumanPlayers; i++) {
      try {
        const player = await registerPlayer(tournamentId, i);
        players.push(player);
        // Small delay to avoid overwhelming the server
        await new Promise(resolve => setTimeout(resolve, 100));
      } catch (error) {
        console.error(`\n❌ Failed to register player ${i}: ${error.message}`);
        // Close all previously registered players
        players.forEach(p => p.ws.close());
        throw error;
      }
    }

    console.log(`\n✅ All human players registered!\n`);
    return players;
  }

  // Step 4: Start tournament
  async function startTournament(tournamentId) {
    console.log('🚀 Starting tournament...\n');

    const response = await fetch(`${serverUrl}/tournaments/${tournamentId}/start`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' }
    });

    if (!response.ok) {
      const error = await response.text();
      throw new Error(`Failed to start tournament: ${error}`);
    }

    const data = await response.json();

    if (data.error) {
      throw new Error(data.error);
    }

    console.log('🏆 Tournament completed!\n');
    return data;
  }

  // Step 5: Display results
  function displayResults(data) {
    // Display tournament results
    if (data.results) {
      console.log('📋 Tournament Results:\n');
      console.log('═'.repeat(60));

      data.results.forEach(round => {
        console.log(`\n🎲 Round ${round.round}:`);
        console.log('─'.repeat(60));

        round.games.forEach(game => {
          if (game.status === 'completed') {
            console.log(`  Game ${game.game}:`);
            console.log(`    🏆 Winner(s): ${game.winners.join(', ')}`);
            if (game.eliminated.length > 0) {
              console.log(`    ❌ Eliminated (in order): ${game.eliminated.join(', ')}`);
            }
          } else {
            console.log(`  Game ${game.game}: ⚠️  Error - ${game.error}`);
          }
        });
      });

      console.log('\n' + '═'.repeat(60) + '\n');
    }

    // Display final scores
    console.log('📊 Final Scores:\n');
    console.log('─'.repeat(40));

    const scores = Object.entries(data.scores)
      .map(([playerId, score]) => ({ playerId, score }))
      .sort((a, b) => b.score - a.score);

    scores.forEach((entry, index) => {
      const medal = index === 0 ? '🥇' : index === 1 ? '🥈' : index === 2 ? '🥉' : '  ';
      console.log(`${medal} Player ${entry.playerId}: ${entry.score} points`);
    });

    console.log('─'.repeat(40));
    console.log(`\n🎉 Winner: Player ${scores[0].playerId} with ${scores[0].score} points!\n`);
  }

  // Main execution
  async function main() {
    try {
      // Create tournament
      const tournament = await createTournament();

      const players = await registerAllPlayers(tournament.tournament_id);

      const results = await startTournament(tournament.tournament_id);

      // Display results
      displayResults(results);

      // Close all WebSocket connections
      players.forEach(p => p.ws.close());

      console.log('✨ Done!\n');
      process.exit(0);
    } catch (error) {
      console.error(`\n❌ Error: ${error.message}\n`);
      process.exit(1);
    }
  }

  // Run tournament
  main();
}

// Check for direct game mode
const urlFlagIndex = args.indexOf('--url');
const isDirectMode = urlFlagIndex !== -1;

if (isDirectMode) {
  // Direct game mode - connect to a specific game URL
  const directUrl = args[urlFlagIndex + 1];
  if (!directUrl) {
    console.error('❌ Error: --url requires a WebSocket URL argument\n');
    process.exit(1);
  }
  runDirectMode(directUrl);
} else {
  // Tournament mode
  const numHumanPlayers = parseInt(args[0]) || 4;
  const serverUrl = args[1] || 'http://localhost:9000';
  const botPlayers = args.slice(2); // Any additional args are bot player types
  runTournamentMode(numHumanPlayers, serverUrl, botPlayers);
}
