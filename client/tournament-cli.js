#!/usr/bin/env node

import WebSocket from 'ws';

// Parse command line arguments
const args = process.argv.slice(2);
const numPlayers = parseInt(args[0]) || 4;
const serverUrl = args[1] || 'http://localhost:9000';
const wsProtocol = serverUrl.startsWith('https') ? 'wss' : 'ws';
const wsUrl = serverUrl.replace(/^https?/, wsProtocol);

console.log(`\n🎮 OCoup Tournament Manager\n`);
console.log(`Server: ${serverUrl}`);
console.log(`Players: ${numPlayers}\n`);

// Step 1: Create tournament
async function createTournament() {
  console.log('📝 Creating tournament...');
  const response = await fetch(`${serverUrl}/tournaments`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ max_players: numPlayers })
  });

  if (!response.ok) {
    throw new Error(`Failed to create tournament: ${response.statusText}`);
  }

  const data = await response.json();
  console.log(`✅ Tournament created: ${data.tournament_id}\n`);
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
      } else {
        console.log(`   🎲 Player ${message.player_id}: received message ${JSON.stringify(message)}`);
      }
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
  console.log(`👥 Registering ${numPlayers} players...\n`);

  const players = [];
  for (let i = 0; i < numPlayers; i++) {
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

  console.log(`\n✅ All players registered!\n`);
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
function displayResults(results) {
  console.log('📊 Final Scores:\n');
  console.log('─'.repeat(40));

  const scores = Object.entries(results.scores)
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

// Run
main();
