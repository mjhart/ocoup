'use client';

import { useState } from 'react';

export function BotHelp() {
  const [isOpen, setIsOpen] = useState(false);

  return (
    <div className="mcm-panel mt-4">
      <div className="flex justify-between items-center mb-3 pb-2 border-b-2 border-mcm-mustard">
        <h2 className="font-display text-xl text-mcm-navy">BOT DEVELOPMENT</h2>
        <button 
          onClick={() => setIsOpen(!isOpen)} 
          className="text-xs uppercase tracking-wider bg-mcm-teal text-white px-3 py-1 rounded-md font-bold hover:bg-mcm-sage"
        >
          {isOpen ? 'Hide Guide' : 'Show Guide'}
        </button>
      </div>
      
      {isOpen && (
        <div className="bg-mcm-offwhite rounded-xl p-4 border-2 border-mcm-mustard">
          <section className="mb-6">
            <h3 className="font-bold text-mcm-navy mb-2 text-lg">How to Build a Bot for Coup</h3>
            <p className="text-sm text-mcm-navy mb-4">
              Your bot connects to the game server via WebSocket and exchanges JSON messages.
              Below are the message types and a minimal example to get you started.
            </p>
          </section>

          <section className="mb-6">
            <h3 className="font-bold text-mcm-navy mb-2">Message Types</h3>
            
            <div className="mb-4">
              <h4 className="font-bold text-mcm-navy mb-1 text-sm">Server → Bot (ServerMessage)</h4>
              <div className="bg-mcm-navy text-white p-3 rounded-md font-mono text-xs overflow-x-auto">
{`type ServerMessage =
  | { type: 'Game_start'; self_player_id: PlayerId; visible_game_state: VisibleGameState }
  | { type: 'Choose_action'; visible_game_state: VisibleGameState }
  | { type: 'Choose_assasination_response'; player_id: PlayerId; visible_game_state: VisibleGameState }
  | { type: 'Choose_foreign_aid_response'; visible_game_state: VisibleGameState }
  | { type: 'Choose_steal_response'; player_id: PlayerId; visible_game_state: VisibleGameState }
  | { type: 'Choose_cards_to_return'; cards: Card[]; visible_game_state: VisibleGameState }
  | { type: 'Reveal_card'; card_1: Card; card_2: Card; visible_game_state: VisibleGameState }
  | { type: 'Offer_challenge'; acting_player_id: PlayerId; action: Challengable; visible_game_state: VisibleGameState }
  | { type: 'Action_chosen'; player_id: PlayerId; action: Action }
  | { type: 'Lost_influence'; player_id: PlayerId; card: Card }
  | { type: 'New_card'; card: Card }
  | { type: 'Challenge'; player_id: PlayerId; has_required_card: boolean }
  | { type: 'Player_responded'; player_id: PlayerId };`}
              </div>
            </div>
            
            <div>
              <h4 className="font-bold text-mcm-navy mb-1 text-sm">Bot → Server (ClientMessage)</h4>
              <div className="bg-mcm-navy text-white p-3 rounded-md font-mono text-xs overflow-x-auto">
{`type ClientMessage =
  | Action // For Choose_action (e.g., { type: 'Income' })
  | AllowOrBlock // For Choose_assasination_response ({ type: 'Allow' } or { type: 'Block' })
  | StealResponse // For Choose_steal_response ({ type: 'Allow' } or { type: 'Block', card: 'Ambassador' | 'Captain' })
  | [Card, Card] // For Choose_cards_to_return (e.g., ['Duke', 'Assassin'])
  | RevealCardResponse // For Reveal_card ({ type: 'Card_1' } or { type: 'Card_2' })
  | ChallengeResponse; // For Offer_challenge ({ type: 'No_challenge' } or { type: 'Challenge' })`}
              </div>
            </div>
          </section>

          <section className="mb-6">
            <h3 className="font-bold text-mcm-navy mb-2">Game State Structure</h3>
            <div className="bg-mcm-navy text-white p-3 rounded-md font-mono text-xs overflow-x-auto">
{`interface VisibleGameState {
  hand: CardInHand[];  // Your cards: { card: Card; revealed: boolean }[]
  coins: number;       // Your coins
  other_players: OtherPlayer[];  // Information about other players
  active_player_id: PlayerId;    // ID of the currently active player
}

interface OtherPlayer {
  player_id: PlayerId;
  visible_card: Card | null;  // Revealed card or null if none revealed
  coins: number;
}`}
            </div>
          </section>

          <section>
            <h3 className="font-bold text-mcm-navy mb-2">Minimal Bot Example (JavaScript)</h3>
            <div className="bg-mcm-navy text-white p-3 rounded-md font-mono text-xs overflow-x-auto whitespace-pre-wrap">
{`// Simple Coup bot example
const WebSocket = require('ws');
const playerWs = new WebSocket('YOUR_PLAYER_URL_HERE');

// Bot state
let myPlayerId = null;
let gameState = null;

playerWs.on('open', () => {
  console.log('Connected to the game server');
});

playerWs.on('message', (data) => {
  const message = JSON.parse(data);
  console.log('Received:', message);
  
  // Update game state
  if (message.visible_game_state) {
    gameState = message.visible_game_state;
  }
  
  // Store player ID on game start
  if (message.type === 'Game_start') {
    myPlayerId = message.self_player_id;
  }
  
  // Handle different message types
  switch (message.type) {
    case 'Choose_action':
      // Simple strategy: Always claim Income if we have less than 7 coins
      // Otherwise, attempt a Coup on the first player that's not us
      const response = gameState.coins >= 7
        ? { type: 'Coup', player_id: findFirstOtherPlayer() }
        : { type: 'Income' };
      sendResponse(response);
      break;
      
    case 'Choose_assasination_response':
    case 'Choose_foreign_aid_response':
      // Simple strategy: Always allow actions
      sendResponse({ type: 'Allow' });
      break;
      
    case 'Choose_steal_response':
      // Simple strategy: Always allow stealing
      sendResponse({ type: 'Allow' });
      break;
      
    case 'Choose_cards_to_return':
      // Simple strategy: Keep first two cards
      sendResponse([message.cards[2], message.cards[3]]);
      break;
      
    case 'Reveal_card':
      // Simple strategy: Always reveal first card
      sendResponse({ type: 'Card_1' });
      break;
      
    case 'Offer_challenge':
      // Simple strategy: Never challenge
      sendResponse({ type: 'No_challenge' });
      break;
  }
});

function findFirstOtherPlayer() {
  return gameState.other_players.find(p => p.player_id !== myPlayerId)?.player_id;
}

function sendResponse(response) {
  console.log('Sending:', response);
  playerWs.send(JSON.stringify(response));
}

// Handle errors and disconnects
playerWs.on('error', console.error);
playerWs.on('close', () => console.log('Disconnected from server'));`}
            </div>
          </section>
        </div>
      )}
    </div>
  );
}