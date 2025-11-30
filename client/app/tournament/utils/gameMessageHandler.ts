interface GameMessage {
  type: string;
  visible_game_state?: {
    coins: number;
    other_players: Array<{ player_id: string }>;
  };
  cards?: string[];
}

/**
 * Handle game messages following Default_action_player_io logic
 * This logic auto-plays the game: coups at 7+ coins, otherwise takes income
 */
export function handleGameMessage(
  ws: WebSocket,
  message: GameMessage,
  playerNum: number,
  addLog: (message: string) => void
): void {
  const { type } = message;

  // Notifications (no response needed)
  if (
    [
      'Action_chosen',
      'Lost_influence',
      'New_card',
      'Challenge',
      'Player_responded',
      'Game_start',
    ].includes(type)
  ) {
    return;
  }

  let response: unknown;

  switch (type) {
    case 'Choose_action': {
      // If coins >= 7, coup the first available player, otherwise take income
      const coins = message.visible_game_state?.coins ?? 0;
      const otherPlayers = message.visible_game_state?.other_players ?? [];

      if (coins >= 7 && otherPlayers.length > 0) {
        const targetPlayerId = otherPlayers[0].player_id;
        response = { type: 'Coup', player_id: targetPlayerId };
        addLog(`Player ${playerNum}: Couping ${targetPlayerId}`);
      } else {
        response = { type: 'Income' };
        addLog(`Player ${playerNum}: Taking income`);
      }
      break;
    }

    case 'Choose_assasination_response':
      // Always allow assassination
      response = { type: 'Allow' };
      addLog(`Player ${playerNum}: Allowing assassination`);
      break;

    case 'Choose_foreign_aid_response':
      // Always allow foreign aid
      response = { type: 'Allow' };
      addLog(`Player ${playerNum}: Allowing foreign aid`);
      break;

    case 'Choose_steal_response':
      // Always allow steal
      response = { type: 'Allow' };
      addLog(`Player ${playerNum}: Allowing steal`);
      break;

    case 'Choose_cards_to_return': {
      // Return the first two cards from the list
      const cards = message.cards ?? [];
      response = cards.length >= 2 ? [cards[0], cards[1]] : cards;
      addLog(`Player ${playerNum}: Returning cards`);
      break;
    }

    case 'Reveal_card':
      // Always reveal Card_1
      response = { type: 'Card_1' };
      addLog(`Player ${playerNum}: Revealing card`);
      break;

    case 'Offer_challenge':
      // Never challenge
      response = { type: 'No_challenge' };
      break;

    default:
      addLog(`Player ${playerNum}: Unknown message type: ${type}`);
      return;
  }

  // Send response
  if (ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify(response));
  }
}
