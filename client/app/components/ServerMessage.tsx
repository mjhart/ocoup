import type { ServerMessage, Card, Action, Challengable } from '../types';

function formatCard(card: Card): string {
  return card;
}

function formatAction(action: Action | Challengable): string {
  switch (action.type) {
    case 'Income':
      return 'Income';
    case 'Foreign_aid':
      return 'Foreign Aid';
    case 'Assassinate':
      return `Assassinate player ${action.player_id}`;
    case 'Coup':
      return `Coup player ${action.player_id}`;
    case 'Tax':
      return 'Tax';
    case 'Steal':
      return `Steal from player ${action.player_id}`;
    case 'Exchange':
      return 'Exchange';
    case 'Block_assassination':
      return 'Block assassination';
    case 'Block_steal':
      return `Block steal as ${action.card}`;
    case 'Block_foreign_aid':
      return 'Block foreign aid';
  }
}

function formatGameState(state: ServerMessage['visible_game_state']) {
  return (
    <div className="mt-2 space-y-2">
      <div>
        <strong>Your hand:</strong>{' '}
        {state.hand.map((card, i) => (
          <span key={i} className="ml-2">
            {formatCard(card.card)}
            {card.revealed ? ' (revealed)' : ' (hidden)'}
          </span>
        ))}
      </div>
      <div>
        <strong>Your coins:</strong> {state.coins}
      </div>
      <div>
        <strong>Other players:</strong>
        <div className="ml-4">
          {state.other_players.map((player, i) => (
            <div key={i}>
              Player {player.player_id}: {player.coins} coins
              {player.visible_card && ` - Revealed: ${formatCard(player.visible_card)}`}
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}

export function ServerMessage({ message }: { message: ServerMessage }) {
  switch (message.type) {
    case 'Choose_action':
      return (
        <div>
          <div className="font-semibold text-indigo-600">Your turn! Choose an action:</div>
          {formatGameState(message.visible_game_state)}
        </div>
      );

    case 'Choose_assasination_response':
      return (
        <div>
          <div className="font-semibold text-red-600">
            Player {message.player_id} is trying to assassinate you! Block or allow?
          </div>
          {formatGameState(message.visible_game_state)}
        </div>
      );

    case 'Choose_foreign_aid_response':
      return (
        <div>
          <div className="font-semibold text-yellow-600">
            A player is attempting to take foreign aid. Block or allow?
          </div>
          {formatGameState(message.visible_game_state)}
        </div>
      );

    case 'Choose_steal_response':
      return (
        <div>
          <div className="font-semibold text-red-600">
            Player {message.player_id} is trying to steal from you! Block or allow?
          </div>
          {formatGameState(message.visible_game_state)}
        </div>
      );

    case 'Choose_cards_to_return':
      return (
        <div>
          <div className="font-semibold text-indigo-600">Choose two cards to return:</div>
          <div className="mt-2">
            Available cards: {message.cards.map(formatCard).join(', ')}
          </div>
          {formatGameState(message.visible_game_state)}
        </div>
      );

    case 'Reveal_card':
      return (
        <div>
          <div className="font-semibold text-red-600">Choose a card to reveal:</div>
          <div className="mt-2">
            Card 1: {formatCard(message.card_1)}
            <br />
            Card 2: {formatCard(message.card_2)}
          </div>
          {formatGameState(message.visible_game_state)}
        </div>
      );

    case 'Offer_challenge':
      return (
        <div>
          <div className="font-semibold text-yellow-600">
            Player {message.acting_player_id} is attempting: {formatAction(message.action)}
            <br />
            Would you like to challenge?
          </div>
          {formatGameState(message.visible_game_state)}
        </div>
      );

    case 'Action_chosen':
      return (
        <div className="text-gray-600">
          Player {message.player_id} chose: {formatAction(message.action)}
        </div>
      );

    case 'Lost_influence':
      return (
        <div className="text-red-600">
          Player {message.player_id} lost influence: {formatCard(message.card)}
        </div>
      );

    case 'New_card':
      return (
        <div className="text-green-600">
          You received a new card: {formatCard(message.card)}
        </div>
      );

    case 'Challenge':
      return (
        <div className={message.has_required_card ? 'text-red-600' : 'text-green-600'}>
          Player {message.player_id} challenged you {message.has_required_card ? 'unsuccessfully' : 'successfully'}
        </div>
      );

    case 'Player_responded':
      return (
        <div className="text-gray-600">
          Player {message.player_id} has responded
        </div>
      );
  }
} 