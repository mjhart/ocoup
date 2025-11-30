import { useState } from 'react';
import type { ServerMessage, Card, Action, Challengable, VisibleGameState, CardInHand, OtherPlayer } from '../types';

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
      return `Block steal ${action.blocking_card ? `as ${action.blocking_card}` : ''}`;
    case 'Block_foreign_aid':
      return 'Block foreign aid';
  }
}

function formatGameState(state: VisibleGameState) {
  return (
    <div className="mt-2 space-y-2">
      <div>
        <strong>Your hand:</strong>{' '}
        {state.hand.map((card: CardInHand, i: number) => (
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
          {state.other_players.map((player: OtherPlayer, i: number) => (
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
  const [showJson, setShowJson] = useState(false);
  
  const toggleJsonView = () => {
    setShowJson(!showJson);
  };
  
  // JSON display component
  const JsonDisplay = () => {
    if (!showJson) return null;

    return (
      <div className="mt-3 p-3 bg-mcm-offwhite rounded-md border border-mcm-cream">
        <pre className="text-xs whitespace-pre-wrap overflow-x-auto">
          {JSON.stringify(message, null, 2)}
        </pre>
      </div>
    );
  };

  // Reusable JSON toggle button
  const JsonToggleButton = () => (
    <button
      onClick={toggleJsonView}
      className="mt-2 text-xs bg-mcm-cream hover:bg-mcm-mustard text-mcm-navy rounded-sm px-2 py-1 transition-colors"
    >
      {showJson ? "Hide JSON" : "Show JSON"}
    </button>
  );

  switch (message.type) {
    case 'Choose_action':
      return (
        <div>
          <div className="font-semibold text-accent">Your turn! Choose an action:</div>
          {formatGameState(message.visible_game_state)}
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'Choose_assasination_response':
      return (
        <div>
          <div className="font-semibold text-red-600">
            Player {message.player_id} is trying to assassinate you! Block or allow?
          </div>
          {formatGameState(message.visible_game_state)}
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'Choose_foreign_aid_response':
      return (
        <div>
          <div className="font-semibold text-yellow-600">
            A player is attempting to take foreign aid. Block or allow?
          </div>
          {formatGameState(message.visible_game_state)}
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'Choose_steal_response':
      return (
        <div>
          <div className="font-semibold text-red-600">
            Player {message.player_id} is trying to steal from you! Block or allow?
          </div>
          {formatGameState(message.visible_game_state)}
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'Choose_cards_to_return':
      return (
        <div>
          <div className="font-semibold text-accent">Choose two cards to return:</div>
          <div className="mt-2">
            Available cards: {message.cards.map(formatCard).join(', ')}
          </div>
          {formatGameState(message.visible_game_state)}
          <JsonToggleButton />
          <JsonDisplay />
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
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'Offer_challenge':
      // Format the action being challenged
      const actionDescription = formatAction(message.action);
      
      return (
        <div>
          <div className="font-display text-xl font-bold uppercase tracking-wider text-mcm-orange mb-2 border-b-2 border-mcm-mustard pb-2">
            Challenge Opportunity
          </div>
          <div className="text-sm font-bold uppercase tracking-wider text-mcm-navy mb-2">
            Player {message.acting_player_id} is attempting: {actionDescription}
          </div>
          <div className="text-sm font-bold text-mcm-navy mb-3">
            Would you like to challenge this action?
          </div>
          {formatGameState(message.visible_game_state)}
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'Action_chosen':
      return (
        <div>
          <div className="text-mcm-navy">
            Player {message.player_id} chose: {formatAction(message.action)}
          </div>
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'Lost_influence':
      return (
        <div>
          <div className="text-red-600">
            Player {message.player_id} lost influence: {formatCard(message.card)}
          </div>
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'New_card':
      return (
        <div>
          <div className="text-green-600">
            You received a new card: {formatCard(message.card)}
          </div>
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'Challenge':
      return (
        <div>
          <div className={message.has_required_card ? 'text-red-600' : 'text-green-600'}>
            Player {message.player_id} challenged you {message.has_required_card ? 'unsuccessfully' : 'successfully'}
          </div>
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'Player_responded':
      return (
        <div>
          <div className="text-mcm-navy">
            Player {message.player_id} has responded
          </div>
          <JsonToggleButton />
          <JsonDisplay />
        </div>
      );

    case 'Game_start':
      return (
        <div>
          <div className="font-semibold text-accent">Game started!</div>
          <div className="text-mcm-navy">
            You are player {message.self_player_id}
          </div>
          {formatGameState(message.visible_game_state)}
        </div>
      );
  }
} 