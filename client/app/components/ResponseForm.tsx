import type { ServerMessage, ClientMessage, Card, Action, PlayerId } from '../types';

interface Props {
  lastMessage: ServerMessage | null;
  onSubmit: (response: ClientMessage) => void;
}

export function ResponseForm({ lastMessage, onSubmit }: Props) {
  if (!lastMessage) return null;

  switch (lastMessage.type) {
    case 'Choose_action':
      return (
        <div className="space-y-4">
          <div className="grid grid-cols-2 gap-2">
            <button
              onClick={() => onSubmit({ type: 'Income' })}
              className="btn"
            >
              Income
            </button>
            <button
              onClick={() => onSubmit({ type: 'Foreign_aid' })}
              className="btn"
            >
              Foreign Aid
            </button>
            <button
              onClick={() => onSubmit({ type: 'Tax' })}
              className="btn"
            >
              Tax
            </button>
            <button
              onClick={() => onSubmit({ type: 'Exchange' })}
              className="btn"
            >
              Exchange
            </button>
          </div>
          
          <div className="space-y-2">
            <div className="font-medium">Actions requiring a target:</div>
            {lastMessage.visible_game_state.other_players.map(player => (
              <div key={player.player_id} className="flex gap-2">
                <button
                  onClick={() => onSubmit({ type: 'Assassinate', player_id: player.player_id })}
                  className="btn"
                  disabled={lastMessage.visible_game_state.coins < 3}
                >
                  Assassinate
                </button>
                <button
                  onClick={() => onSubmit({ type: 'Coup', player_id: player.player_id })}
                  className="btn"
                  disabled={lastMessage.visible_game_state.coins < 7}
                >
                  Coup
                </button>
                <button
                  onClick={() => onSubmit({ type: 'Steal', player_id: player.player_id })}
                  className="btn"
                >
                  Steal
                </button>
                <span className="ml-2 text-gray-600">
                  Player {player.player_id} ({player.coins} coins)
                </span>
              </div>
            ))}
          </div>
        </div>
      );

    case 'Choose_assasination_response':
    case 'Choose_foreign_aid_response':
      return (
        <div className="flex gap-2">
          <button
            onClick={() => onSubmit({ type: 'Allow' })}
            className="btn"
          >
            Allow
          </button>
          <button
            onClick={() => onSubmit({ type: 'Block' })}
            className="btn"
          >
            Block
          </button>
        </div>
      );

    case 'Choose_steal_response':
      return (
        <div className="flex gap-2">
          <button
            onClick={() => onSubmit({ type: 'Allow' })}
            className="btn"
          >
            Allow
          </button>
          <button
            onClick={() => onSubmit({ type: 'Block', card: 'Ambassador' })}
            className="btn"
          >
            Block with Ambassador
          </button>
          <button
            onClick={() => onSubmit({ type: 'Block', card: 'Captain' })}
            className="btn"
          >
            Block with Captain
          </button>
        </div>
      );

    case 'Choose_cards_to_return':
      return (
        <div className="grid grid-cols-2 gap-2">
          {lastMessage.cards.map((card1, i) => 
            lastMessage.cards.slice(i + 1).map(card2 => (
              <button
                key={`${card1}-${card2}`}
                onClick={() => onSubmit([card1, card2])}
                className="btn"
              >
                Keep {card1} and {card2}
              </button>
            ))
          )}
        </div>
      );

    case 'Reveal_card':
      return (
        <div className="flex gap-2">
          <button
            onClick={() => onSubmit({ type: 'Card_1' })}
            className="btn"
          >
            Reveal {lastMessage.card_1}
          </button>
          <button
            onClick={() => onSubmit({ type: 'Card_2' })}
            className="btn"
          >
            Reveal {lastMessage.card_2}
          </button>
        </div>
      );

    case 'Offer_challenge':
      return (
        <div className="flex gap-2">
          <button
            onClick={() => onSubmit({ type: 'No_challenge' })}
            className="btn"
          >
            Allow
          </button>
          <button
            onClick={() => onSubmit({ type: 'Challenge' })}
            className="btn"
          >
            Challenge
          </button>
        </div>
      );

    default:
      return null;
  }
} 