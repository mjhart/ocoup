import type { ServerMessage, ClientMessage, Card, Action, PlayerId } from '../types';

interface Props {
  lastMessage: ServerMessage | null;
  onSubmit: (response: ClientMessage) => void;
}

export function ResponseForm({ lastMessage, onSubmit }: Props) {
  if (!lastMessage) return null;

  // Define Tailwind classes for buttons
  const btnClass = "py-3 px-5 bg-mcm-coral text-white border-none rounded-xl shadow-mcm transition-all duration-200 cursor-pointer text-xs font-bold uppercase tracking-wider hover:bg-mcm-orange hover:shadow-mcm-lg disabled:opacity-50 disabled:cursor-not-allowed";

  switch (lastMessage.type) {
    case 'Choose_action':
      return (
        <div className="space-y-4">
          <div className="grid grid-cols-2 gap-2">
            <button
              onClick={() => onSubmit({ type: 'Income' })}
              className={btnClass}
            >
              Income
            </button>
            <button
              onClick={() => onSubmit({ type: 'Foreign_aid' })}
              className={btnClass}
            >
              Foreign Aid
            </button>
            <button
              onClick={() => onSubmit({ type: 'Tax' })}
              className={btnClass}
            >
              Tax
            </button>
            <button
              onClick={() => onSubmit({ type: 'Exchange' })}
              className={btnClass}
            >
              Exchange
            </button>
          </div>
          
          <div className="space-y-2">
            <div className="font-bold uppercase tracking-wider text-xs text-mcm-navy border-b border-mcm-mustard pb-2 mb-2">Actions requiring a target:</div>
            {lastMessage.visible_game_state.other_players.map(player => (
              <div key={player.player_id} className="flex gap-2">
                <button
                  onClick={() => onSubmit({ type: 'Assassinate', player_id: player.player_id })}
                  className={btnClass}
                  disabled={lastMessage.visible_game_state.coins < 3}
                >
                  Assassinate
                </button>
                <button
                  onClick={() => onSubmit({ type: 'Coup', player_id: player.player_id })}
                  className={btnClass}
                  disabled={lastMessage.visible_game_state.coins < 7}
                >
                  Coup
                </button>
                <button
                  onClick={() => onSubmit({ type: 'Steal', player_id: player.player_id })}
                  className={btnClass}
                >
                  Steal
                </button>
                <span className={`ml-2 ${player.player_id === lastMessage.visible_game_state.active_player_id ? "text-mcm-coral font-bold" : "text-mcm-navy"}`}>
                  Player {player.player_id} ({player.coins} coins)
                  {player.player_id === lastMessage.visible_game_state.active_player_id && 
                    <span className="ml-1 text-xs uppercase tracking-wider font-bold bg-mcm-orange text-white px-2 py-0.5 rounded-full">Active</span>
                  }
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
            className={btnClass}
          >
            Allow
          </button>
          <button
            onClick={() => onSubmit({ type: 'Block' })}
            className={btnClass}
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
            className={btnClass}
          >
            Allow
          </button>
          <button
            onClick={() => onSubmit({ type: 'Block', card: 'Ambassador' })}
            className={btnClass}
          >
            Block with Ambassador
          </button>
          <button
            onClick={() => onSubmit({ type: 'Block', card: 'Captain' })}
            className={btnClass}
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
                className={btnClass}
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
            className={btnClass}
          >
            Reveal {lastMessage.card_1}
          </button>
          <button
            onClick={() => onSubmit({ type: 'Card_2' })}
            className={btnClass}
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
            className={btnClass}
          >
            Allow
          </button>
          <button
            onClick={() => onSubmit({ type: 'Challenge' })}
            className={btnClass}
          >
            Challenge
          </button>
        </div>
      );

    default:
      return null;
  }
}