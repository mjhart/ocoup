// Card types
type Card = 'Duke' | 'Assassin' | 'Captain' | 'Ambassador' | 'Contessa';

// Player ID type
type PlayerId = number;

// Game state types
interface CardInHand {
  card: Card;
  revealed: boolean;
}

interface OtherPlayer {
  player_id: PlayerId;
  visible_card: Card | null;
  coins: number;
}

interface VisibleGameState {
  hand: CardInHand[];
  coins: number;
  other_players: OtherPlayer[];
  active_player_id: PlayerId;
}

// Action types
type Action = 
  | { type: 'Income' }
  | { type: 'Foreign_aid' }
  | { type: 'Assassinate'; player_id: PlayerId }
  | { type: 'Coup'; player_id: PlayerId }
  | { type: 'Tax' }
  | { type: 'Steal'; player_id: PlayerId }
  | { type: 'Exchange' };

// Response types
type AllowOrBlock = { type: 'Allow' } | { type: 'Block' };

type StealResponse = 
  | { type: 'Allow' }
  | { type: 'Block'; card: 'Ambassador' | 'Captain' };

type ChallengeResponse = 
  | { type: 'No_challenge' }
  | { type: 'Challenge' };

type RevealCardResponse = 
  | { type: 'Card_1' }
  | { type: 'Card_2' };

// Server message types (messages sent to client)
type ServerMessage =
  | { type: 'Choose_action'; visible_game_state: VisibleGameState }
  | { type: 'Choose_assasination_response'; player_id: PlayerId; visible_game_state: VisibleGameState }
  | { type: 'Choose_foreign_aid_response'; visible_game_state: VisibleGameState }
  | { type: 'Choose_steal_response'; player_id: PlayerId; visible_game_state: VisibleGameState }
  | { type: 'Choose_cards_to_return'; cards: Card[]; visible_game_state: VisibleGameState }
  | { type: 'Reveal_card'; card_1: Card; card_2: Card; visible_game_state: VisibleGameState }
  | { type: 'Offer_challenge'; acting_player_id: PlayerId; action: Action; visible_game_state: VisibleGameState }
  | { type: 'Action_chosen'; player_id: PlayerId; action: Action }
  | { type: 'Lost_influence'; player_id: PlayerId; card: Card }
  | { type: 'New_card'; card: Card }
  | { type: 'Challenge'; player_id: PlayerId; has_required_card: boolean }
  | { type: 'Player_responded'; player_id: PlayerId };

// Client message types (responses sent to server)
type ClientMessage =
  | Action // For Choose_action
  | AllowOrBlock // For Choose_assasination_response and Choose_foreign_aid_response
  | StealResponse // For Choose_steal_response
  | [Card, Card] // For Choose_cards_to_return
  | RevealCardResponse // For Reveal_card
  | ChallengeResponse; // For Offer_challenge

export type {
  Card,
  PlayerId,
  CardInHand,
  OtherPlayer,
  VisibleGameState,
  Action,
  AllowOrBlock,
  StealResponse,
  ChallengeResponse,
  RevealCardResponse,
  ServerMessage,
  ClientMessage
}; 