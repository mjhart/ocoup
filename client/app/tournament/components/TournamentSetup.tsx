'use client';

import { useState } from 'react';
import { SERVER_URL } from '../../config';

interface TournamentSetupProps {
  onComplete: (data: {
    tournamentId: string;
    numHumanPlayers: number;
    botPlayers: string[];
  }) => void;
  onError: (error: string) => void;
}

const BOT_TYPES = [
  { value: 'gpt-5-mini', label: 'GPT-5-mini', description: 'OpenAI GPT-5-mini' },
  { value: 'gpt-5-nano', label: 'GPT-5-nano', description: 'OpenAI GPT-5-nano' },
  { value: 'o3-mini', label: 'O3-mini', description: 'OpenAI O3-mini' },
  { value: 'gemini-2-5', label: 'Gemini 2.5', description: 'Google Gemini 2.5' },
];

export default function TournamentSetup({ onComplete, onError }: TournamentSetupProps) {
  const [numHumanPlayers, setNumHumanPlayers] = useState(4);
  const [selectedBots, setSelectedBots] = useState<string[]>([]);
  const [isCreating, setIsCreating] = useState(false);

  const totalPlayers = numHumanPlayers + selectedBots.length;

  const handleBotToggle = (botType: string) => {
    setSelectedBots((prev) =>
      prev.includes(botType) ? prev.filter((b) => b !== botType) : [...prev, botType]
    );
  };

  const handleCreateTournament = async () => {
    if (totalPlayers < 2) {
      onError('Tournament requires at least 2 players');
      return;
    }

    setIsCreating(true);

    try {
      const requestBody: { max_players: number; bot_players?: string[] } = {
        max_players: totalPlayers,
      };

      if (selectedBots.length > 0) {
        requestBody.bot_players = selectedBots;
      }

      const response = await fetch(`${SERVER_URL}/tournaments`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(requestBody),
      });

      if (!response.ok) {
        throw new Error(`Failed to create tournament: ${response.statusText}`);
      }

      const data = await response.json();

      onComplete({
        tournamentId: data.tournament_id,
        numHumanPlayers,
        botPlayers: selectedBots,
      });
    } catch (error) {
      onError(error instanceof Error ? error.message : 'Failed to create tournament');
      setIsCreating(false);
    }
  };

  return (
    <div className="mcm-panel">
      <h2 className="font-display text-xl text-mcm-navy mb-3 pb-2 border-b-2 border-mcm-mustard">Tournament Setup</h2>

      <div className="space-y-6">
        <div>
          <label className="block text-sm font-bold text-mcm-navy mb-2 uppercase tracking-wider">
            Number of Human Players
          </label>
          <input
            type="number"
            min="0"
            max="20"
            value={numHumanPlayers}
            onChange={(e) => setNumHumanPlayers(parseInt(e.target.value) || 0)}
            className="w-full bg-white border-2 border-mcm-mustard rounded-xl px-4 py-2 text-mcm-navy focus:outline-hidden focus:border-mcm-coral focus:shadow-mcm"
          />
          <p className="mt-1 text-sm text-mcm-brown">
            Each human player will auto-play using simple AI logic
          </p>
        </div>

        <div>
          <label className="block text-sm font-bold text-mcm-navy mb-3 uppercase tracking-wider">
            Bot Players (Optional)
          </label>
          <div className="space-y-2">
            {BOT_TYPES.map((bot) => (
              <label
                key={bot.value}
                className="flex items-center gap-3 p-3 bg-white border-2 border-mcm-mustard rounded-xl cursor-pointer hover:border-mcm-coral transition-colors"
              >
                <input
                  type="checkbox"
                  checked={selectedBots.includes(bot.value)}
                  onChange={() => handleBotToggle(bot.value)}
                  className="w-4 h-4 rounded-sm border-mcm-mustard text-mcm-coral focus:ring-mcm-coral"
                />
                <div className="flex-1">
                  <div className="text-mcm-navy font-bold">{bot.label}</div>
                  <div className="text-sm text-mcm-brown">{bot.description}</div>
                </div>
                {selectedBots.includes(bot.value) && (
                  <div className="flex items-center gap-2">
                    <button
                      type="button"
                      onClick={(e) => {
                        e.preventDefault();
                        setSelectedBots((prev) => [...prev, bot.value]);
                      }}
                      className="text-mcm-coral hover:text-mcm-orange font-bold text-sm px-2 py-1"
                    >
                      +
                    </button>
                    <span className="text-mcm-navy font-bold text-sm">
                      {selectedBots.filter((b) => b === bot.value).length}
                    </span>
                    <button
                      type="button"
                      onClick={(e) => {
                        e.preventDefault();
                        const index = selectedBots.lastIndexOf(bot.value);
                        if (index !== -1) {
                          setSelectedBots((prev) => [
                            ...prev.slice(0, index),
                            ...prev.slice(index + 1),
                          ]);
                        }
                      }}
                      className="text-mcm-coral hover:text-mcm-orange font-bold text-sm px-2 py-1"
                    >
                      -
                    </button>
                  </div>
                )}
              </label>
            ))}
          </div>
        </div>

        <div className="pt-4 border-t-2 border-mcm-mustard">
          <div className="flex items-center justify-between mb-4">
            <span className="text-mcm-navy font-bold uppercase tracking-wider">Total Players:</span>
            <span className="text-2xl font-bold text-mcm-navy">{totalPlayers}</span>
          </div>
          <button
            onClick={handleCreateTournament}
            disabled={isCreating || totalPlayers < 2}
            className="w-full btn"
          >
            {isCreating ? 'Creating Tournament...' : 'Create Tournament'}
          </button>
        </div>
      </div>
    </div>
  );
}
