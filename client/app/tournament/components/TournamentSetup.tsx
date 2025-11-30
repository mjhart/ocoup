'use client';

import { useState } from 'react';

interface TournamentSetupProps {
  onComplete: (data: {
    tournamentId: string;
    numHumanPlayers: number;
    botPlayers: string[];
    serverUrl: string;
  }) => void;
  onError: (error: string) => void;
}

const BOT_TYPES = [
  { value: 'gpt-4o-mini', label: 'GPT-4o-mini', description: 'OpenAI GPT-4o-mini' },
  { value: 'o3-mini', label: 'O3-mini', description: 'OpenAI O3-mini' },
  { value: 'gemini-2-5', label: 'Gemini 2.5', description: 'Google Gemini 2.5' },
];

export default function TournamentSetup({ onComplete, onError }: TournamentSetupProps) {
  const [serverUrl, setServerUrl] = useState('http://localhost:9000');
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

      const response = await fetch(`${serverUrl}/tournaments`, {
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
        serverUrl,
      });
    } catch (error) {
      onError(error instanceof Error ? error.message : 'Failed to create tournament');
      setIsCreating(false);
    }
  };

  return (
    <div className="bg-mcm-navy/50 backdrop-blur-xs rounded-xl p-8 border border-mcm-mustard">
      <h2 className="text-2xl font-bold text-white mb-6">Tournament Setup</h2>

      <div className="space-y-6">
        <div>
          <label className="block text-sm font-medium text-mcm-cream mb-2">
            Server URL
          </label>
          <input
            type="text"
            value={serverUrl}
            onChange={(e) => setServerUrl(e.target.value)}
            className="w-full bg-mcm-charcoal/50 border border-mcm-brown rounded-lg px-4 py-2 text-white placeholder-mcm-sage focus:outline-hidden focus:ring-2 focus:ring-accent"
            placeholder="http://localhost:9000"
          />
        </div>

        <div>
          <label className="block text-sm font-medium text-mcm-cream mb-2">
            Number of Human Players
          </label>
          <input
            type="number"
            min="0"
            max="20"
            value={numHumanPlayers}
            onChange={(e) => setNumHumanPlayers(parseInt(e.target.value) || 0)}
            className="w-full bg-mcm-charcoal/50 border border-mcm-brown rounded-lg px-4 py-2 text-white focus:outline-hidden focus:ring-2 focus:ring-accent"
          />
          <p className="mt-1 text-sm text-mcm-sage">
            Each human player will auto-play using simple AI logic
          </p>
        </div>

        <div>
          <label className="block text-sm font-medium text-mcm-cream mb-3">
            Bot Players (Optional)
          </label>
          <div className="space-y-2">
            {BOT_TYPES.map((bot) => (
              <label
                key={bot.value}
                className="flex items-center gap-3 p-3 bg-mcm-charcoal/50 border border-mcm-brown rounded-lg cursor-pointer hover:border-accent transition-colors"
              >
                <input
                  type="checkbox"
                  checked={selectedBots.includes(bot.value)}
                  onChange={() => handleBotToggle(bot.value)}
                  className="w-4 h-4 rounded-sm border-mcm-brown text-accent focus:ring-accent focus:ring-offset-mcm-charcoal"
                />
                <div className="flex-1">
                  <div className="text-white font-medium">{bot.label}</div>
                  <div className="text-sm text-mcm-sage">{bot.description}</div>
                </div>
                {selectedBots.includes(bot.value) && (
                  <div className="flex items-center gap-2">
                    <button
                      type="button"
                      onClick={(e) => {
                        e.preventDefault();
                        setSelectedBots((prev) => [...prev, bot.value]);
                      }}
                      className="text-mcm-coral hover:text-mcm-orange text-sm px-2 py-1"
                    >
                      +
                    </button>
                    <span className="text-mcm-sage text-sm">
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
                      className="text-mcm-coral hover:text-mcm-orange text-sm px-2 py-1"
                    >
                      -
                    </button>
                  </div>
                )}
              </label>
            ))}
          </div>
        </div>

        <div className="pt-4 border-t border-mcm-mustard">
          <div className="flex items-center justify-between mb-4">
            <span className="text-mcm-cream">Total Players:</span>
            <span className="text-2xl font-bold text-white">{totalPlayers}</span>
          </div>
          <button
            onClick={handleCreateTournament}
            disabled={isCreating || totalPlayers < 2}
            className="w-full bg-accent hover:bg-accent-dark disabled:bg-mcm-brown disabled:cursor-not-allowed text-white font-semibold py-3 px-6 rounded-lg transition-colors"
          >
            {isCreating ? 'Creating Tournament...' : 'Create Tournament'}
          </button>
        </div>
      </div>
    </div>
  );
}
