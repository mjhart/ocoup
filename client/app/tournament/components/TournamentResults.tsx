'use client';

interface TournamentResultsProps {
  results: {
    results: Array<{
      round: number;
      games: Array<{
        game: number;
        status: string;
        winners?: string[];
        eliminated?: string[];
        error?: string;
      }>;
    }>;
    scores: Record<string, number>;
  };
  onReset: () => void;
}

export default function TournamentResults({ results, onReset }: TournamentResultsProps) {
  const sortedScores = Object.entries(results.scores)
    .map(([playerId, score]) => ({ playerId, score }))
    .sort((a, b) => b.score - a.score);

  const getMedal = (index: number) => {
    if (index === 0) return '🥇';
    if (index === 1) return '🥈';
    if (index === 2) return '🥉';
    return '';
  };

  return (
    <div className="space-y-6">
      <div className="bg-slate-800/50 backdrop-blur-sm rounded-xl p-8 border border-slate-700">
        <div className="flex items-center justify-between mb-6">
          <h2 className="text-2xl font-bold text-white">🏆 Tournament Complete!</h2>
          <button
            onClick={onReset}
            className="bg-purple-600 hover:bg-purple-700 text-white font-semibold py-2 px-4 rounded-lg transition-colors"
          >
            New Tournament
          </button>
        </div>

        <div className="mb-8">
          <h3 className="text-xl font-semibold text-white mb-4">📊 Final Scores</h3>
          <div className="space-y-2">
            {sortedScores.map((entry, index) => (
              <div
                key={entry.playerId}
                className={`bg-slate-900/50 rounded-lg p-4 flex items-center justify-between ${
                  index === 0 ? 'border-2 border-yellow-400' : 'border border-slate-700'
                }`}
              >
                <div className="flex items-center gap-3">
                  <span className="text-2xl w-10">{getMedal(index)}</span>
                  <span className="text-white font-semibold">Player {entry.playerId}</span>
                </div>
                <span className="text-2xl font-bold text-purple-400">{entry.score}</span>
              </div>
            ))}
          </div>

          {sortedScores.length > 0 && (
            <div className="mt-6 text-center">
              <p className="text-xl text-white">
                🎉 Winner: <span className="font-bold text-yellow-400">Player {sortedScores[0].playerId}</span>
              </p>
              <p className="text-lg text-slate-300 mt-2">
                with {sortedScores[0].score} points!
              </p>
            </div>
          )}
        </div>

        {results.results && (
          <div>
            <h3 className="text-xl font-semibold text-white mb-4">📋 Game Results</h3>
            <div className="space-y-4">
              {results.results.map((round) => (
                <div
                  key={round.round}
                  className="bg-slate-900/50 rounded-lg p-4 border border-slate-700"
                >
                  <h4 className="text-lg font-semibold text-purple-400 mb-3">
                    🎲 Round {round.round}
                  </h4>
                  <div className="space-y-3">
                    {round.games.map((game) => (
                      <div key={game.game} className="pl-4 border-l-2 border-slate-700">
                        {game.status === 'completed' ? (
                          <div>
                            <p className="text-white font-medium mb-1">Game {game.game}</p>
                            {game.winners && game.winners.length > 0 && (
                              <p className="text-sm text-green-400">
                                🏆 Winner(s): {game.winners.join(', ')}
                              </p>
                            )}
                            {game.eliminated && game.eliminated.length > 0 && (
                              <p className="text-sm text-slate-400">
                                ❌ Eliminated (in order): {game.eliminated.join(', ')}
                              </p>
                            )}
                          </div>
                        ) : (
                          <div>
                            <p className="text-white font-medium mb-1">Game {game.game}</p>
                            <p className="text-sm text-red-400">⚠️ Error: {game.error}</p>
                          </div>
                        )}
                      </div>
                    ))}
                  </div>
                </div>
              ))}
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
