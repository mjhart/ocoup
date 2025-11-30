'use client';

import { useState } from 'react';
import TournamentSetup from './components/TournamentSetup';
import TournamentProgress from './components/TournamentProgress';
import TournamentResults from './components/TournamentResults';

type TournamentState = 'setup' | 'registering' | 'running' | 'completed';

interface TournamentData {
  tournamentId: string;
  numHumanPlayers: number;
  botPlayers: string[];
  serverUrl: string;
}

interface TournamentResults {
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
}

export default function TournamentPage() {
  const [state, setState] = useState<TournamentState>('setup');
  const [tournamentData, setTournamentData] = useState<TournamentData | null>(null);
  const [results, setResults] = useState<TournamentResults | null>(null);
  const [error, setError] = useState<string | null>(null);

  const handleSetupComplete = (data: TournamentData) => {
    setTournamentData(data);
    setState('registering');
  };

  const handleRegistrationComplete = () => {
    setState('running');
  };

  const handleTournamentComplete = (resultsData: TournamentResults) => {
    setResults(resultsData);
    setState('completed');
  };

  const handleError = (errorMessage: string) => {
    setError(errorMessage);
  };

  const handleReset = () => {
    setState('setup');
    setTournamentData(null);
    setResults(null);
    setError(null);
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-mcm-navy via-mcm-brown to-mcm-navy p-8">
      <div className="max-w-6xl mx-auto">
        <div className="mb-8">
          <h1 className="text-4xl font-bold text-white mb-2">🎮 OCoup Tournament</h1>
          <p className="text-mcm-cream">Create and manage multiplayer Coup tournaments</p>
        </div>

        {error && (
          <div className="mb-6 bg-red-500/10 border border-red-500 rounded-lg p-4">
            <p className="text-red-400">❌ {error}</p>
            <button
              onClick={() => setError(null)}
              className="mt-2 text-sm text-red-300 hover:text-red-200 underline"
            >
              Dismiss
            </button>
          </div>
        )}

        {state === 'setup' && (
          <TournamentSetup onComplete={handleSetupComplete} onError={handleError} />
        )}

        {(state === 'registering' || state === 'running') && tournamentData && (
          <TournamentProgress
            tournamentData={tournamentData}
            onRegistrationComplete={handleRegistrationComplete}
            onTournamentComplete={handleTournamentComplete}
            onError={handleError}
            state={state}
          />
        )}

        {state === 'completed' && results && (
          <TournamentResults results={results} onReset={handleReset} />
        )}
      </div>
    </div>
  );
}
