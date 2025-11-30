'use client';

import { useEffect, useState, useRef, useCallback } from 'react';
import { handleGameMessage } from '../utils/gameMessageHandler';

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

interface TournamentProgressProps {
  tournamentData: {
    tournamentId: string;
    numHumanPlayers: number;
    botPlayers: string[];
    serverUrl: string;
  };
  onRegistrationComplete: () => void;
  onTournamentComplete: (results: TournamentResults) => void;
  onError: (error: string) => void;
  state: 'registering' | 'running';
}

interface Player {
  ws: WebSocket;
  playerId: string;
  playerNum: number;
  status: 'connecting' | 'registered' | 'error';
}

export default function TournamentProgress({
  tournamentData,
  onRegistrationComplete,
  onTournamentComplete,
  onError,
  state,
}: TournamentProgressProps) {
  const [players, setPlayers] = useState<Player[]>([]);
  const [isStarting, setIsStarting] = useState(false);
  const [logs, setLogs] = useState<string[]>([]);
  const playersRef = useRef<Player[]>([]);
  const hasStarted = useRef(false);
  const hasRegistered = useRef(false);

  const addLog = (message: string) => {
    setLogs((prev) => [...prev, `[${new Date().toLocaleTimeString()}] ${message}`]);
  };

  useEffect(() => {
    playersRef.current = players;
  }, [players]);

  const startTournament = useCallback(async () => {
    setIsStarting(true);
    addLog('Starting tournament...');

    try {
      const response = await fetch(
        `${tournamentData.serverUrl}/tournaments/${tournamentData.tournamentId}/start`,
        {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
        }
      );

      if (!response.ok) {
        const error = await response.text();
        throw new Error(`Failed to start tournament: ${error}`);
      }

      const data = await response.json();

      if (data.error) {
        throw new Error(data.error);
      }

      addLog('Tournament completed!');
      onTournamentComplete(data);
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to start tournament';
      addLog(`Error: ${errorMessage}`);
      onError(errorMessage);
    } finally {
      setIsStarting(false);
    }
  }, [tournamentData.serverUrl, tournamentData.tournamentId, onTournamentComplete, onError]);

  const registerPlayer = useCallback((wsUrl: string, playerNum: number): Promise<void> => {
    return new Promise((resolve, reject) => {
      const ws = new WebSocket(
        `${wsUrl}/tournaments/${tournamentData.tournamentId}/register`
      );
      let registered = false;

      const player: Player = {
        ws,
        playerId: '',
        playerNum,
        status: 'connecting',
      };

      setPlayers((prev) => [...prev, player]);

      ws.onopen = () => {
        addLog(`Player ${playerNum}: Connecting...`);
      };

      ws.onmessage = (event) => {
        const message = JSON.parse(event.data);

        if (message.error) {
          addLog(`Player ${playerNum}: Error - ${message.error}`);
          player.status = 'error';
          ws.close();
          reject(new Error(message.error));
          return;
        }

        if (message.status === 'registered') {
          addLog(`Player ${playerNum}: Registered (ID: ${message.player_id})`);
          player.playerId = message.player_id;
          player.status = 'registered';
          registered = true;

          setPlayers((prev) =>
            prev.map((p) => (p.playerNum === playerNum ? player : p))
          );

          const allRegistered = playersRef.current.every((p) => p.status === 'registered');
          if (allRegistered && playersRef.current.length === tournamentData.numHumanPlayers) {
            addLog('All players registered!');
            onRegistrationComplete();
            if (!hasStarted.current) {
              hasStarted.current = true;
              startTournament();
            }
          }

          resolve();
          return;
        }

        handleGameMessage(ws, message, playerNum, addLog);
      };

      ws.onerror = () => {
        if (!registered) {
          addLog(`Player ${playerNum}: Connection error`);
          player.status = 'error';
          reject(new Error('Connection error'));
        }
      };

      ws.onclose = () => {
        if (!registered) {
          reject(new Error('Connection closed before registration'));
        }
      };
    });
  }, [tournamentData.tournamentId, tournamentData.numHumanPlayers, onRegistrationComplete, startTournament]);

  const registerAllPlayers = useCallback(async () => {
    const wsProtocol = tournamentData.serverUrl.startsWith('https') ? 'wss' : 'ws';
    const wsUrl = tournamentData.serverUrl.replace(/^https?/, wsProtocol);

    for (let i = 0; i < tournamentData.numHumanPlayers; i++) {
      await registerPlayer(wsUrl, i);
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
  }, [tournamentData.serverUrl, tournamentData.numHumanPlayers, registerPlayer]);

  useEffect(() => {
    if (hasRegistered.current) {
      console.log('Registration already initiated, skipping');
      return;
    }

    hasRegistered.current = true;

    if (tournamentData.numHumanPlayers === 0) {
      addLog('No human players to register (bots only)');
      startTournament();
      return;
    }

    addLog(`Registering ${tournamentData.numHumanPlayers} human player(s)...`);
    registerAllPlayers();

    return () => {
      playersRef.current.forEach((player) => {
        if (player.ws.readyState === WebSocket.OPEN) {
          player.ws.close();
        }
      });
    };
  }, [tournamentData.numHumanPlayers, registerAllPlayers, startTournament]);

  return (
    <div className="bg-mcm-navy/50 backdrop-blur-xs rounded-xl p-8 border border-mcm-mustard">
      <h2 className="text-2xl font-bold text-white mb-6">
        {state === 'registering' ? '👥 Registering Players' : '🎮 Tournament in Progress'}
      </h2>

      <div className="mb-6">
        <h3 className="text-lg font-semibold text-white mb-3">Tournament Info</h3>
        <div className="bg-mcm-charcoal/50 rounded-lg p-4 space-y-2">
          <div className="flex justify-between text-sm">
            <span className="text-mcm-sage">Tournament ID:</span>
            <span className="text-white font-mono">{tournamentData.tournamentId}</span>
          </div>
          <div className="flex justify-between text-sm">
            <span className="text-mcm-sage">Human Players:</span>
            <span className="text-white">{tournamentData.numHumanPlayers}</span>
          </div>
          {tournamentData.botPlayers.length > 0 && (
            <div className="flex justify-between text-sm">
              <span className="text-mcm-sage">Bot Players:</span>
              <span className="text-white">{tournamentData.botPlayers.join(', ')}</span>
            </div>
          )}
        </div>
      </div>

      {players.length > 0 && (
        <div className="mb-6">
          <h3 className="text-lg font-semibold text-white mb-3">Player Status</h3>
          <div className="space-y-2">
            {players.map((player) => (
              <div
                key={player.playerNum}
                className="bg-mcm-charcoal/50 rounded-lg p-3 flex items-center justify-between"
              >
                <span className="text-white">Player {player.playerNum}</span>
                <span
                  className={`text-sm ${
                    player.status === 'registered'
                      ? 'text-green-400'
                      : player.status === 'error'
                      ? 'text-red-400'
                      : 'text-yellow-400'
                  }`}
                >
                  {player.status === 'registered' && '✅ Registered'}
                  {player.status === 'connecting' && '⏳ Connecting...'}
                  {player.status === 'error' && '❌ Error'}
                </span>
              </div>
            ))}
          </div>
        </div>
      )}

      <div>
        <h3 className="text-lg font-semibold text-white mb-3">Activity Log</h3>
        <div className="bg-mcm-charcoal/50 rounded-lg p-4 h-64 overflow-y-auto font-mono text-sm">
          {logs.map((log, index) => (
            <div key={index} className="text-mcm-cream mb-1">
              {log}
            </div>
          ))}
          {isStarting && (
            <div className="text-mcm-coral animate-pulse">
              🚀 Starting tournament...
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
