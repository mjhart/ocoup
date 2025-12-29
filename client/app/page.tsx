'use client';

import { useState, useEffect, useRef } from 'react';
import type { ServerMessage, ClientMessage } from './types';
import { ServerMessage as ServerMessageComponent } from './components/ServerMessage';
import { ResponseForm } from './components/ResponseForm';
import { BotHelp } from './components/BotHelp';
import { SERVER_URL, WS_BASE_URL } from './config';

const BOT_TYPES = [
  { value: 'gpt-5-mini', label: 'GPT-5-mini', description: 'OpenAI GPT-5-mini' },
  { value: 'gpt-5-nano', label: 'GPT-5-nano', description: 'OpenAI GPT-5-nano' },
  { value: 'o3-mini', label: 'O3-mini', description: 'OpenAI O3-mini' },
  { value: 'gemini-2-5', label: 'Gemini 2.5', description: 'Google Gemini 2.5' },
];

export default function Home() {
  const [events, setEvents] = useState<Array<{type: 'sent' | 'received' | 'system', message: ServerMessage | ClientMessage | string}>>([]);
  const [isConnected, setIsConnected] = useState(false);
  const [isBotGame, setIsBotGame] = useState(false);
  const [playerUrl, setPlayerUrl] = useState<string | null>(null);
  const [showBotSetup, setShowBotSetup] = useState(false);
  const [selectedBots, setSelectedBots] = useState<string[]>(['gpt-5-nano']);
  const wsRef = useRef<WebSocket | null>(null);
  const eventsEndRef = useRef<HTMLDivElement>(null);

  const scrollToBottom = () => {
    eventsEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(() => {
    if (events.length > 0) {
      scrollToBottom();
    }
  }, [events]);

  const handleBotToggle = (botType: string) => {
    setSelectedBots((prev) =>
      prev.includes(botType) ? prev.filter((b) => b !== botType) : [...prev, botType]
    );
  };

  const createBotGame = async () => {
    try {
      const botList = selectedBots.length > 0 ? selectedBots : ['gpt-5-mini', 'gpt-5-nano'];
      setEvents(prev => [...prev, { type: 'system', message: `Creating a new bot game with: ${botList.join(', ')}...` }]);
      setShowBotSetup(false);

      const response = await fetch(`${SERVER_URL}/games`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ bot_players: botList })
      });

      if (!response.ok) {
        throw new Error(`Failed to create game: ${response.status} ${response.statusText}`);
      }

      const gameData = await response.json();
      const updatesUrl = `${WS_BASE_URL}${gameData.updates_url}`;
      const botPlayerUrl = `${WS_BASE_URL}${gameData.player_url}`;

      setPlayerUrl(botPlayerUrl);
      setIsBotGame(true);

      // Add the player URL to the events for the user to see
      setEvents(prev => [...prev, {
        type: 'system',
        message: `Bot game created with ${gameData.num_bot_players} bot(s)! Connect your bot to: ${botPlayerUrl}`
      }]);

      // Connect to the updates WebSocket
      connectWebSocket(updatesUrl);

    } catch (error) {
      setEvents(prev => [...prev, {
        type: 'system',
        message: `Error creating bot game: ${error instanceof Error ? error.message : String(error)}`
      }]);
    }
  };

  const connectWebSocket = (url: string = `${WS_BASE_URL}/new_game`) => {
    // Close any existing connection
    if (wsRef.current) {
      wsRef.current.close();
    }
    
    const ws = new WebSocket(url);

    ws.onopen = () => {
      setIsConnected(true);
      setEvents(prev => [...prev, { 
        type: 'system', 
        message: prev.length > 0 ? 'Reconnected to server' : 'Connected to server' 
      }]);
    };

    ws.onmessage = (event) => {
      try {
        const parsed = JSON.parse(event.data) as ServerMessage;
        setEvents(prev => [...prev, { type: 'received', message: parsed }]);
      } catch {
        setEvents(prev => [...prev, { type: 'received', message: event.data }]);
      }
    };

    ws.onclose = (event) => {
      setIsConnected(false);
      const message = event.wasClean 
        ? `Disconnected from server: ${event.reason || 'Connection closed cleanly'}`
        : 'Connection lost. The server may be down or unavailable.';
      
      setEvents(prev => [...prev, { 
        type: 'system', 
        message 
      }]);
    };

    ws.onerror = (error) => {
      setIsConnected(false);
      setEvents(prev => [...prev, { 
        type: 'system', 
        message: `WebSocket error: ${error}` 
      }]);
    };

    wsRef.current = ws;
  };

  const sendResponse = (response: ClientMessage) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      const message = JSON.stringify(response);
      wsRef.current.send(message);
      setEvents(prev => [...prev, { type: 'sent', message: response }]);
    }
  };

  const lastServerMessage = events
    .filter((e): e is { type: 'received', message: ServerMessage } => 
      e.type === 'received' && typeof e.message !== 'string')
    .map(e => e.message)
    .pop() || null;

  return (
    <main className="main">
      <div className="flex flex-col gap-8">
        <div className="header">
          <h1 className="title">COUP-O-MATIC 3000</h1>
          <div className="flex items-center gap-3">
            <div className="mcm-dial"></div>
            {!isConnected && !showBotSetup && (
              <div className="flex gap-2">
                <button
                  onClick={() => connectWebSocket()}
                  className="btn"
                >
                  {events.length > 0 ? 'Reconnect' : 'Start Game'}
                </button>
                <button
                  onClick={() => setShowBotSetup(true)}
                  className="btn"
                >
                  Bot Game
                </button>
              </div>
            )}
            {showBotSetup && (
              <button
                onClick={() => setShowBotSetup(false)}
                className="btn"
              >
                Cancel
              </button>
            )}
            {isConnected && (
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-mcm-teal animate-pulse"></div>
                <span className="uppercase text-xs font-bold tracking-wider text-mcm-teal">
                  {isBotGame ? 'Bot Game Connected' : 'Connected'}
                </span>
              </div>
            )}
          </div>
        </div>

        {showBotSetup && (
          <div className="mcm-panel">
            <h2 className="font-display text-xl text-mcm-navy mb-3 pb-2 border-b-2 border-mcm-mustard">BOT GAME SETUP</h2>
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-bold text-mcm-navy mb-3 uppercase tracking-wider">
                  Select Bot Opponents
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
                  <span className="text-mcm-navy font-bold uppercase tracking-wider">Total Bots:</span>
                  <span className="text-2xl font-bold text-mcm-navy">{selectedBots.length}</span>
                </div>
                <p className="text-sm text-mcm-brown mb-4">
                  You will connect as an additional player via WebSocket.
                </p>
                <button
                  onClick={createBotGame}
                  disabled={selectedBots.length === 0}
                  className="w-full btn"
                >
                  Create Bot Game
                </button>
              </div>
            </div>
          </div>
        )}

        <div className="content">
          <div className="mcm-panel">
            <div className="flex justify-between items-center mb-3 pb-2 border-b-2 border-mcm-mustard">
              <h2 className="font-display text-xl text-mcm-navy">GAME CONSOLE</h2>
              <div className="flex gap-2 items-center">
                {!isConnected && events.length > 0 && (
                  <div className="bg-mcm-orange px-3 py-1 rounded-full text-white uppercase text-xs font-bold tracking-wider mr-2 animate-pulse">
                    Disconnected
                  </div>
                )}
                <div className="w-3 h-3 rounded-full bg-mcm-coral"></div>
                <div className="w-3 h-3 rounded-full bg-mcm-mustard"></div>
                <div className={`w-3 h-3 rounded-full ${isConnected ? 'bg-mcm-teal' : 'bg-mcm-orange'}`}></div>
              </div>
            </div>
            
            <div className="bg-mcm-offwhite rounded-xl min-h-[400px] max-h-[600px] overflow-y-auto p-4 border-2 border-mcm-mustard">
              {events.map((event, index) => {
                let eventClasses = "event-message";
                
                if (event.type === 'received') {
                  eventClasses += " received ml-[20%]";
                } else if (event.type === 'sent') {
                  eventClasses += " sent mr-[20%]";
                } else if (event.type === 'system') {
                  eventClasses += " system mx-[10%] text-center";
                  
                  // Add special styling for disconnect messages
                  if (typeof event.message === 'string' && 
                      (event.message.includes('Disconnected') || 
                       event.message.includes('Connection lost'))) {
                    eventClasses += " disconnected";
                  }
                }
                
                return (
                  <div key={index} className={eventClasses}>
                    {typeof event.message === 'string' ? (
                      <div>{event.message}</div>
                    ) : event.type === 'received' ? (
                      <ServerMessageComponent message={event.message as ServerMessage} />
                    ) : (
                      <pre className="whitespace-pre-wrap">{JSON.stringify(event.message, null, 2)}</pre>
                    )}
                  </div>
                );
              })}
              <div ref={eventsEndRef} />
            </div>
            
            <div className="mcm-control-panel">
              <div className="mcm-dial"></div>
              <div className="mcm-dial"></div>
              <div className="grow flex justify-between items-center">
                <div className="text-xs uppercase tracking-wider text-mcm-navy font-bold">Signal Strength</div>
                {!isConnected && events.length > 0 && !showBotSetup && (
                  <div className="flex gap-2">
                    <button
                      onClick={() => connectWebSocket()}
                      className="btn-small"
                    >
                      Reconnect
                    </button>
                    <button
                      onClick={() => setShowBotSetup(true)}
                      className="btn-small"
                    >
                      Bot Game
                    </button>
                  </div>
                )}
              </div>
            </div>
          </div>

          {isConnected && !isBotGame && (
            <div className="mcm-panel">
              <h2 className="font-display text-xl text-mcm-navy mb-3 pb-2 border-b-2 border-mcm-mustard">CONTROL DECK</h2>
              <ResponseForm 
                lastMessage={lastServerMessage}
                onSubmit={sendResponse}
              />
            </div>
          )}
          
          {isConnected && isBotGame && playerUrl && (
            <div className="mcm-panel">
              <h2 className="font-display text-xl text-mcm-navy mb-3 pb-2 border-b-2 border-mcm-mustard">BOT INFO</h2>
              <div className="bg-mcm-offwhite rounded-xl p-4 border-2 border-mcm-mustard">
                <div className="mb-3">
                  <h3 className="font-bold text-mcm-navy mb-1">Player WebSocket URL:</h3>
                  <div className="bg-mcm-navy text-white p-2 rounded-md font-mono text-xs overflow-x-auto">
                    {playerUrl}
                  </div>
                </div>
                <p className="text-sm text-mcm-navy mb-2">
                  Connect your bot to this WebSocket URL to play the game.
                </p>
                <p className="text-sm text-mcm-navy">
                  You are currently viewing game updates in spectator mode.
                </p>
              </div>
            </div>
          )}
        </div>
        
        {/* Add the Bot Help component */}
        <BotHelp />
      </div>
    </main>
  );
} 