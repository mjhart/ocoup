'use client';

import { useState, useEffect, useRef } from 'react';
import type { ServerMessage, ClientMessage } from './types';
import { ServerMessage as ServerMessageComponent } from './components/ServerMessage';
import { ResponseForm } from './components/ResponseForm';

const server_host = "localhost:8080"

export default function Home() {
  const [events, setEvents] = useState<Array<{type: 'sent' | 'received' | 'system', message: ServerMessage | ClientMessage | string}>>([]);
  const [isConnected, setIsConnected] = useState(false);
  const [isBotGame, setIsBotGame] = useState(false);
  const [playerUrl, setPlayerUrl] = useState<string | null>(null);
  const wsRef = useRef<WebSocket | null>(null);
  const eventsEndRef = useRef<HTMLDivElement>(null);

  const scrollToBottom = () => {
    eventsEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(() => {
    scrollToBottom();
  }, [events]);

  const createBotGame = async () => {
    try {
      setEvents(prev => [...prev, { type: 'system', message: 'Creating a new bot game...' }]);
      
      const response = await fetch(`http://${server_host}/games`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        }
      });
      
      if (!response.ok) {
        throw new Error(`Failed to create game: ${response.status} ${response.statusText}`);
      }
      
      const gameData = await response.json();
      const updatesUrl = `ws://${server_host}${gameData.updates_url}`;
      const botPlayerUrl = `ws://${server_host}${gameData.player_url}`;
      
      setPlayerUrl(botPlayerUrl);
      setIsBotGame(true);
      
      // Add the player URL to the events for the user to see
      setEvents(prev => [...prev, { 
        type: 'system', 
        message: `Bot game created! Connect your bot to: ${botPlayerUrl}` 
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

  const connectWebSocket = (url: string = `ws://${server_host}/new_game`) => {
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
            {!isConnected && (
              <div className="flex gap-2">
                <button
                  onClick={() => connectWebSocket()}
                  className="btn"
                >
                  {events.length > 0 ? 'Reconnect' : 'Start Game'}
                </button>
                <button
                  onClick={createBotGame}
                  className="btn bg-mcm-coral hover:bg-mcm-orange"
                >
                  Bot Game
                </button>
              </div>
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
                  eventClasses += " received";
                } else if (event.type === 'sent') {
                  eventClasses += " sent";
                } else if (event.type === 'system') {
                  eventClasses += " system";
                  
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
              <div className="flex-grow flex justify-between items-center">
                <div className="text-xs uppercase tracking-wider text-mcm-navy font-bold">Signal Strength</div>
                {!isConnected && events.length > 0 && (
                  <div className="flex gap-2">
                    <button 
                      onClick={() => connectWebSocket()}
                      className="text-xs uppercase tracking-wider bg-mcm-coral text-white px-3 py-1 rounded-md font-bold hover:bg-mcm-orange"
                    >
                      Reconnect
                    </button>
                    <button 
                      onClick={createBotGame}
                      className="text-xs uppercase tracking-wider bg-mcm-mustard text-white px-3 py-1 rounded-md font-bold hover:bg-mcm-orange"
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
      </div>
    </main>
  );
} 