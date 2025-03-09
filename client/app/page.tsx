'use client';

import { useState, useEffect, useRef } from 'react';
import type { ServerMessage, ClientMessage } from './types';
import { ServerMessage as ServerMessageComponent } from './components/ServerMessage';
import { ResponseForm } from './components/ResponseForm';

export default function Home() {
  const [events, setEvents] = useState<Array<{type: 'sent' | 'received' | 'system', message: ServerMessage | ClientMessage | string}>>([]);
  const [isConnected, setIsConnected] = useState(false);
  const wsRef = useRef<WebSocket | null>(null);
  const eventsEndRef = useRef<HTMLDivElement>(null);

  const scrollToBottom = () => {
    eventsEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(() => {
    scrollToBottom();
  }, [events]);

  const connectWebSocket = () => {
    const ws = new WebSocket('ws://localhost:8080');

    ws.onopen = () => {
      setIsConnected(true);
      setEvents(prev => [...prev, { type: 'system', message: 'Connected to server' }]);
    };

    ws.onmessage = (event) => {
      try {
        const parsed = JSON.parse(event.data) as ServerMessage;
        setEvents(prev => [...prev, { type: 'received', message: parsed }]);
      } catch {
        setEvents(prev => [...prev, { type: 'received', message: event.data }]);
      }
    };

    ws.onclose = () => {
      setIsConnected(false);
      setEvents(prev => [...prev, { type: 'system', message: 'Disconnected from server' }]);
    };

    ws.onerror = (error) => {
      setEvents(prev => [...prev, { type: 'system', message: `WebSocket error: ${error}` }]);
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
    <main className="min-h-screen p-4 max-w-4xl mx-auto md:p-8">
      <div className="flex flex-col gap-8">
        <div className="flex items-center justify-between mb-8">
          <h1 className="text-xl font-semibold text-gray-800 md:text-3xl">Coup Game Client</h1>
          {!isConnected && (
            <button
              onClick={connectWebSocket}
              className="py-2 px-4 bg-indigo-600 text-white border-none rounded-lg shadow-sm transition-all duration-200 cursor-pointer text-sm font-medium hover:bg-indigo-700 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              Start Game
            </button>
          )}
        </div>

        <div className="flex flex-col gap-4 flex-grow">
          <div className="bg-white rounded-xl shadow-sm border border-gray-200 min-h-[400px] max-h-[600px] overflow-y-auto p-4">
            {events.map((event, index) => {
              let eventClasses = "p-3 rounded-lg mb-2 text-sm font-mono";
              
              if (event.type === 'received') {
                eventClasses += " bg-white border border-gray-100 shadow-sm";
              } else if (event.type === 'sent') {
                eventClasses += " bg-indigo-50 border border-indigo-100";
              } else if (event.type === 'system') {
                eventClasses += " bg-gray-50 border border-gray-100 text-gray-600";
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

          {isConnected && (
            <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-4">
              <ResponseForm 
                lastMessage={lastServerMessage}
                onSubmit={sendResponse}
              />
            </div>
          )}
        </div>
      </div>
    </main>
  );
} 