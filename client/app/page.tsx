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
    <main className="main">
      <div className="flex-col gap-8">
        <div className="header">
          <h1 className="title">Coup Game Client</h1>
          {!isConnected && (
            <button
              onClick={connectWebSocket}
              className="btn"
            >
              Start Game
            </button>
          )}
        </div>

        <div className="content">
          <div className="messages">
            {events.map((event, index) => (
              <div key={index} className={`event-message ${event.type}`}>
                {typeof event.message === 'string' ? (
                  <div>{event.message}</div>
                ) : event.type === 'received' ? (
                  <ServerMessageComponent message={event.message as ServerMessage} />
                ) : (
                  <pre className="whitespace-pre-wrap">{JSON.stringify(event.message, null, 2)}</pre>
                )}
              </div>
            ))}
            <div ref={eventsEndRef} />
          </div>

          {isConnected && (
            <div className="response-form">
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