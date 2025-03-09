'use client';

import { useState, useEffect, useRef } from 'react';

export default function Home() {
  const [events, setEvents] = useState<Array<{type: 'sent' | 'received' | 'system', content: string}>>([]);
  const [response, setResponse] = useState('');
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
      setEvents(prev => [...prev, { type: 'system', content: 'Connected to server' }]);
    };

    ws.onmessage = (event) => {
      try {
        const parsed = JSON.parse(event.data);
        setEvents(prev => [...prev, { 
          type: 'received', 
          content: JSON.stringify(parsed, null, 2)
        }]);
      } catch {
        setEvents(prev => [...prev, { 
          type: 'received', 
          content: event.data
        }]);
      }
    };

    ws.onclose = () => {
      setIsConnected(false);
      setEvents(prev => [...prev, { type: 'system', content: 'Disconnected from server' }]);
    };

    ws.onerror = (error) => {
      setEvents(prev => [...prev, { type: 'system', content: `WebSocket error: ${error}` }]);
    };

    wsRef.current = ws;
  };

  const sendResponse = () => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      try {
        const parsed = JSON.parse(response);
        const formatted = JSON.stringify(parsed, null, 2);
        wsRef.current.send(response);
        setEvents(prev => [...prev, { type: 'sent', content: formatted }]);
        setResponse('');
      } catch (error) {
        setEvents(prev => [...prev, { type: 'system', content: 'Error: Invalid JSON format' }]);
      }
    }
  };

  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      sendResponse();
    }
  };

  return (
    <main className="min-h-screen p-4 md:p-8 max-w-4xl mx-auto">
      <div className="flex flex-col gap-8">
        <div className="flex items-center justify-between">
          <h1 className="text-2xl md:text-3xl font-semibold text-gray-800">Coup Game Client</h1>
          {!isConnected && (
            <button
              onClick={connectWebSocket}
              className="bg-indigo-600 hover:bg-indigo-700 text-white font-medium py-2 px-4 rounded-lg shadow-sm"
            >
              Start Game
            </button>
          )}
        </div>

        <div className="flex flex-col gap-4 flex-grow">
          <div className="bg-white rounded-xl shadow-sm border min-h-[400px] max-h-[600px] overflow-y-auto p-4">
            {events.map((event, index) => (
              <div key={index} className={`event-message ${event.type}`}>
                <pre className="whitespace-pre-wrap">{event.content}</pre>
              </div>
            ))}
            <div ref={eventsEndRef} />
          </div>

          {isConnected && (
            <div className="flex gap-4">
              <textarea
                value={response}
                onChange={(e) => setResponse(e.target.value)}
                onKeyPress={handleKeyPress}
                placeholder="Enter JSON response..."
                className="flex-1 p-3 rounded-lg border bg-white shadow-sm min-h-[120px] text-sm font-mono"
              />
              <button
                onClick={sendResponse}
                className="bg-indigo-600 hover:bg-indigo-700 text-white font-medium px-6 rounded-lg shadow-sm self-end"
              >
                Send
              </button>
            </div>
          )}
        </div>
      </div>
    </main>
  );
} 