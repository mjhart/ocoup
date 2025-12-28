// Server configuration from environment
export const SERVER_URL = process.env.NEXT_PUBLIC_SERVER_URL || 'https://ocoup-server-production.up.railway.app';

// Helper to convert HTTP URL to WebSocket URL
export function getWebSocketUrl(httpUrl: string = SERVER_URL): string {
  const url = new URL(httpUrl);
  url.protocol = url.protocol === 'https:' ? 'wss:' : 'ws:';
  return url.origin;
}

export const WS_BASE_URL = getWebSocketUrl();
