'use client';

import Link from 'next/link';
import { usePathname } from 'next/navigation';

export function Navigation() {
  const pathname = usePathname();

  return (
    <nav className="bg-slate-800 border-b border-slate-700">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-16">
          <div className="flex items-center">
            <Link href="/" className="text-xl font-bold text-white">
              🎮 OCoup
            </Link>
          </div>
          <div className="flex space-x-4">
            <Link
              href="/"
              className={`px-3 py-2 rounded-md text-sm font-medium ${
                pathname === '/'
                  ? 'bg-slate-900 text-white'
                  : 'text-slate-300 hover:bg-slate-700 hover:text-white'
              }`}
            >
              Single Game
            </Link>
            <Link
              href="/tournament"
              className={`px-3 py-2 rounded-md text-sm font-medium ${
                pathname === '/tournament'
                  ? 'bg-slate-900 text-white'
                  : 'text-slate-300 hover:bg-slate-700 hover:text-white'
              }`}
            >
              Tournament
            </Link>
          </div>
        </div>
      </div>
    </nav>
  );
}
