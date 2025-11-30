'use client';

import Link from 'next/link';
import { usePathname } from 'next/navigation';

export function Navigation() {
  const pathname = usePathname();

  return (
    <nav className="bg-mcm-navy border-b border-mcm-mustard">
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
                  ? 'bg-mcm-charcoal text-white'
                  : 'text-mcm-cream hover:bg-mcm-brown hover:text-white'
              }`}
            >
              Single Game
            </Link>
            <Link
              href="/tournament"
              className={`px-3 py-2 rounded-md text-sm font-medium ${
                pathname === '/tournament'
                  ? 'bg-mcm-charcoal text-white'
                  : 'text-mcm-cream hover:bg-mcm-brown hover:text-white'
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
