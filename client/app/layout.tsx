import type { Metadata } from 'next'
import { Playfair_Display } from 'next/font/google'
import './globals.css'
import { Navigation } from './components/Navigation'

const playfairDisplay = Playfair_Display({
  subsets: ['latin'],
  weight: ['400', '700'],
  variable: '--font-playfair',
})

export const metadata: Metadata = {
  title: 'Coup Game Client',
  description: 'A web client for playing Coup',
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en">
      <body className={playfairDisplay.variable}>
        <Navigation />
        {children}
      </body>
    </html>
  )
} 