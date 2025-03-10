/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './pages/**/*.{js,ts,jsx,tsx,mdx}',
    './components/**/*.{js,ts,jsx,tsx,mdx}',
    './app/**/*.{js,ts,jsx,tsx,mdx}',
    './src/**/*.{js,ts,jsx,tsx,mdx}',
  ],
  theme: {
    extend: {
      colors: {
        mcm: {
          teal: '#2A9D8F',
          mustard: '#E9C46A',
          orange: '#F4A261',
          coral: '#E76F51',
          cream: '#F8F3E6',
          navy: '#264653',
          brown: '#854D27',
          sage: '#A8DADC',
          offwhite: '#FAF9F7',
          charcoal: '#333333'
        },
        border: '#E9C46A', // mustard
        accent: {
          DEFAULT: '#E76F51', // coral
          dark: '#D05A3E', // darker coral
        },
      },
      fontFamily: {
        sans: [
          'Futura', 
          'Century Gothic',
          'Avenir Next',
          'Gill Sans', 
          'sans-serif'
        ],
        display: [
          'Playfair Display',
          'Georgia',
          'serif'
        ],
        mono: [
          'ui-monospace', 
          'SFMono-Regular', 
          'Menlo', 
          'Monaco', 
          'Consolas', 
          '"Courier New"', 
          'monospace'
        ],
      },
      borderRadius: {
        'xl': '1rem',
        '2xl': '1.5rem',
        '3xl': '2rem',
      },
      boxShadow: {
        'mcm': '0.5rem 0.5rem 0 0 rgba(231, 111, 81, 0.2)',
        'mcm-lg': '1rem 1rem 0 0 rgba(231, 111, 81, 0.2)',
      },
    },
  },
  plugins: [
    require('@tailwindcss/forms'),
  ],
}

