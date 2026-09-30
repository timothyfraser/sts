// vite.config.js: how Vite serves the page in development and builds it for deploy.
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

// The development proxy. The browser only ever talks to Vite (port 5173).
// A request to /api/trend?year=2016 is forwarded by Vite to
// http://localhost:8000/trend?year=2016. Because the browser never makes a
// cross-origin request, CORS cannot get in the way while you develop.
const apiProxy = {
  '/api': {
    target: process.env.API_TARGET || 'http://localhost:8000', // where runme.R serves the API
    changeOrigin: true,
    rewrite: (path) => path.replace(/^\/api/, ''),             // drop the /api prefix Plumber doesn't know about
  },
};

export default defineConfig({
  plugins: [react()],
  // base './' makes the built files use relative paths, so the page works at any
  // URL Posit Connect gives it (for example /content/<guid>/), not only at "/".
  base: './',
  server: { port: 5173, strictPort: true, proxy: apiProxy },   // npm run dev
  preview: { port: 4173, strictPort: true, proxy: apiProxy },  // npm run preview (serves dist/)
});
