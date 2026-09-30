// vite.config.js
// base: './' makes every asset link relative, so the same dist/ works at the
// root of a DigitalOcean URL AND under a Posit Connect content path.
import { defineConfig } from 'vite'

export default defineConfig({
  base: './',
})
