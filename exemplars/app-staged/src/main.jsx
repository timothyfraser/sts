// src/main.jsx: load the styles once and mount <App/> into index.html's #root.
import { createRoot } from 'react-dom/client';

// The design kit, COPIED into src/styles/ (never imported from the design folder).
import './styles/tokens.css';
import './styles/components.css';
import './styles/app.css';

import App from './App.jsx';

// No <React.StrictMode> here on purpose: in development it runs every effect
// twice, which would double the request counter and hide the N+1 lesson (6 -> 1).
createRoot(document.getElementById('root')).render(<App />);
