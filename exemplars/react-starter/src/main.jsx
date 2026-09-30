// src/main.jsx: the entry file. It loads the styles once and mounts <App/> into index.html's #root.
import React from 'react';
import { createRoot } from 'react-dom/client';

// The design kit, COPIED into src/styles/ (never imported from the design folder,
// because the deployed page will not have that folder next to it).
import './styles/tokens.css';
import './styles/components.css';
import './styles/app.css';

import App from './App.jsx';

createRoot(document.getElementById('root')).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
