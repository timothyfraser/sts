// src/router.js: a tiny hash router (no library). Two routes:
//   #/            -> the overview (one card per year)
//   #/year/2016   -> the detail page for one year (every month)
// A hash route works on Posit Connect at any URL, because the server never sees it.
import { useEffect, useState } from 'react';

export function parseRoute(hash) {
  const m = /^#\/year\/(\d{4})$/.exec(hash || '');
  return m ? { name: 'detail', year: Number(m[1]) } : { name: 'overview' };
}

export function useRoute() {
  const [route, setRoute] = useState(() => parseRoute(window.location.hash));
  useEffect(() => {
    const on = () => setRoute(parseRoute(window.location.hash));
    window.addEventListener('hashchange', on);
    return () => window.removeEventListener('hashchange', on);
  }, []);
  return route;
}
