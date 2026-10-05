// src/useData.js: run one async loader and give the page { data, loading, error }.
// The loader is re-run when `key` changes; a stale answer never overwrites a new one.
import { useEffect, useState } from 'react';

export function useData(loader, key) {
  const [state, setState] = useState({ data: null, loading: true, error: null });
  useEffect(() => {
    let live = true;
    setState((s) => ({ ...s, loading: true, error: null }));
    loader()
      .then((data) => live && setState({ data, loading: false, error: null }))
      .catch((err) => live && setState({ data: null, loading: false, error: err.message }));
    return () => { live = false; };
  }, [key]); // eslint-disable-line react-hooks/exhaustive-deps
  return state;
}
