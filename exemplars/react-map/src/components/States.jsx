// src/components/States.jsx: the three states every data panel has besides "done".
// They use the kit's .state classes (components.css), so they match the design kit.
// A panel is never just blank: it says what it is waiting for, or what went wrong.

// Loading: a moving bar plus the request we are waiting on.
export function Loading({ what = 'data', url }) {
  return (
    <div className="card state" role="status" aria-live="polite">
      <span className="t">Loading {what}…</span>
      {url && <code className="note">GET {url}</code>}
      <div className="bar"><i /></div>
    </div>
  );
}

// Empty: the request WORKED but found no rows. Say so, and say what to try.
export function Empty({ hint = 'Try another filter.' }) {
  return (
    <div className="card state warn" role="status">
      <span className="t">No rows for this filter</span>
      <span className="note">{hint}</span>
    </div>
  );
}

// Error: the request FAILED. Say what probably went wrong, and offer a retry.
export function ErrorState({ message, url, onRetry }) {
  return (
    <div className="card state err" role="alert">
      <span className="t">The API did not answer</span>
      <span className="note">{message}</span>
      {url && <code className="note">GET {url}</code>}
      {onRetry && (
        <button className="btn btn-secondary" type="button" onClick={onRetry}>Retry</button>
      )}
    </div>
  );
}
