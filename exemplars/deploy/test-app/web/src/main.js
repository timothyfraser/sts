// main.js: call the three routes of the test API.
// VITE_API_URL is read at BUILD time (npm run build) and baked into the page.
// With no value set, the page calls a local API on port 8080.
const API = (import.meta.env.VITE_API_URL || 'http://localhost:8080').replace(/\/+$/, '')
document.getElementById('api-url').textContent = API

// Show a result, or say plainly what went wrong.
function show(id, text) {
  document.getElementById(id).textContent = text
}

// GET /echo?msg=...  returns {"msg": "The message is: '...'"}
async function echo(event) {
  event?.preventDefault()
  const msg = document.getElementById('echo-msg').value
  try {
    const res = await fetch(`${API}/echo?msg=${encodeURIComponent(msg)}`)
    show('echo-out', `${res.status}  ${await res.text()}`)
  } catch (err) {
    show('echo-out', `Could not reach the API (${err.message}). Is it running, and does it allow this page's origin (CORS)?`)
  }
}

// POST /sum?a=..&b=..  returns the number. The inputs go in the query string
// with no body, so the browser sends a "simple" request.
async function sum(event) {
  event?.preventDefault()
  const a = document.getElementById('sum-a').value
  const b = document.getElementById('sum-b').value
  try {
    const res = await fetch(`${API}/sum?a=${encodeURIComponent(a)}&b=${encodeURIComponent(b)}`, { method: 'POST' })
    show('sum-out', `${res.status}  ${await res.text()}`)
  } catch (err) {
    show('sum-out', `Could not reach the API (${err.message}).`)
  }
}

// GET /plot returns a PNG. An <img> can load it directly; the timestamp
// forces a fresh histogram each click instead of the cached one.
function plot() {
  const img = document.getElementById('plot-img')
  show('plot-status', 'Loading...')
  img.onload = () => show('plot-status', '')
  img.onerror = () => show('plot-status', 'The plot did not load. Check the API URL above.')
  img.src = `${API}/plot?t=${Date.now()}`
}

document.getElementById('echo-form').addEventListener('submit', echo)
document.getElementById('sum-form').addEventListener('submit', sum)
document.getElementById('plot-btn').addEventListener('click', plot)
echo(); sum(); plot()
