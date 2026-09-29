# api-plumber: the chapter 4 R API

A small Plumber API your React starter (`http://localhost:5173`) can call.

## Run it (one command, from the repo root)

```bash
Rscript exemplars/api-plumber/test_smoke.R
```

It starts the API locally, runs `testme.R` against it, stops it, and exits 0 if every check passes.
Needs R 4.3 with `plumber`, `dplyr`, `readr`, `sf`, `jsonlite`, `httr2`.

To keep the API running for your React app:

```bash
Rscript exemplars/api-plumber/runme.R      # http://localhost:8000
Rscript exemplars/api-plumber/testme.R     # in a second terminal
```

## Endpoints

| Route | Returns |
|---|---|
| `GET /health` | `{status, service, time}` |
| `GET /trend?year=` | `jp_solar` by month: mean `solar_rate`, total `solar`, number of municipalities |
| `GET /spatial-join?ward=&min_places=` | Boston polling places counted per precinct with `sf::st_join`, plus turnout; `ward` (e.g. `01`) filters |
| `GET /predict?month=` | **Stub.** `{stub: true, month, predicted_solar_rate, model}`, a fixed baseline mean |

## CORS

Allowed origin is `ALLOWED_ORIGIN`, default `http://localhost:5173` (the React starter).

## Helpers (same roles as in DSAI)

`runme.R` run locally, `testme.R` smoke test, `manifestme.R` write the Connect manifest, `deployme.R` deploy to Posit Connect (reads `.env`; see `.env.example`). No secrets in git.

## Where Supabase slots in

`/trend` and `/spatial-join` read local files from `data/` through two functions in `plumber.R`, `read_solar()` and `read_spatial()`. Once the Supabase project exists, replace those two functions with queries (URL and key from env vars) and leave the endpoints unchanged.

## Richer next step

The DSAI course's `08_function_calling/mcp_plumber` exposes R functions as MCP tools (JSON-RPC over `POST /mcp`) so an LLM agent can call them. Start there once these endpoints work; it is not needed for this chapter.
