#!/usr/bin/env bash
# apply.sh
# Builds the SYSEN 5460 (STS) class database in one command: runs
# sql/00_extensions.sql .. sql/06_geometry.sql in order against
# $SUPABASE_DB_URL with psql, stops at the first error, then prints a row
# count per table. Used for chapters 1-13 (the class database) and set up
# once per term by the instructor.
#
#   export SUPABASE_DB_URL='postgresql://postgres:<password>@db.<ref>.supabase.co:5432/postgres'
#   export STS_READER_PASSWORD='...'   # optional: lets sts_reader log in
#   export STS_WRITER_PASSWORD='...'   # optional: lets sts_writer log in
#   bash apply.sh
#
# Options (env vars):
#   STS_RESET=1      allow dropping and rebuilding an existing sts schema
#   STS_SKIP_CRON=1  skip pg_cron (local Postgres without the extension)
#   STS_SMOKE=1      also run test/smoke.sql at the end
#
# No credential is stored in this repo: everything comes from env vars.
set -euo pipefail

cd "$(dirname "$0")"

if [ -z "${SUPABASE_DB_URL:-}" ]; then
  echo "apply.sh: SUPABASE_DB_URL is not set (see README.md)." >&2
  exit 2
fi
command -v psql >/dev/null || { echo "apply.sh: psql not found on PATH." >&2; exit 2; }

for f in seed/*.csv; do
  [ -e "$f" ] || { echo "apply.sh: no seed CSVs; run 'Rscript seed/make_seed.R' first." >&2; exit 2; }
done

psql_base=(psql "$SUPABASE_DB_URL" -X -q -v ON_ERROR_STOP=1)

exists=$("${psql_base[@]}" -At -c "SELECT count(*) FROM pg_namespace WHERE nspname = 'sts'")
if [ "$exists" != "0" ] && [ "${STS_RESET:-0}" != "1" ]; then
  echo "apply.sh: schema sts already exists. Re-run with STS_RESET=1 to drop and rebuild it" >&2
  echo "          (this deletes anything written to sts since, e.g. DL Challenge rows)." >&2
  exit 3
fi

vars=(-v skip_cron="$([ "${STS_SKIP_CRON:-0}" = "1" ] && echo on || echo off)")
[ -n "${STS_READER_PASSWORD:-}" ] && vars+=(-v reader_password="$STS_READER_PASSWORD")
[ -n "${STS_WRITER_PASSWORD:-}" ] && vars+=(-v writer_password="$STS_WRITER_PASSWORD")

# One psql session, one transaction for 01..06: a failure leaves no half-built
# schema behind. 00 (extensions) runs first on its own because CREATE
# EXTENSION pg_cron is not transactional in every Supabase setup.
echo "== 00_extensions.sql"
"${psql_base[@]}" "${vars[@]}" -f sql/00_extensions.sql

echo "== 01..06 (single transaction)"
"${psql_base[@]}" "${vars[@]}" --single-transaction \
  -f sql/01_schema.sql \
  -f sql/02_roles.sql \
  -f sql/03_views.sql \
  -f sql/04_cron.sql \
  -f sql/05_seed.sql \
  -f sql/06_geometry.sql

echo "== row counts"
"${psql_base[@]}" -P pager=off <<'SQL'
SELECT c.relname AS "table",
       (xpath('/row/n/text()',
              query_to_xml(format('SELECT count(*) AS n FROM sts.%I', c.relname), false, true, '')))[1]::text::bigint AS "rows"
FROM pg_class c
JOIN pg_namespace n ON n.oid = c.relnamespace
WHERE n.nspname = 'sts' AND c.relkind IN ('r', 'm')
ORDER BY c.relname;
SQL

if [ "${STS_SMOKE:-0}" = "1" ]; then
  echo "== test/smoke.sql"
  "${psql_base[@]}" -P pager=off -f test/smoke.sql
fi

echo "apply.sh: done."
