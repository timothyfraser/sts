-- 00_extensions.sql
-- Extensions for the STS class database. Used by chapter 6 (PostGIS spatial
-- joins), chapter 7 (ST_DWithin / ST_Distance / ST_SquareGrid) and chapter 13
-- (pg_cron refresh of a materialised view).
--
-- Supabase notes
-- * PostGIS installs into the `extensions` schema, which Supabase creates for
--   every project. On a plain Postgres we create that schema ourselves.
-- * pg_cron must be switched on in the dashboard first:
--   Database > Extensions > search "pg_cron" > enable. It lives in the
--   `postgres` database only (cron jobs are stored in the `cron` schema there),
--   so apply this whole folder to the `postgres` database.
-- * pgcrypto is NOT needed: primary keys are natural keys or identity columns,
--   and no passwords are hashed in SQL (credentials live in env vars).
-- * Local testing without pg_cron: apply.sh sets STS_SKIP_CRON=1 to skip it.

-- Default: do not skip, unless apply.sh passed -v skip_cron=on.
\if :{?skip_cron}
\else
  \set skip_cron off
\endif

CREATE SCHEMA IF NOT EXISTS extensions;

CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA extensions;

\if :skip_cron
  \echo '00_extensions: skipping pg_cron (STS_SKIP_CRON=1)'
\else
  CREATE EXTENSION IF NOT EXISTS pg_cron;
\endif
