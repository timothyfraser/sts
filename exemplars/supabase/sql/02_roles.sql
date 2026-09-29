-- 02_roles.sql
-- Class roles and row-level security for schema sts.
-- Used by chapter 1 ("connect to the class DB and query it"), chapter 5
-- (Supabase for real; DL Challenge writes) and every chapter that connects.
--
--   sts_reader  the shared class login. SELECT on schema sts only. No INSERT,
--               UPDATE, DELETE, TRUNCATE or DDL. 30 s statement timeout.
--   sts_writer  instructor loads and the DL Challenge (option B) writes:
--               SELECT, INSERT, UPDATE, DELETE on sts tables. No DDL.
--
-- Scope: sts_reader gets USAGE on schema sts and on schema `extensions` (so
-- PostGIS functions such as ST_Intersects resolve). It is granted nothing in
-- public, auth, storage, cron or any other schema; Supabase grants tables in
-- those schemas to its own roles (anon, authenticated, service_role), never to
-- PUBLIC, so sts_reader cannot read them.
--
-- RLS is enabled on every sts table with a permissive SELECT policy for
-- sts_reader and a read-write policy for sts_writer. The table owner
-- (postgres) bypasses RLS, which is what apply.sh relies on to seed.
--
-- ---------------------------------------------------------------------------
-- How students get the reader credentials (no secret is ever in git)
-- ---------------------------------------------------------------------------
-- Passwords are never written in this file. apply.sh passes them in from env
-- vars, and only if they are set:
--   STS_READER_PASSWORD -> psql -v reader_password=...  (enables LOGIN)
--   STS_WRITER_PASSWORD -> psql -v writer_password=...  (enables LOGIN)
-- Without them the roles are created NOLOGIN, which is safe.
--
-- Students copy the course .env.example (same pattern as
-- workshops/8P_database/env.example) to .env and fill in the values the
-- instructor posts on Canvas; .env is git-ignored:
--   SUPABASE_HOST=aws-0-<region>.pooler.supabase.com   (or db.<ref>.supabase.co)
--   SUPABASE_PORT=5432
--   SUPABASE_DB=postgres
--   SUPABASE_USER=sts_reader.<project-ref>   (the pooler wants user.<ref>)
--   SUPABASE_PASSWORD=<the reader password>
-- R reads them with Sys.getenv() after readRenviron(".env") or dotenv; the
-- reader password is low-risk by design because the role can only SELECT.
-- Rotate it each term: ALTER ROLE sts_reader PASSWORD '...' from the SQL editor.

SET search_path = sts, extensions, public;

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'sts_reader') THEN
    CREATE ROLE sts_reader NOLOGIN NOINHERIT;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'sts_writer') THEN
    CREATE ROLE sts_writer NOLOGIN NOINHERIT;
  END IF;
END
$$;

COMMENT ON ROLE sts_reader IS 'SYSEN 5460 class login: SELECT on schema sts only.';
COMMENT ON ROLE sts_writer IS 'SYSEN 5460 instructor loads and DL Challenge writes on schema sts.';

ALTER ROLE sts_reader SET search_path = sts, extensions;
ALTER ROLE sts_writer SET search_path = sts, extensions;
ALTER ROLE sts_reader SET statement_timeout = '30s';
ALTER ROLE sts_reader CONNECTION LIMIT 80;

-- Let the applying user SET ROLE into each role (test/smoke.sql does this to
-- prove sts_reader cannot INSERT) without inheriting their privileges.
GRANT sts_reader TO CURRENT_USER WITH INHERIT FALSE, SET TRUE;
GRANT sts_writer TO CURRENT_USER WITH INHERIT FALSE, SET TRUE;

-- Optional logins, from env vars via apply.sh. Never hard-code a password here.
\if :{?reader_password}
  ALTER ROLE sts_reader WITH LOGIN PASSWORD :'reader_password';
  \echo '02_roles: sts_reader can log in'
\else
  \echo '02_roles: STS_READER_PASSWORD not set; sts_reader stays NOLOGIN'
\endif
\if :{?writer_password}
  ALTER ROLE sts_writer WITH LOGIN PASSWORD :'writer_password';
  \echo '02_roles: sts_writer can log in'
\else
  \echo '02_roles: STS_WRITER_PASSWORD not set; sts_writer stays NOLOGIN'
\endif

-- ---- Schema privileges ------------------------------------------------------
REVOKE ALL ON SCHEMA sts FROM PUBLIC;
REVOKE ALL ON ALL TABLES IN SCHEMA sts FROM PUBLIC;
GRANT USAGE ON SCHEMA sts TO sts_reader, sts_writer;
GRANT USAGE ON SCHEMA extensions TO sts_reader, sts_writer;
GRANT SELECT ON extensions.spatial_ref_sys TO sts_reader, sts_writer;

GRANT SELECT ON ALL TABLES IN SCHEMA sts TO sts_reader;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA sts TO sts_writer;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA sts TO sts_writer;

-- Objects created later in sts by this user (03_views.sql, instructor loads)
-- get the same grants automatically.
ALTER DEFAULT PRIVILEGES IN SCHEMA sts GRANT SELECT ON TABLES TO sts_reader;
ALTER DEFAULT PRIVILEGES IN SCHEMA sts GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO sts_writer;
ALTER DEFAULT PRIVILEGES IN SCHEMA sts GRANT USAGE, SELECT ON SEQUENCES TO sts_writer;

-- Belt and braces: sts_reader never writes, whatever a later GRANT says.
REVOKE INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER ON ALL TABLES IN SCHEMA sts FROM sts_reader;
REVOKE CREATE ON SCHEMA sts FROM sts_reader, sts_writer;

-- ---- Row-level security: every table in sts ---------------------------------
SET client_min_messages = warning;  -- quiet the DROP POLICY IF EXISTS notices
DO $$
DECLARE
  t record;
BEGIN
  FOR t IN
    SELECT c.relname
    FROM pg_class c
    JOIN pg_namespace n ON n.oid = c.relnamespace
    WHERE n.nspname = 'sts' AND c.relkind IN ('r', 'p')
  LOOP
    EXECUTE format('ALTER TABLE sts.%I ENABLE ROW LEVEL SECURITY', t.relname);
    EXECUTE format('DROP POLICY IF EXISTS sts_reader_select ON sts.%I', t.relname);
    EXECUTE format('DROP POLICY IF EXISTS sts_writer_all ON sts.%I', t.relname);
    EXECUTE format(
      'CREATE POLICY sts_reader_select ON sts.%I AS PERMISSIVE FOR SELECT TO sts_reader USING (true)',
      t.relname);
    EXECUTE format(
      'CREATE POLICY sts_writer_all ON sts.%I AS PERMISSIVE FOR ALL TO sts_writer USING (true) WITH CHECK (true)',
      t.relname);
  END LOOP;
END
$$;
