-- 04_cron.sql
-- Schedules the hourly refresh of sts.mv_station_month_totals with pg_cron.
-- Used by chapter 13 ("a pipeline on a clock": cron, pg_cron and freshness;
-- lab V13a).
--
-- Enabling pg_cron on Supabase: Dashboard > Database > Extensions > "pg_cron"
-- > enable (00_extensions.sql also runs CREATE EXTENSION). pg_cron runs in the
-- `postgres` database only, and jobs live in cron.job; run history is in
-- cron.job_run_details. Check it with:
--   SELECT jobid, jobname, schedule, command FROM cron.job;
--   SELECT status, start_time, end_time, return_message
--   FROM cron.job_run_details ORDER BY start_time DESC LIMIT 5;
--
-- Idempotent: an existing job with the same name is unscheduled first.
-- CONCURRENTLY lets students keep reading the view during a refresh; it needs
-- the unique index mv_station_month_totals_pk and a populated view (06 does
-- the first refresh).

-- Default: do not skip, unless apply.sh passed -v skip_cron=on.
\if :{?skip_cron}
\else
  \set skip_cron off
\endif

\if :skip_cron
  \echo '04_cron: skipping pg_cron schedule (STS_SKIP_CRON=1)'
\else
  SELECT cron.unschedule(jobid)
  FROM cron.job
  WHERE jobname = 'sts_refresh_station_month_totals';

  SELECT cron.schedule(
    'sts_refresh_station_month_totals',
    '7 * * * *',  -- hourly, at minute 7
    $cron$REFRESH MATERIALIZED VIEW CONCURRENTLY sts.mv_station_month_totals$cron$
  );
\endif
