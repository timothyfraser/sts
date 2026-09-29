<!-- README.md: the STS class database (Supabase Postgres + PostGIS + pg_cron),
     built from SQL scripts. Used by chapters 1, 3, 5-11 and 13. -->

# STS class database (Supabase)

Everything needed to build the SYSEN 5460 class database on a fresh Supabase
Postgres (17, PostGIS 3) in one command. Tim's ruling, 2026-09-29: write the
SQL now, patch and integrate once the project exists.

```sh
export SUPABASE_DB_URL='postgresql://postgres:<db-password>@db.<project-ref>.supabase.co:5432/postgres'
export STS_READER_PASSWORD='<new class password>'   # optional; omit to keep sts_reader NOLOGIN
STS_SMOKE=1 bash apply.sh
```

Never commit these values. Put them in your shell or a git-ignored `.env`.

## What it builds

| File | Does | Chapters |
|---|---|---|
| `sql/00_extensions.sql` | `postgis` (in schema `extensions`), `pg_cron` | 6, 7, 13 |
| `sql/01_schema.sql` | schema `sts`: 23 tables, PKs, FKs, comments, named indexes (GIST on every `geom`) | all |
| `sql/02_roles.sql` | `sts_reader` (SELECT only), `sts_writer`, RLS + policies on every table | 1, 5 |
| `sql/03_views.sql` | `mv_station_month_totals`, `v_solar_by_region`, `v_sites_per_block_group`, `v_bsi_heatmap_grid` | 3, 6, 7, 8, 9, 13 |
| `sql/04_cron.sql` | hourly `REFRESH MATERIALIZED VIEW CONCURRENTLY` via `cron.schedule` | 13 |
| `sql/05_seed.sql` | `\copy` of `seed/*.csv` | all |
| `sql/06_geometry.sql` | WKT to `geometry(...,4326)`, validity check, first MV refresh, ANALYZE | 6-11 |
| `seed/make_seed.R` | rebuilds the seed CSVs from `data/` (see `seed/README.md`) | |
| `test/smoke.sql` | 6 known-count queries + an INSERT that must fail for `sts_reader` | 1 |

`apply.sh` runs 00 on its own, then 01-06 in a single transaction (a failure
leaves nothing half-built), then prints a row count per table. It refuses to
touch an existing `sts` schema unless `STS_RESET=1`, because 01 drops and
recreates it. `STS_SKIP_CRON=1` skips pg_cron for a local Postgres without it.

Verified locally 2026-09-29 on Postgres 16 + PostGIS 3 with
`STS_SKIP_CRON=1`: all files apply, every table loads, smoke tests 1-7 pass.
pg_cron (`04_cron.sql`) has not yet run anywhere.

## What needs the dashboard

pg_cron. In the Supabase dashboard: **Database > Extensions**, search
`pg_cron`, enable it. It runs in the `postgres` database only, which is where
`apply.sh` builds `sts`. PostGIS can be enabled from SQL (00 does it), but
enabling it in the same panel does no harm.

Check the job after applying:

```sql
SELECT jobid, jobname, schedule FROM cron.job;
SELECT status, start_time, return_message FROM cron.job_run_details ORDER BY start_time DESC LIMIT 5;
```

## How students get the reader credentials

`sts_reader` is one shared, SELECT-only login. Its password comes from
`STS_READER_PASSWORD` at apply time and is never in the repo. The
instructor posts the values on Canvas; students copy the course
`.env.example` (same pattern as `workshops/8P_database/env.example`) to `.env`
and fill in:

```
SUPABASE_HOST=aws-0-<region>.pooler.supabase.com
SUPABASE_PORT=5432
SUPABASE_DB=postgres
SUPABASE_USER=sts_reader.<project-ref>
SUPABASE_PASSWORD=<the reader password>
```

Through the Supabase pooler the user name is `sts_reader.<project-ref>`; on
the direct host (`db.<project-ref>.supabase.co`) it is plain `sts_reader`.
The role's `search_path` is `sts, extensions`, so `SELECT * FROM flights`
works, and it has a 30 s statement timeout. It cannot write, create objects, or
read `public`, `auth`, `storage` or `cron`. Rotate the password each term with
`ALTER ROLE sts_reader PASSWORD '...'` in the SQL editor.

`sts_writer` (`STS_WRITER_PASSWORD`) is for instructor loads and the DL
Challenge option B writes. Hand it out per team only if that option runs.

## Patch later: when the project exists

- [ ] Create the Supabase project in the **FRASER** org (region us-east-1;
      save the database password in the password manager, not here).
- [ ] Dashboard > Database > Extensions: enable **pg_cron** (and postgis).
- [ ] Choose a reader password and `export STS_READER_PASSWORD=...`.
- [ ] `export SUPABASE_DB_URL=...` (Project Settings > Database > connection
      string, direct connection, `postgres` user).
- [ ] From `exemplars/supabase/`: `STS_SMOKE=1 bash apply.sh`. Expect the row
      counts below and `smoke 1-6: PASS`, `smoke 7: PASS`.
- [ ] Wait past minute :07 of the hour and check `cron.job_run_details` shows
      `succeeded`, and that `SELECT max(refreshed_at) FROM sts.mv_station_month_totals` moved.
- [ ] Connect as `sts_reader` from R (`DBI::dbConnect(RPostgres::Postgres(), ...)`
      with the `.env` values) and run smoke query 1.
- [ ] Run the Supabase advisors (Database > Advisors) and fix anything they flag.
- [ ] Put the reader credentials on Canvas, and update chapter connection code
      to the real host.

Expected row counts: airlines 16, airports 1458, planes 3322, flights 18000,
gapminder 1704, jp_solar_farms_2018 1741, jp_solar 6321, japan_municipalities
1750, bsi_sites 1049, bsi_block_groups 680, bsi_grid 73, bsi_census 73,
precincts 255, polling_places 255, bluebikes_stations 425, bluebikes_trips
66604, evacuation_nodes 427, evacuation_edges 15000, committees 39,
committee_members 656, committee_memberships 749, air_quality_sites 56,
air_quality_hourly 8395, mv_station_month_totals 2950.

## Smoke query

```sh
psql "$SUPABASE_DB_URL" -X -v ON_ERROR_STOP=1 -f test/smoke.sql
```

1. `SELECT count(*) FROM sts.airlines` returns 16.
2. `gapminder` has 1704 rows and 142 countries.
3. `LAG(solar) OVER (PARTITION BY muni_code ORDER BY date)` is non-null on 6174 rows.
4. `sum(n_sites)` in `v_sites_per_block_group` (ST_Intersects) is 1004.
5. `ST_DWithin(..., 1000)` finds 4 polling places within 1 km of polling place 1.
6. `committee_memberships` has 749 rows over 39 committees.
7. As `sts_reader`, `INSERT INTO sts.airlines` must fail with insufficient privilege.

## Known gaps

- `flights.dest` and `flights.tailnum` have no foreign keys on purpose: the
  source has destinations and tail numbers missing from `airports`/`planes`
  (chapter 3's anti-join). `polling_places.ward_precinct` has none because
  polling place `052A` has no precinct polygon.
- `bsi_census` is keyed by grid `cell`, as in `boston_census_data.csv`, not by
  block group.
- The `v_bsi_heatmap_grid` cell size (500 m, EPSG:26986) is a first choice;
  chapters can build their own grid with `ST_SquareGrid`.
