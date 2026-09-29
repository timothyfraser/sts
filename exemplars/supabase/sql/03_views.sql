-- 03_views.sql
-- Derived objects the chapters query by name.
--   sts.mv_station_month_totals  ch 9 pre-aggregation, ch 13 pg_cron refresh
--   sts.v_solar_by_region        ch 3 group/summarise + join (SQL twin)
--   sts.v_sites_per_block_group  ch 6 spatial join with ST_Intersects
--   sts.v_bsi_heatmap_grid       ch 7/8 heatmap grid with ST_SquareGrid
--
-- Plain views use security_invoker = true, so a student querying one is held
-- to their own privileges and RLS (Supabase's advisor flags definer views).
-- The materialised view is created WITH NO DATA because seeds load later
-- (05); 06_geometry.sql does the first REFRESH, and 04_cron.sql keeps it
-- fresh. Grants come from the default privileges set in 02_roles.sql.

SET search_path = sts, extensions, public;

-- ---- Chapter 9 / 13: Bluebikes station-month totals ------------------------
CREATE MATERIALIZED VIEW sts.mv_station_month_totals AS
WITH outbound AS (
  SELECT start_code AS code, month, sum(trips) AS trips_out, count(*) AS n_destinations
  FROM sts.bluebikes_trips
  GROUP BY start_code, month
),
inbound AS (
  SELECT end_code AS code, month, sum(trips) AS trips_in, count(*) AS n_origins
  FROM sts.bluebikes_trips
  GROUP BY end_code, month
)
SELECT
  coalesce(o.code, i.code)                            AS code,
  coalesce(o.month, i.month)                          AS month,
  coalesce(o.trips_out, 0)::bigint                    AS trips_out,
  coalesce(i.trips_in, 0)::bigint                     AS trips_in,
  (coalesce(o.trips_out, 0) + coalesce(i.trips_in, 0))::bigint AS trips_total,
  coalesce(o.n_destinations, 0)::integer              AS n_destinations,
  coalesce(i.n_origins, 0)::integer                   AS n_origins,
  now()                                               AS refreshed_at
FROM outbound o
FULL JOIN inbound i ON i.code = o.code AND i.month = o.month
WITH NO DATA;

COMMENT ON MATERIALIZED VIEW sts.mv_station_month_totals IS
  'Rush-hour trips out of / into each Bluebikes station per month. Refreshed hourly by pg_cron (04_cron.sql); refreshed_at shows freshness. Ch 9, 13.';
-- REFRESH ... CONCURRENTLY needs a unique index on the materialised view.
CREATE UNIQUE INDEX mv_station_month_totals_pk ON sts.mv_station_month_totals (code, month);

-- ---- Chapter 3: solar by region and month -----------------------------------
CREATE VIEW sts.v_solar_by_region WITH (security_invoker = true) AS
SELECT
  f.region,
  s.date,
  count(*)                          AS n_munis,
  sum(s.solar)                      AS solar,
  sum(s.pop)                        AS pop,
  1000 * sum(s.solar) / nullif(sum(s.pop), 0) AS solar_per_1000
FROM sts.jp_solar s
JOIN sts.jp_solar_farms_2018 f ON f.muni_code = s.muni_code
GROUP BY f.region, s.date;

COMMENT ON VIEW sts.v_solar_by_region IS
  'SQL twin of jp_solar %>% left_join(jp_solar_farms_2018) %>% group_by(region, date) %>% summarise(...). Ch 3.';

-- ---- Chapter 6: social-infrastructure sites per block group ----------------
CREATE VIEW sts.v_sites_per_block_group WITH (security_invoker = true) AS
SELECT
  bg.geoid,
  coalesce(n.n_sites, 0)             AS n_sites,
  coalesce(n.n_parks, 0)             AS n_parks,
  coalesce(n.n_community, 0)         AS n_community,
  coalesce(n.n_worship, 0)           AS n_worship,
  coalesce(n.n_social_business, 0)   AS n_social_business,
  bg.geom
FROM sts.bsi_block_groups bg
LEFT JOIN (
  SELECT
    b.geoid,
    count(*)                                                AS n_sites,
    count(*) FILTER (WHERE s.site_group = 'Parks')             AS n_parks,
    count(*) FILTER (WHERE s.site_group = 'Community Spaces')  AS n_community,
    count(*) FILTER (WHERE s.site_group = 'Places of Worship') AS n_worship,
    count(*) FILTER (WHERE s.site_group = 'Social Businesses') AS n_social_business
  FROM sts.bsi_block_groups b
  JOIN sts.bsi_sites s ON ST_Intersects(b.geom, s.geom)
  GROUP BY b.geoid
) n ON n.geoid = bg.geoid;

COMMENT ON VIEW sts.v_sites_per_block_group IS
  'Count of social-infrastructure sites per block group via ST_Intersects (uses bsi_sites_geom_gix and bsi_block_groups_geom_gix). SQL twin of st_join() + count(). Ch 6.';

-- ---- Chapters 7, 8: heatmap grid ---------------------------------------------
-- 500 m square cells built in EPSG:26986 (Massachusetts State Plane, metres)
-- so the cell size is a real distance, then returned in EPSG:4326 and joined
-- to sites in 4326 so the GIST index on bsi_sites.geom is used.
CREATE VIEW sts.v_bsi_heatmap_grid WITH (security_invoker = true) AS
WITH bounds AS (
  SELECT ST_Transform(ST_SetSRID(ST_Extent(geom)::geometry, 4326), 26986) AS geom
  FROM sts.bsi_block_groups
),
grid AS (
  SELECT g.i, g.j, ST_Transform(g.geom, 4326) AS geom
  FROM bounds, ST_SquareGrid(500, bounds.geom) AS g
)
SELECT
  grid.i,
  grid.j,
  count(s.id) AS n_sites,
  grid.geom
FROM grid
LEFT JOIN sts.bsi_sites s ON ST_Intersects(grid.geom, s.geom)
GROUP BY grid.i, grid.j, grid.geom;

COMMENT ON VIEW sts.v_bsi_heatmap_grid IS
  '500 m ST_SquareGrid over Boston with a count of social-infrastructure sites per cell; (i, j) is the cell index. Ch 7, 8.';
