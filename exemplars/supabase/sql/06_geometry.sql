-- 06_geometry.sql
-- Converts the WKT text loaded by 05_seed.sql into PostGIS geometry
-- (ST_GeomFromText(wkt, 4326)), makes geom NOT NULL, drops the staging `wkt`
-- column, does the first refresh of the materialised view, and ANALYZEs so the
-- planner uses the GIST indexes from the first query.
-- Used by chapters 6, 7, 8, 9, 11 (every PostGIS query) and 13 (the view).

SET search_path = sts, extensions, public;

-- Polygons: ST_MakeValid repairs the few rings that rounding WKT to 7 decimal
-- places makes self-touching; ST_CollectionExtract(..., 3) keeps the polygon
-- parts and ST_Multi matches the MultiPolygon column type.

UPDATE sts.japan_municipalities SET geom = ST_Multi(ST_CollectionExtract(ST_MakeValid(ST_GeomFromText(wkt, 4326)), 3));
UPDATE sts.bsi_block_groups     SET geom = ST_Multi(ST_CollectionExtract(ST_MakeValid(ST_GeomFromText(wkt, 4326)), 3));
UPDATE sts.bsi_grid             SET geom = ST_Multi(ST_CollectionExtract(ST_MakeValid(ST_GeomFromText(wkt, 4326)), 3));
UPDATE sts.precincts            SET geom = ST_Multi(ST_CollectionExtract(ST_MakeValid(ST_GeomFromText(wkt, 4326)), 3));
UPDATE sts.bsi_sites            SET geom = ST_GeomFromText(wkt, 4326);
UPDATE sts.polling_places       SET geom = ST_GeomFromText(wkt, 4326);
UPDATE sts.bluebikes_stations   SET geom = ST_GeomFromText(wkt, 4326);
UPDATE sts.air_quality_sites    SET geom = ST_GeomFromText(wkt, 4326);

-- Fail loudly if any WKT did not parse or any polygon is invalid.
DO $$
DECLARE
  t text;
  n bigint;
BEGIN
  FOREACH t IN ARRAY ARRAY['japan_municipalities', 'bsi_block_groups', 'bsi_grid', 'precincts',
                           'bsi_sites', 'polling_places', 'bluebikes_stations', 'air_quality_sites']
  LOOP
    EXECUTE format('SELECT count(*) FROM sts.%I WHERE geom IS NULL OR NOT ST_IsValid(geom)', t) INTO n;
    IF n > 0 THEN
      RAISE EXCEPTION '06_geometry: % has % rows with a NULL or invalid geom', t, n;
    END IF;
    EXECUTE format('ALTER TABLE sts.%I ALTER COLUMN geom SET NOT NULL', t);
    EXECUTE format('ALTER TABLE sts.%I DROP COLUMN wkt', t);
  END LOOP;
END
$$;

REFRESH MATERIALIZED VIEW sts.mv_station_month_totals;

ANALYZE sts.japan_municipalities, sts.bsi_block_groups, sts.bsi_grid, sts.precincts,
        sts.bsi_sites, sts.polling_places, sts.bluebikes_stations, sts.air_quality_sites,
        sts.bluebikes_trips, sts.flights, sts.jp_solar, sts.evacuation_edges,
        sts.air_quality_hourly;
