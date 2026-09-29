-- test/smoke.sql
-- Smoke test for the STS class database after apply.sh. Six queries return
-- known counts (the expected value is in each comment and checked by an
-- assertion), and the last block proves sts_reader cannot INSERT.
-- Used when setting up the term's database (README "Patch later" checklist)
-- and by chapter 1's "connect to the class DB and query it".
--   psql "$SUPABASE_DB_URL" -X -v ON_ERROR_STOP=1 -f test/smoke.sql

SET search_path = sts, extensions, public;

-- 1. Ch 1 SQL twin of filter() + count(): 16 airlines.
SELECT count(*) AS airlines FROM sts.airlines;                        -- expect 16

-- 2. Ch 2: gapminder has 142 countries x 12 years = 1704 rows.
SELECT count(*) AS gapminder_rows, count(DISTINCT country) AS countries
FROM sts.gapminder;                                                   -- expect 1704, 142

-- 3. Ch 3 LAG(): every municipality-month except each municipality's first
--    month has a previous value (6321 rows - 147 municipalities = 6174).
SELECT count(prev_solar) AS rows_with_lag
FROM (
  SELECT LAG(solar) OVER (PARTITION BY muni_code ORDER BY date) AS prev_solar
  FROM sts.jp_solar
) x;                                                                  -- expect 6174

-- 4. Ch 6 spatial join: sites inside a Boston block group (ST_Intersects).
SELECT sum(n_sites) AS sites_in_block_groups
FROM sts.v_sites_per_block_group;                                     -- expect 1004

-- 5. Ch 7 ST_DWithin: polling places within 1 km of polling place id 1.
SELECT count(*) AS polling_within_1km
FROM sts.polling_places a
JOIN sts.polling_places b
  ON a.id = 1 AND b.id <> a.id
 AND ST_DWithin(a.geom::geography, b.geom::geography, 1000);           -- expect 4

-- 6. Ch 10 bipartite: 749 memberships across 39 committees.
SELECT count(*) AS memberships, count(DISTINCT committee_id) AS committees
FROM sts.committee_memberships;                                       -- expect 749, 39

-- Assertions: the same six numbers, checked, so a wrong load fails loudly.
DO $$
DECLARE
  got bigint;
BEGIN
  SELECT count(*) INTO got FROM sts.airlines;
  IF got <> 16 THEN RAISE EXCEPTION 'smoke 1: airlines = %, expected 16', got; END IF;
  SELECT count(*) INTO got FROM sts.gapminder;
  IF got <> 1704 THEN RAISE EXCEPTION 'smoke 2: gapminder = %, expected 1704', got; END IF;
  SELECT count(prev_solar) INTO got FROM (
    SELECT LAG(solar) OVER (PARTITION BY muni_code ORDER BY date) AS prev_solar FROM sts.jp_solar) x;
  IF got <> 6174 THEN RAISE EXCEPTION 'smoke 3: rows_with_lag = %, expected 6174', got; END IF;
  SELECT sum(n_sites) INTO got FROM sts.v_sites_per_block_group;
  IF got <> 1004 THEN RAISE EXCEPTION 'smoke 4: sites_in_block_groups = %, expected 1004', got; END IF;
  SELECT count(*) INTO got FROM sts.polling_places a JOIN sts.polling_places b
    ON a.id = 1 AND b.id <> a.id AND ST_DWithin(a.geom::geography, b.geom::geography, 1000);
  IF got <> 4 THEN RAISE EXCEPTION 'smoke 5: polling_within_1km = %, expected 4', got; END IF;
  SELECT count(*) INTO got FROM sts.committee_memberships;
  IF got <> 749 THEN RAISE EXCEPTION 'smoke 6: memberships = %, expected 749', got; END IF;
  RAISE NOTICE 'smoke 1-6: PASS';
END
$$;

-- 7. MUST FAIL: sts_reader cannot write. The INSERT is expected to raise
--    insufficient_privilege; if it succeeds, this block raises instead.
DO $$
BEGIN
  SET LOCAL ROLE sts_reader;
  BEGIN
    INSERT INTO sts.airlines (carrier, name) VALUES ('ZZ', 'Smoke Test Air');
    RAISE EXCEPTION 'smoke 7: FAIL - sts_reader was able to INSERT into sts.airlines';
  EXCEPTION WHEN insufficient_privilege THEN
    RAISE NOTICE 'smoke 7: PASS - sts_reader INSERT refused (%)', SQLERRM;
  END;
  RESET ROLE;
END
$$;
