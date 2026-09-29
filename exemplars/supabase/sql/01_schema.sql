-- 01_schema.sql
-- Schema `sts`: one table per course dataset the chapters and labs query.
-- Used by chapters 1, 3, 5 (flights, airlines, airports, planes, gapminder,
-- jp_solar), 6-8 (Boston social infrastructure, Boston voting, Japan
-- municipalities), 9-11 (bluebikes, evacuation, committees) and 7, 13 (air
-- quality). Column names follow the CSVs in data/ so a dplyr pipeline and its
-- SQL twin (dbplyr::show_query()) name the same columns.
--
-- Spatial tables carry a `wkt` text column: 05_seed.sql COPYs WKT into it and
-- 06_geometry.sql converts it to `geom` with ST_GeomFromText(wkt, 4326) and
-- then drops it. Every index is named so a chapter can point at it.
--
-- This file DROPS and recreates schema sts. apply.sh refuses to run it against
-- a database that already has an sts schema unless STS_RESET=1.

SET search_path = sts, extensions, public;

DROP SCHEMA IF EXISTS sts CASCADE;
CREATE SCHEMA sts;
COMMENT ON SCHEMA sts IS 'SYSEN 5460 (STS) class database. Read-only for students (role sts_reader).';

SET search_path = sts, extensions, public;

-- ===========================================================================
-- Chapters 1, 3, 5: nycflights (relational design, joins, SQL twins)
-- ===========================================================================
CREATE TABLE sts.airlines (
  carrier text PRIMARY KEY,
  name    text NOT NULL
);
COMMENT ON TABLE sts.airlines IS 'Airline carriers (nycflights13). Ch 3 joins, ch 5 normalisation.';

CREATE TABLE sts.airports (
  faa   text PRIMARY KEY,
  name  text NOT NULL,
  lat   double precision,
  lon   double precision,
  alt   integer,
  tz    double precision,
  dst   text,
  tzone text
);
COMMENT ON TABLE sts.airports IS 'Airports by FAA code (nycflights13). Ch 3 joins.';

CREATE TABLE sts.planes (
  tailnum      text PRIMARY KEY,
  year         integer,
  type         text,
  manufacturer text,
  model        text,
  engines      integer,
  seats        integer,
  speed        integer,
  engine       text
);
COMMENT ON TABLE sts.planes IS 'Aircraft by tail number (nycflights13). Ch 5 normalisation.';

CREATE TABLE sts.flights (
  flight_id      bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  year           integer NOT NULL,
  month          integer NOT NULL,
  day            integer NOT NULL,
  dep_time       integer,
  sched_dep_time integer,
  dep_delay      double precision,
  arr_time       integer,
  sched_arr_time integer,
  arr_delay      double precision,
  carrier        text NOT NULL REFERENCES sts.airlines (carrier),
  flight         integer,
  tailnum        text,
  origin         text NOT NULL REFERENCES sts.airports (faa),
  dest           text NOT NULL,
  air_time       double precision,
  distance       double precision,
  hour           integer,
  minute         integer,
  time_hour      timestamptz
);
COMMENT ON TABLE sts.flights IS 'A fixed 18,000-row sample (set.seed(5460)) of nycflights13::flights. Ch 1 SQL twins, ch 3 joins, ch 5 sampling.';
COMMENT ON COLUMN sts.flights.flight_id IS 'Surrogate key; the source has no single-column key.';
COMMENT ON COLUMN sts.flights.dest IS 'No foreign key on purpose: some destinations (e.g. BQN, SJU, STT, PSE) are not in airports. Ch 3 finds them with an anti-join.';
COMMENT ON COLUMN sts.flights.tailnum IS 'No foreign key on purpose: many tail numbers are not in planes. Ch 5 uses this to discuss referential integrity.';
CREATE INDEX flights_carrier_idx ON sts.flights (carrier);
CREATE INDEX flights_origin_dest_idx ON sts.flights (origin, dest);
CREATE INDEX flights_date_idx ON sts.flights (year, month, day);

-- ===========================================================================
-- Chapter 2: gapminder (tidy data, pivots)
-- ===========================================================================
CREATE TABLE sts.gapminder (
  country     text NOT NULL,
  continent   text NOT NULL,
  year        integer NOT NULL,
  "lifeExp"   double precision,
  pop         bigint,
  "gdpPercap" double precision,
  CONSTRAINT gapminder_pk PRIMARY KEY (country, year)
);
COMMENT ON TABLE sts.gapminder IS 'Gapminder country-years. Ch 2 pivots. "lifeExp" and "gdpPercap" keep the R names, so quote them in SQL.';

-- ===========================================================================
-- Chapter 3: Japan solar (group_by/summarise, LAG() window functions, models)
-- ===========================================================================
CREATE TABLE sts.jp_solar_farms_2018 (
  muni_code                      text PRIMARY KEY,
  muni                           text,
  muni_type                      text,
  pref                           text,
  pref_code                      text,
  region                         text,
  fukushima_exclusion_zone       integer,
  coast                          integer,
  pv_output_2018                 double precision,
  windspeed_2018                 double precision,
  solarkw                        double precision,
  sp                             double precision,
  sp_10_49kw                     double precision,
  sp_50_499kw                    double precision,
  sp_500_1999kw                  double precision,
  sp_2000kw_plus                 double precision,
  land_price_commercial_2009     double precision,
  land_price_residential_2009    double precision,
  area_2010                      double precision,
  population_2010                double precision,
  unemployment_2010              double precision,
  voter_turnout_2012             double precision,
  crime_rate_2008                double precision,
  income_taxable_per_capita_2010 double precision,
  financial_str_index_2010       double precision,
  ratio_revs_exp_2010            double precision,
  disaster_deaths_2011           double precision,
  disaster_damage_2011           double precision
);
COMMENT ON TABLE sts.jp_solar_farms_2018 IS 'Municipality covariates for Japanese solar adoption (one row per municipality). Ch 3 joins and one-model-per-group.';
CREATE INDEX jp_solar_farms_2018_region_idx ON sts.jp_solar_farms_2018 (region);

CREATE TABLE sts.jp_solar (
  muni_code        text NOT NULL REFERENCES sts.jp_solar_farms_2018 (muni_code),
  date             date NOT NULL,
  year             integer,
  solar_under_10kw double precision,
  disaster         integer,
  pop              double precision,
  solar            double precision,
  solar_rate       double precision,
  CONSTRAINT jp_solar_pk PRIMARY KEY (muni_code, date)
);
COMMENT ON TABLE sts.jp_solar IS 'Municipality-month residential solar installations. Ch 3 group/summarise and LAG() OVER (PARTITION BY muni_code ORDER BY date).';
CREATE INDEX jp_solar_date_idx ON sts.jp_solar (date);

-- ===========================================================================
-- Chapter 7: Japan municipalities (neighbours, spatial lag)
-- ===========================================================================
CREATE TABLE sts.japan_municipalities (
  muni_code    text PRIMARY KEY,
  muni         text,
  muni_type    text,
  muni_type_jp text,
  muni_jp      text,
  pref         text,
  pref_code    text,
  region       text,
  wkt          text,
  geom         geometry(MultiPolygon, 4326)
);
COMMENT ON TABLE sts.japan_municipalities IS 'Japanese municipality polygons, simplified (see seed/README.md). Ch 7 neighbours via ST_Touches / ST_Intersects.';
CREATE INDEX japan_municipalities_geom_gix ON sts.japan_municipalities USING gist (geom);

-- ===========================================================================
-- Chapters 6, 7, 8: Boston social infrastructure
-- ===========================================================================
CREATE TABLE sts.bsi_sites (
  id         integer PRIMARY KEY,
  name       text,
  site_group text,
  wkt        text,
  geom       geometry(Point, 4326)
);
COMMENT ON TABLE sts.bsi_sites IS 'Social infrastructure sites in Boston (parks, community spaces, places of worship, social businesses). Ch 6 spatial joins.';
COMMENT ON COLUMN sts.bsi_sites.site_group IS 'Called `group` in the source GeoJSON; renamed because GROUP is an SQL keyword.';
CREATE INDEX bsi_sites_geom_gix ON sts.bsi_sites USING gist (geom);
CREATE INDEX bsi_sites_group_idx ON sts.bsi_sites (site_group);

CREATE TABLE sts.bsi_block_groups (
  geoid text PRIMARY KEY,
  wkt   text,
  geom  geometry(MultiPolygon, 4326)
);
COMMENT ON TABLE sts.bsi_block_groups IS 'Census block groups covering Boston (12-digit GEOID). Ch 6 point-in-polygon joins.';
CREATE INDEX bsi_block_groups_geom_gix ON sts.bsi_block_groups USING gist (geom);

CREATE TABLE sts.bsi_grid (
  cell text PRIMARY KEY,
  wkt  text,
  geom geometry(MultiPolygon, 4326)
);
COMMENT ON TABLE sts.bsi_grid IS 'Regular grid over Boston that the census table is measured on. Ch 8 rates by area.';
CREATE INDEX bsi_grid_geom_gix ON sts.bsi_grid USING gist (geom);

CREATE TABLE sts.bsi_census (
  cell                        text PRIMARY KEY REFERENCES sts.bsi_grid (cell),
  neighborhood                text,
  pop_density                 double precision,
  pop_women                   double precision,
  pop_white                   double precision,
  pop_black                   double precision,
  pop_natam                   double precision,
  pop_asian                   double precision,
  pop_pacific                 double precision,
  pop_hisplat                 double precision,
  pop_some_college            double precision,
  median_income               double precision,
  income_inequality           double precision,
  pop_unemployed              double precision,
  median_monthly_housing_cost double precision,
  pop_age_65_plus             double precision
);
COMMENT ON TABLE sts.bsi_census IS 'Census covariates per grid cell (boston_census_data.csv, keyed by cell, not by block group). Ch 8 small-area rates.';

-- ===========================================================================
-- Chapters 6, 7: Boston voting (CRS, nearest facility with ST_DWithin)
-- ===========================================================================
CREATE TABLE sts.precincts (
  ward_precinct text PRIMARY KEY,
  ward          text,
  precinct      text,
  voted_biden   double precision,
  voted_trump   double precision,
  voter_turnout double precision,
  wkt           text,
  geom          geometry(MultiPolygon, 4326)
);
COMMENT ON TABLE sts.precincts IS 'Boston ward-precincts with 2020 presidential vote shares and turnout (boston_votes.csv joined on ward_precinct). Ch 6, 7.';
CREATE INDEX precincts_geom_gix ON sts.precincts USING gist (geom);

CREATE TABLE sts.polling_places (
  id            integer PRIMARY KEY,
  ward_precinct text,
  wkt           text,
  geom          geometry(Point, 4326)
);
COMMENT ON TABLE sts.polling_places IS 'Boston polling places. Ch 7 nearest facility: pre-filter with ST_DWithin, then ORDER BY geom <-> point.';
COMMENT ON COLUMN sts.polling_places.ward_precinct IS 'No foreign key on purpose: one polling place (052A) has no matching precinct polygon.';
CREATE INDEX polling_places_geom_gix ON sts.polling_places USING gist (geom);
CREATE INDEX polling_places_ward_precinct_idx ON sts.polling_places (ward_precinct);

-- ===========================================================================
-- Chapters 9, 10, 11: Bluebikes station network
-- ===========================================================================
CREATE TABLE sts.bluebikes_stations (
  code                      text PRIMARY KEY,
  geoid                     text,
  pop_density_2020_smooth5  double precision,
  pop_some_college_smooth5  double precision,
  pop_white_2020_smooth5    double precision,
  pop_over_65_2019_smooth5  double precision,
  wkt                       text,
  geom                      geometry(Point, 4326)
);
COMMENT ON TABLE sts.bluebikes_stations IS 'Bluebikes stations (the nodes table) with block-group covariates. Ch 9 nodes/edges, ch 10 centrality.';
CREATE INDEX bluebikes_stations_geom_gix ON sts.bluebikes_stations USING gist (geom);

CREATE TABLE sts.bluebikes_trips (
  start_code text    NOT NULL REFERENCES sts.bluebikes_stations (code),
  end_code   text    NOT NULL REFERENCES sts.bluebikes_stations (code),
  month      date    NOT NULL,
  trips      integer NOT NULL CHECK (trips >= 0),
  CONSTRAINT bluebikes_trips_pk PRIMARY KEY (start_code, end_code, month)
);
COMMENT ON TABLE sts.bluebikes_trips IS 'Station-pair rush-hour trips per month, 2019, pairs with >= 100 trips that year (the edges table). Ch 9 indexes and pre-aggregation; ch 10 degree and recursive-CTE neighbour counts.';
-- Ch 9 teaches these: the PK serves lookups by start_code; the two below
-- serve "who arrives at station X" and "all edges in month M".
CREATE INDEX bluebikes_trips_end_code_idx ON sts.bluebikes_trips (end_code);
CREATE INDEX bluebikes_trips_month_idx ON sts.bluebikes_trips (month);

-- ===========================================================================
-- Chapters 9, 11: Hurricane Dorian evacuation (origin-destination flows)
-- ===========================================================================
CREATE TABLE sts.evacuation_nodes (
  node            integer PRIMARY KEY,
  geoid           text UNIQUE,
  county          text,
  pop             double precision,
  social_capital  double precision,
  bonding         double precision,
  bridging        double precision,
  linking         double precision,
  svi             double precision,
  median_income   double precision,
  pop_age_65_plus double precision,
  evacuation_more double precision,
  evacuation_less double precision
);
COMMENT ON TABLE sts.evacuation_nodes IS 'County subdivisions in the Hurricane Dorian evacuation network (subset of nodes.rds columns). Ch 9 sampling a network, ch 11 flows.';

CREATE TABLE sts.evacuation_edges (
  edge_id    bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  from_node  integer NOT NULL REFERENCES sts.evacuation_nodes (node),
  to_node    integer NOT NULL REFERENCES sts.evacuation_nodes (node),
  date_time  timestamptz NOT NULL,
  evacuation double precision,
  km         double precision,
  from_geoid text,
  to_geoid   text
);
COMMENT ON TABLE sts.evacuation_edges IS 'A 15,000-row sample (set.seed(5460)) of directed OD observations: change from baseline movement (Facebook users). Ch 9, 11.';
CREATE INDEX evacuation_edges_from_to_idx ON sts.evacuation_edges (from_node, to_node);
CREATE INDEX evacuation_edges_to_idx ON sts.evacuation_edges (to_node);
CREATE INDEX evacuation_edges_date_time_idx ON sts.evacuation_edges (date_time);

-- ===========================================================================
-- Chapter 10: disaster recovery committees (bipartite membership)
-- ===========================================================================
CREATE TABLE sts.committees (
  committee_id       text PRIMARY KEY,
  committee_type     text,
  geography          text,
  level              text,
  town               text,
  committee_romaji   text,
  committee_japanese text
);
COMMENT ON TABLE sts.committees IS 'Disaster recovery committees (mode 1 of the bipartite network). Ch 10 projection.';

CREATE TABLE sts.committee_members (
  member_id  text PRIMARY KEY,
  role       text,
  age_group  text,
  gender     text,
  politician text,
  govt       text,
  research   text,
  business   text,
  fisheries  text,
  media      text
);
COMMENT ON TABLE sts.committee_members IS 'Anonymised committee members (mode 2; ids like name_1). Subset of members.csv columns. Ch 10.';

CREATE TABLE sts.committee_memberships (
  committee_id text NOT NULL REFERENCES sts.committees (committee_id),
  member_id    text NOT NULL REFERENCES sts.committee_members (member_id),
  weight       double precision NOT NULL DEFAULT 1,
  CONSTRAINT committee_memberships_pk PRIMARY KEY (committee_id, member_id)
);
COMMENT ON TABLE sts.committee_memberships IS 'Bipartite edge list. Ch 10: a self-join on committee_id projects members to a one-mode co-membership network.';
CREATE INDEX committee_memberships_member_idx ON sts.committee_memberships (member_id);

-- ===========================================================================
-- Chapters 7, 13: air quality (gridding over time, pg_cron freshness)
-- ===========================================================================
CREATE TABLE sts.air_quality_sites (
  aqs_id_full bigint PRIMARY KEY,
  aqs_id      text,
  site_name   text,
  nyc_id      text,
  dist_km     double precision,
  wkt         text,
  geom        geometry(Point, 4326)
);
COMMENT ON TABLE sts.air_quality_sites IS 'PM2.5 sensor sites in the NYC metro. dist_km = distance to the congestion relief zone boundary. Ch 7, 13.';
CREATE INDEX air_quality_sites_geom_gix ON sts.air_quality_sites USING gist (geom);

CREATE TABLE sts.air_quality_hourly (
  aqs_id_full bigint NOT NULL REFERENCES sts.air_quality_sites (aqs_id_full),
  datetime    timestamptz NOT NULL,
  pollutant   text NOT NULL,
  value       double precision,
  unit        text NOT NULL,
  CONSTRAINT air_quality_hourly_pk PRIMARY KEY (aqs_id_full, datetime, pollutant, unit)
);
COMMENT ON TABLE sts.air_quality_hourly IS 'One week (2025-05-01 to 2025-05-07 UTC) of hourly PM2.5 in UG/M3. Ch 7 gridding over time, ch 13 the simulated hourly feed.';
CREATE INDEX air_quality_hourly_datetime_idx ON sts.air_quality_hourly (datetime);
