-- 05_seed.sql
-- Loads seed/*.csv into schema sts with psql's client-side \copy (works on
-- Supabase, where server-side COPY FROM a file is not allowed). Run from
-- exemplars/supabase/ (apply.sh does the cd) so the relative paths resolve.
-- Used by every chapter that queries the class database (1-3, 5-11, 13).
--
-- Seeds are built by seed/make_seed.R (see seed/README.md). CSV NULL is an
-- empty unquoted field. Spatial tables load WKT into the `wkt` text column;
-- 06_geometry.sql turns it into `geom`. Order respects foreign keys.

SET search_path = sts, extensions, public;

\copy sts.airlines (carrier, name) FROM 'seed/airlines.csv' WITH (FORMAT csv, HEADER true)
\copy sts.airports (faa, name, lat, lon, alt, tz, dst, tzone) FROM 'seed/airports.csv' WITH (FORMAT csv, HEADER true)
\copy sts.planes (tailnum, year, type, manufacturer, model, engines, seats, speed, engine) FROM 'seed/planes.csv' WITH (FORMAT csv, HEADER true)
\copy sts.flights (year, month, day, dep_time, sched_dep_time, dep_delay, arr_time, sched_arr_time, arr_delay, carrier, flight, tailnum, origin, dest, air_time, distance, hour, minute, time_hour) FROM 'seed/flights.csv' WITH (FORMAT csv, HEADER true)
\copy sts.gapminder (country, continent, year, "lifeExp", pop, "gdpPercap") FROM 'seed/gapminder.csv' WITH (FORMAT csv, HEADER true)
\copy sts.jp_solar_farms_2018 (muni_code, muni, muni_type, pref, pref_code, region, fukushima_exclusion_zone, coast, pv_output_2018, windspeed_2018, solarkw, sp, sp_10_49kw, sp_50_499kw, sp_500_1999kw, sp_2000kw_plus, land_price_commercial_2009, land_price_residential_2009, area_2010, population_2010, unemployment_2010, voter_turnout_2012, crime_rate_2008, income_taxable_per_capita_2010, financial_str_index_2010, ratio_revs_exp_2010, disaster_deaths_2011, disaster_damage_2011) FROM 'seed/jp_solar_farms_2018.csv' WITH (FORMAT csv, HEADER true)
\copy sts.jp_solar (muni_code, date, year, solar_under_10kw, disaster, pop, solar, solar_rate) FROM 'seed/jp_solar.csv' WITH (FORMAT csv, HEADER true)
\copy sts.japan_municipalities (muni_code, muni, muni_type, muni_type_jp, muni_jp, pref, pref_code, region, wkt) FROM 'seed/japan_municipalities.csv' WITH (FORMAT csv, HEADER true)
\copy sts.bsi_sites (id, name, site_group, wkt) FROM 'seed/bsi_sites.csv' WITH (FORMAT csv, HEADER true)
\copy sts.bsi_block_groups (geoid, wkt) FROM 'seed/bsi_block_groups.csv' WITH (FORMAT csv, HEADER true)
\copy sts.bsi_grid (cell, wkt) FROM 'seed/bsi_grid.csv' WITH (FORMAT csv, HEADER true)
\copy sts.bsi_census (cell, neighborhood, pop_density, pop_women, pop_white, pop_black, pop_natam, pop_asian, pop_pacific, pop_hisplat, pop_some_college, median_income, income_inequality, pop_unemployed, median_monthly_housing_cost, pop_age_65_plus) FROM 'seed/bsi_census.csv' WITH (FORMAT csv, HEADER true)
\copy sts.precincts (ward_precinct, wkt, ward, precinct, voted_biden, voted_trump, voter_turnout) FROM 'seed/precincts.csv' WITH (FORMAT csv, HEADER true)
\copy sts.polling_places (id, ward_precinct, wkt) FROM 'seed/polling_places.csv' WITH (FORMAT csv, HEADER true)
\copy sts.bluebikes_stations (code, geoid, pop_density_2020_smooth5, pop_some_college_smooth5, pop_white_2020_smooth5, pop_over_65_2019_smooth5, wkt) FROM 'seed/bluebikes_stations.csv' WITH (FORMAT csv, HEADER true)
\copy sts.bluebikes_trips (start_code, end_code, month, trips) FROM 'seed/bluebikes_trips.csv' WITH (FORMAT csv, HEADER true)
\copy sts.evacuation_nodes (node, geoid, county, pop, social_capital, bonding, bridging, linking, svi, median_income, pop_age_65_plus, evacuation_more, evacuation_less) FROM 'seed/evacuation_nodes.csv' WITH (FORMAT csv, HEADER true)
\copy sts.evacuation_edges (from_node, to_node, date_time, evacuation, km, from_geoid, to_geoid) FROM 'seed/evacuation_edges.csv' WITH (FORMAT csv, HEADER true)
\copy sts.committees (committee_id, committee_type, geography, level, town, committee_romaji, committee_japanese) FROM 'seed/committees.csv' WITH (FORMAT csv, HEADER true)
\copy sts.committee_members (member_id, role, age_group, gender, politician, govt, research, business, fisheries, media) FROM 'seed/committee_members.csv' WITH (FORMAT csv, HEADER true)
\copy sts.committee_memberships (committee_id, member_id, weight) FROM 'seed/committee_memberships.csv' WITH (FORMAT csv, HEADER true)
\copy sts.air_quality_sites (aqs_id_full, aqs_id, site_name, nyc_id, dist_km, wkt) FROM 'seed/air_quality_sites.csv' WITH (FORMAT csv, HEADER true)
\copy sts.air_quality_hourly (aqs_id_full, datetime, pollutant, value, unit) FROM 'seed/air_quality_hourly.csv' WITH (FORMAT csv, HEADER true)
