clean_cbsa_state_map = function(cbsa_state_map_file) {
  cbsa_state_map_file |> 
    select(metro = cbsa20, state_fips = state) |> 
    filter(state_fips != 72)
}

create_acs_csvs = function(
  acs_raw_data, 
  cbsa_state_map, 
  state_abbs, 
  ces_gov_emp,
  ces_total_emp
) {
  state_csv_output = "web_assets/federal_workers_state.csv"
  county_csv_output = "web_assets/federal_workers_county.csv"
  cd_csv_output = "web_assets/federal_workers_cd.csv"
  metro_csv_output = "web_assets/federal_workers_metro.csv"

  acs_data_prep = acs_raw_data |> 
    mutate(
      county_fips = as.numeric(paste0(state, county)),
      state_fips = as.numeric(state),
      cd = as.numeric(congressional_district),
      metro = as.numeric(metro)
    ) |> 
    # drop any areas with missing values for federal workers
    filter(!is.na(value)) |> 
    select(geo_type, metro, state_fips, county_fips, cd, geo_name, value, me, total) 
  
  acs_original_gov = acs_data_prep |> 
    filter(geo_type == "state") |> 
    summarize(sum(value)) |> 
    pull()

  acs_original_total = acs_data_prep |> 
    filter(geo_type == "state") |> 
    summarize(sum(total)) |> 
    pull()

  scaling_factor_gov = ces_gov_emp / acs_original_gov
  scaling_factor_total = ces_total_emp / acs_original_total

  acs_data = acs_data_prep |> 
    mutate(
      value = value * scaling_factor_gov, 
      me = me * scaling_factor_gov,
      total = total * scaling_factor_total,
      share = value / total,
    ) |> 
    mutate(
      value = round(value),
      me = round(me),
      share = round(share, 3)
    )

  acs_states = acs_data |> 
    filter(geo_type == "state") |> 
    distinct(state_fips, geo_name) |> 
    select(state_fips, state_name = geo_name)

  acs_data_final = acs_data |> 
    full_join(acs_states, by = "state_fips") |> 
    select(
      geo_type, state_name, state_fips, county_fips, cd, metro, geo_name, 
      federal_workers = value,
      margin_of_error = me,
      share_of_employment = share
    ) 

  acs_data_final |> 
    filter(geo_type == "state") |> 
    select(
      state_name, state_fips, geo_name, 
      federal_workers, margin_of_error, share_of_employment
    ) |> 
    arrange(geo_name) |> 
    write_csv(state_csv_output)

  acs_data_final |> 
    filter(geo_type == "county") |>
    mutate(geo_name = str_replace(geo_name, ",.*", "")) |> 
    select(
      state_name, county_fips, geo_name, 
      federal_workers, margin_of_error, share_of_employment
    ) |> 
    arrange(state_name, geo_name) |> 
    write_csv(county_csv_output)


  redistricting_exceptions = c(
    "Alabama",
    "Georgia",
    "Louisiana",
    "North Carolina",
    "New York"
  )

  acs_data_final |> 
    filter(geo_type == "cd") |> 
    full_join(state_abbs, by = "state_fips") |> 
    mutate(
      geo_name = str_replace(geo_name, " \\(118.*", ""),
      geo_name = str_replace(geo_name, "\\(at Large\\)", "1"),
      geo_name = str_replace(geo_name, "Congressional District ", paste0(state_abb, "\\u2013"))
    ) |> 
    mutate(geo_name = case_match(
      state_name,
      "District of Columbia" ~ "DC-1",
      "Puerto Rico" ~ "PR-1",
      .default = geo_name
    )) |> 
    # redistricting exceptions
    mutate(geo_name = if_else(
      state_name %in% redistricting_exceptions,
      paste0(geo_name, "*"),
      geo_name
    )) |> 
    select(
      state_name, state_fips, cd, geo_name, 
      federal_workers, margin_of_error, share_of_employment
    ) |> 
    arrange(state_name, cd) |> 
    write_csv(cd_csv_output)

  acs_data_final |> 
    filter(geo_type == "metro") |> 
    select(-c(state_fips, state_name)) |> 
    left_join(cbsa_state_map, by = "metro") |> 
    left_join(acs_states, by = "state_fips") |> 
    mutate(geo_name = str_replace(geo_name, "( Metro Area)|( Micro Area)", ""))  |> 
    select(
      state_name, metro, geo_name,
      federal_workers, margin_of_error, share_of_employment
    ) |> 
    arrange(metro) |> 
    write_csv(metro_csv_output)

  c(
    state_csv_output,
    county_csv_output,
    cd_csv_output,
    metro_csv_output
  )
}


download_acs_data = function(download_date) {
  county_level = getCensus(
    name = "acs/acsse", 
    vintage = 2023, 
    vars = "group(K202402)", 
    region = "county:*") |> 
    as_tibble() |> 
    select(
      state, 
      county, 
      geo_id = GEO_ID, 
      geo_name = NAME,
      value = K202402_007E,
      value_note = K202402_007EA,
      me = K202402_007M,
      me_note = K202402_007MA,
      total = K202402_001E
    ) |> 
    mutate(geo_type = "county") |> 
    filter(state != "72")

  metro_level = getCensus(
    name = "acs/acsse", 
    vintage = 2023, 
    vars = "group(K202402)", 
    region = "metropolitan statistical area/micropolitan statistical area:*") |> 
    as_tibble() |> 
    select(
      metro = metropolitan_statistical_area_micropolitan_statistical_area, 
      geo_id = GEO_ID, 
      geo_name = NAME,
      value = K202402_007E,
      value_note = K202402_007EA,
      me = K202402_007M,
      me_note = K202402_007MA,
      total = K202402_001E
    ) |> 
    mutate(geo_type = "metro") |> 
    filter(!str_detect(geo_name, "PR$"))

  state_level = getCensus(
    name = "acs/acsse", 
    vintage = 2023, 
    vars = "group(K202402)", 
    region = "state:*") |> 
    as_tibble() |> 
    select(
      state, 
      geo_id = GEO_ID, 
      geo_name = NAME,
      value = K202402_007E,
      value_note = K202402_007EA,
      me = K202402_007M,
      me_note = K202402_007MA,
      total = K202402_001E
    ) |> 
    mutate(geo_type = "state") |> 
    filter(state != "72")

  congressional_district = getCensus(
    name = "acs/acsse", 
    vintage = 2023, 
    vars = "group(K202402)", 
    region = "congressional district:*") |> 
    as_tibble() |> 
    select(
      state, 
      congressional_district,
      geo_id = GEO_ID, 
      geo_name = NAME,
      value = K202402_007E,
      value_note = K202402_007EA,
      me = K202402_007M,
      me_note = K202402_007MA,
      total = K202402_001E
    ) |> 
    mutate(geo_type = "cd") |> 
    filter(state != "72")


  bind_rows(
    county_level,
    metro_level,
    state_level,
    congressional_district
  )
  
}