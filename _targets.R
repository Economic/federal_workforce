## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

download_date = "14 February 2025"

tar_assign({

  # total federal gov benchmark
  # ACS emp seems overstated relative to CES, QCEW, and OPM+USPS
  # ACS = 4.4 million; other sources ~ 3 million
  # from BLS series CES9091000001, value Jan 2025
  ces_gov_emp = 3024000 |> 
    tar_target()

  # from BLS nonfarm series CES0000000001, value Jan 2025
  ces_total_emp = 159069000 |> 
    tar_target()

  state_abbs = state |> 
    mutate(state_fips = as.numeric(fips)) |> 
    add_row(state_fips = 72, usps = "PR") |> 
    select(state_fips, state_abb = usps) |> 
    tar_target()

  cbsa_state_map_file = "geocorr_2022_cbsa_state.csv" |> 
    tar_file_read(read_csv(!!.x, show_col_types = FALSE))

  cbsa_state_map = clean_cbsa_state_map(cbsa_state_map_file) |> 
    tar_target()

  acs_raw_data = download_acs_data(download_date) |> 
    tar_target()

  acs_csvs = create_acs_csvs(
    acs_raw_data, 
    cbsa_state_map, 
    state_abbs, 
    ces_gov_emp, 
    ces_total_emp
  ) |> 
    tar_file()

  website = tar_quarto(quiet = FALSE)

})
