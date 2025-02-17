## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

download_date = "14 February 2025"

tar_assign({

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

  acs_csvs = create_acs_csvs(acs_raw_data, cbsa_state_map, state_abbs) |> 
    tar_file()

  website = tar_quarto(quiet = FALSE)

})
