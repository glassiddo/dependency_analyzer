#* Project: Migration Africa
#* Author:  Sam Marshall, edited by Iddo Glass
#* Date:    April 1, 2023, edited on December 22, 2025
#* Title:   IDs
#* Desc:    Build geographical unit IDs Dataset for each admin unit that we use
#*******************************************************************************

source("code/SSA_env_SetUp.R")

single_first_cens_ctries <- c("CIV")
single_second_cens_ctries <- c(
  "AGO", "MWI", "RWA", "SDN", "TGO", "ZWE", "MDG",
  "NGA", "ETH"
  )
aggregate_only_ctries <- c("GAB") # countries without microdata

countries <- c( # all countries
  "AGO", "BEN", "BWA", "BFA", "CMR", "GHA", "GIN", "CIV", "KEN", "LSO",
  "MWI", "MLI", "MUS", "MOZ", "RWA", "SEN", "SLE", "ZAF", "SDN", "TZA",
  "TGO", "UGA", "ZMB", "ZWE", "MDG", "GAB", 
  "NGA", "ETH"
)

process_geo_units <- function(ctry_str) {
  
  c_info <- census_info %>% filter(country == ctry_str)
  lvl <- c_info$lvl
  
  if (ctry_str %in% aggregate_only_ctries) {
    # this won't work for all countries - gets the first two digits of iso3
    # to get iso2, but for now just gabon so works there...
    iso2 <- str_sub(ctry_str, 1, 2) 
    
    raw_df <- read_sf(here(
      raw.dir, "Countries", ctry_str, "Shapefiles", paste0("level ", lvl), 
      paste0(ctry_str, "_adm", lvl, ".shp")
      )) %>%
      st_drop_geometry() %>%
      arrange(ADMIN_NAME) %>%
      mutate(
        region_name = ADMIN_NAME,
        region      = row_number(),
        !!paste0("geo", lvl, "_", iso2) := get(paste0("GEOLEVEL", lvl))
      )
    
  } else {
    # read one census - if two census country, doesn't matter which one
    # otherwise, read the existing one
    suffix <- case_when(
      ctry_str %in% single_first_cens_ctries ~ str_sub(c_info$census_yr1, -2),
      ctry_str %in% single_second_cens_ctries ~ str_sub(c_info$census_yr2, -2),
      TRUE ~ c_info$yr2_suffix # default
    )
    
    raw_df <- readRDS(here(
      build.dir, "Countries", ctry_str, "Census", 
      paste0("census", suffix, ".rds")
      ))
  }
  
  # need to get the name of a specific column, named e.g., geo1_ga
  target_geo_col <- raw_df %>% 
    select(matches(paste0("^geo", lvl, "_[a-z]{2}$"))) %>% 
    colnames()
  
  # get the relevant geographic columns
  geo_cols <- if (lvl == 2) {
    c("district", "district_name", "region", "region_name", "geo1", "geo2")
  } else {
    c("region", "region_name", "geo1")
  }
  
  out_df <- raw_df %>%
    select(
      ipums_id = target_geo_col,
      any_of(geo_cols)
    ) %>%
    distinct() 

  # the old version (Final/Archive/1-ID.R) had some other variables
  # but we aren't actually using them to my understanding
  
  if (lvl == 2) {
    out_df <- out_df %>%
      mutate(
        admin_name = district_name,
        admin_name_l2 = district_name, 
        admin_id_l2 = district,
        admin_name_l1 = region_name, 
        admin_id_l1 = region,
        admin_id = admin_id_l2
      )
  } else {
    out_df <- out_df %>%
      mutate(
        admin_name = region_name,
        admin_name_l1 = region_name, 
        admin_id_l1 = region,
        admin_id = admin_id_l1
      )
  }
  
  out_df <- out_df %>%
    mutate(
      country = ctry_str,
      country_name = sapply(country, n_ctry),
      ipums_id = as.character(ipums_id), 
      geo_lvl = lvl,
      census_yr1 = c_info$census_yr1,
      census_yr2 = c_info$census_yr2,
    ) %>% 
    select(country, country_name, ipums_id, everything())
  
  return(out_df)
}

id_df <- countries %>%
  purrr::map_dfr(~{
    message("Processing: ", .x)
    process_geo_units(.x)
  })

# save ----
id_df %<>% 
  var_labels(geo_lvl = "admin unit level")

saveRDS(id_df, here(out.dir, "R", "id.rds"))
write_dta(id_df, here(out.dir, "id.dta"))

id_df <- readRDS(here(out.dir, "R", "id.rds"))

country_census_tbl <- id_df %>%
  group_by(country_name) %>%
  reframe(
    lvl = as.integer(mean(geo_lvl)),
    n_units = n_distinct(ipums_id),
    census_1 = {
      yrs <- sort(unique(c(census_yr1, census_yr2)), na.last = NA)
      yrs[1]
    },
    census_2 = {
      yrs <- sort(unique(c(census_yr1, census_yr2)), na.last = NA)
      if (length(yrs) > 1) yrs[2] else NA_integer_
    }
  ) %>%
  arrange(country_name) %>% 
  remove_rownames()

print(
  xtable::xtable(country_census_tbl),
  include.rownames = FALSE,
  floating = T,
  sanitize.text.function = identity
)
