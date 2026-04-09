#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    April 1, 2023
#* Title:   IDs
#* Desc:    Build geographical unit IDs Dataset for each admin unit that we use
#*******************************************************************************

# TODO - add an error/warning if census_yr1=year isn't identical to the csv 

# Set Up ----
source("code/SSA_env_SetUp.R")

#* create the following (+ naming convention)
#* 1. geo name
#* 2. geo id
#* 3. ipums id
#* 4. country 

get_units <- function(out_df, ctry_str, lvl, census_yr1, census_yr2){
  geo_cols <- if (lvl == 2) {
    c("district", "region", "geo")
  } else {
    c("region", "geo1")
  }
  
  out_df <- out_df %>%
    select(starts_with(geo_cols),
           -ends_with(c("0","1","2","3","4","5","6","7","8","9", "geomig1_p"))) %>%
    distinct() %>%
    rename_with(~"geolevel1", starts_with("geo1"))
  
  if (lvl == 2) {
    out_df <- out_df %>%
      rename_with(~"geolevel2", starts_with("geo2")) %>%
      rename(admin_name_l2 = district_name, admin_id_l2 = district,
             admin_name_l1 = region_name, admin_id_l1 = region) %>%
      mutate(
        ipums_id = as.character(geolevel2),
        admin_name = admin_name_l2,
        admin_id = admin_id_l2,
        across(c(geolevel1, geolevel2), ~as.character(.x))
      )
  } else {
    out_df <- out_df %>%
      rename(admin_name_l1 = region_name, admin_id_l1 = region) %>%
      mutate(
        geolevel1 = as.character(geolevel1),
        ipums_id = as.character(geolevel1),
        admin_name = admin_name_l1,
        admin_id = admin_id_l1
      )
  }
  
  out_df <- out_df %>%
    mutate(
      country = ctry_str,
      country_name = sapply(ctry_str, n_ctry),
      geo_lvl = lvl,
      census_yr1 = census_yr1
    )
  
  if (!is.null(census_yr2)) {
    out_df <- out_df %>% mutate(census_yr2 = census_yr2)
  }
  
  reloc_cols <- c("country_name", "ipums_id", "admin_name", "admin_id", 
                  "geo_lvl", "census_yr1")
  if (!is.null(census_yr2)) {
    reloc_cols <- c(reloc_cols, "census_yr2")
  }
  
  out_df %>% relocate(all_of(reloc_cols))
}

f_id <- function(ctry_str) {
  
  c_info <- census_info %>% filter(country == ctry_str)
  
  census_yr1 <- c_info %>% pull(census_yr1)
  census_yr2 <- c_info %>% pull(census_yr2)
  lvl <- c_info %>% pull(lvl)

  # read the second census - the first could also be used...
  # but no reason to read both as the only diff should be in the year
  # which can be extracted from census_info
  yr2 <- c_info %>% pull(yr2_suffix)
  
  out_df <- readRDS(here(
    build.dir, "Countries", ctry_str, "Census", paste0("census", yr2, ".rds")
  ))
  
  get_units(out_df, ctry_str, lvl, census_yr1, census_yr2)
}

f_id1 <- function(ctry_str) {
  #* for countries with only one census
  c_info <- census_info %>% filter(country == ctry_str)
  
  # get the single census year
  census_yr <- c_info %>% 
    remove_empty(which = "cols") %>%
    rename(census_yr = any_of(c("census_yr1", "census_yr2"))) %>% 
    pull(census_yr)  
  
  yr <- str_sub(census_yr, -2)
  lvl <- c_info %>% pull(lvl)
  
  # read censuses
  out_df <- readRDS(here(
    build.dir, "Countries", ctry_str, "Census", paste0("census", yr, ".rds")
  ))
  
  if (lvl == 2) {
    out_df %<>% 
      select(starts_with(c("district", "region", "geo")),
             -ends_with(c("0","1","2","3","4","5","6","7","8","9")), 
             census_yr1 = year) %>%
      distinct() %>%
      rename_with(~"geolevel2", starts_with("geo2") ) %>%
      rename_with(~"geolevel1", starts_with("geo1") ) %>%
      rename(admin_name_l2 = district_name, admin_id_l2 = district,
             admin_name_l1 = region_name, admin_id_l1 = region) %>%
      mutate(
        country = ctry_str,
        country_name = sapply(ctry_str, n_ctry),
        ipums_id = as.character(geolevel2),
        admin_name = admin_name_l2,
        admin_id = admin_id_l2,
        geo_lvl = lvl,
        across(c(geolevel1, geolevel2), ~as.character(.x))) %>%
      relocate(country_name, ipums_id, admin_name, admin_id, geo_lvl, census_yr1)
  } else {
    out_df %<>% 
      select(starts_with(c("region", "geo1")),
             -ends_with(c("0","1","2","3","4","5","6","7","8","9")), 
             census_yr1 = year) %>%
      distinct() %>%
      rename_with(~"geolevel1", starts_with("geo1") ) %>%
      rename(admin_name_l1 = region_name, admin_id_l1 = region) %>%
      mutate(
        country = ctry_str,
        country_name = sapply(ctry_str, n_ctry),
        geolevel1 = as.character(geolevel1),
        ipums_id = as.character(geolevel1),
        admin_name = admin_name_l1,
        admin_id = admin_id_l1,
        geo_lvl = lvl
      ) %>%
      relocate(country_name, ipums_id, admin_name, admin_id, geo_lvl, census_yr1)
  }
  return(out_df)
}

f_id0 <- function(ctry_str) {
  #* for countries with no microdata, where we get aggregates
  c_info <- census_info %>% filter(country == ctry_str)
  
  census_yr1 <- c_info %>% pull(census_yr1)
  census_yr2 <- c_info %>% pull(census_yr2)
  lvl <- c_info %>% pull(lvl)
  
  # get the data from the shapefile
  out_df <- read_sf(here(
    raw.dir, "Countries", ctry_str, "Shapefiles", paste0("level ", lvl),
    paste0(ctry_str, "_adm", lvl, ".shp")
    )) %>% 
    st_drop_geometry() 
  
  iso2 <- ctry_str %>% # won't always work. TODO - fix 
    str_sub(1,2)

  if (lvl == 2) {
    ### TODO - level 2 isn't actually going to work as it's missing region_name
    out_df %>%
      arrange(ADMIN_NAME) %>%
      transmute(
        district_name = ADMIN_NAME,
        district = row_number(),
        !!paste0("geo2_", iso2) := GEOLEVEL2
      ) 
  } else {
    out_df <- out_df %>%
      arrange(ADMIN_NAME) %>%
      transmute(
        region_name = ADMIN_NAME,
        region = row_number(),
        !!paste0("geo1_", iso2) := GEOLEVEL1
      )
  }
  get_units(out_df, ctry_str, lvl, census_yr1, census_yr2)
}


#*******************************************************************************

countries <- c( # list of all countries
  "AGO", "BEN", "BWA", "BFA", "CMR",
  "GHA", "GIN", "CIV", "KEN", "LSO",
  "MWI", "MLI", "MUS", "MOZ", "RWA",
  "SEN", "SLE", "ZAF", "SDN", "TZA",
  "TGO", "UGA", "ZMB", "ZWE",
  "MDG", "GAB", "GAB"
)

single_cens_ctries <- c("AGO", "CIV", "MWI", "RWA", "SDN", "TGO", "ZWE")

no_cens_ctries <- c("GAB") # countries where we exclusively get aggregates

get_country_data <- function(ctry_str) {
  if (ctry_str %in% single_cens_ctries) f_id1(ctry_str) 
  if (ctry_str %in% no_cens_ctries) f_id0(ctry_str)
  else f_id(ctry_str)
}

id_df <- purrr::map_dfr(countries, get_country_data)

# save ----
id_df %<>% 
  var_labels(geo_lvl = "admin unit level")

saveRDS(id_df, here(out.dir, "R", "id.rds"))
write_dta(id_df, here(out.dir, "id.dta"))
