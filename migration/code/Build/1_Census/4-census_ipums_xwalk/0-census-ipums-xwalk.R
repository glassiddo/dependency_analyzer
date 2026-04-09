# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Feb 21, 2023
# Title:   IPUMS admin unit crosswalk 
# Desc:    
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

geo_xwalk <- function(ctry_str) {
  
  yr <- census_info %>%
    filter(country == ctry_str) %>%
    summarise(
      max_yr = max(c(census_yr1, census_yr2), na.rm = TRUE)
    ) %>%
    pull(max_yr) %>%
    str_sub(3, 4)
  
  geo_l <- census_info %>% filter(country == ctry_str) %>% pull(lvl)
  
  dat_df <- readRDS(
    here(build.dir, "Countries", ctry_str, "Census", paste0("census", yr, ".rds"))
  )
  
  # get name of variable from IPUMS
  vname <- names(dat_df)[str_sub(names(dat_df), 1, 4) == paste0("geo", geo_l)]
  # there may be more than one if there are year specific geometries
  vname <- vname[str_length(vname) == 7]
  ipums_geo <- sym(vname)
  
  if (geo_l == 2) {
    
    codes_df <- dat_df %>% 
      mutate({{ipums_geo}} := as.character(!!ipums_geo)) %>%
      # force it to be a character, a solution to merge with botswana 
      # as botswana regions start with 0 so can't be numeric/double
      select(district, district_name, {{ipums_geo}}) %>% 
      unique() %>% 
      group_by(district_name) %>%
      mutate(N = row_number()) %>%
      pivot_wider(
        values_from = {{ipums_geo}},
        names_from = N,
        names_glue = "geo2_{N}"
      ) 
  }
  else {
    codes_df <- dat_df %>% 
      mutate({{ipums_geo}} := as.character(!!ipums_geo)) %>%
      # see above
      select(region, region_name, {{ipums_geo}}) %>% 
      unique() %>% 
      group_by(region_name) %>%
      mutate(N = row_number()) %>%
      pivot_wider(
        values_from = {{ipums_geo}},
        names_from = N,
        names_glue = "geo1_{N}"
      ) 
  }
  
  # add this infor at front of file
  codes_df %<>%
    mutate(
      country = ctry_str,
      ipums_var = vname,
      admin_unit = geo_l
    ) %>%
    select(country, admin_unit, ipums_var, everything())
  
  return(codes_df)
  
}

#*******************************************************************************
# Build ----

countries <- c( # list of all countries
  "BEN", "BWA", "BFA", "GHA",  
  #"CMR", "MLI", "RWA", "SDN", "TGO", "ZWE", 
  # weren't in it for some reason - single cens countries + cameroon
  # hence am not adding "CIV" nor AGO MDG GAB 
  "GIN", "KEN", "LSO", "MWI", 
  "MUS", "MOZ", "RWA", "SEN", 
  "SLE", "ZAF", "SDN", "TZA", "TGO", 
  "UGA", "ZMB", "ZWE"
)

xwalk <- purrr::map_dfr(countries, geo_xwalk)


# save file in multiple formats
write_dta(xwalk, here(build.dir, "Africa", "Support", "ipums_xwalk.dta"))
saveRDS(xwalk, here(build.dir, "Africa", "Support", "ipums_xwalk.rds"))
