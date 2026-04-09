# Author:  Iddo Glass
# Date:    February 11, 2026
# Title:   Update masters map
# Output:  Level 1 and 2 shapefiles with all our admin units
# Desc:      
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

special_ctries <- c("NGA", "ETH", "GAB", "MDG", "AGO", "CIV")
# non_IPUMS countries + CIV
# moz changes later

# read all non ipums countries and bind rows by level ----
read_country_sf_lvl <- function(ctry_code, lvl_target) {
  
  shp_path <- here(
    raw.dir, "Countries", ctry_code, "Shapefiles",
    paste0("level ", lvl_target),
    paste0(ctry_code, "_adm", lvl_target, ".shp")
  )
  
  sf_data <- read_sf(shp_path) %>% 
    mutate(
      country = sapply(CNTRY_NAME, g_ctry3) # get three letter code
    )
  
  if (lvl_target == 1) {
    sf_data <- sf_data %>%
      mutate(GEOLEVEL1 = as.character(GEOLEVEL1))  
  } else {
    sf_data <- sf_data %>%
      mutate(GEOLEVEL2 = as.character(GEOLEVEL2))
  }
  
  return(sf_data)
}

special_lvl1_ctries <- census_info %>%
  filter(country %in% special_ctries, lvl == 1) %>%
  pull(country)

special_lvl2_ctries <- census_info %>%
  filter(country %in% special_ctries, lvl == 2) %>%
  pull(country)

special_sfs_lvl1 <- map_dfr(
  special_lvl1_ctries,
  ~ read_country_sf_lvl(.x, 1)
)

special_sfs_lvl2 <- map_dfr(
  special_lvl2_ctries,
  ~ read_country_sf_lvl(.x, 2)
)

#### ipums
maps <- list()

for (lvl_i in 1:2) {
  lvl_ctries <- census_info %>% 
    filter(
      lvl == lvl_i,
      !country %in% special_ctries
    ) %>% 
    pull(country)
  
  base_map <- read_sf(here(
    raw.dir, "Africa", "Shapefiles", "IPUMS", paste0("level ", lvl_i), 
    paste0("world_geolev",lvl_i, "_2021.shp")
  )) %>% 
    mutate(
      # merge sudan and south sudan (code it as sudan). 
      # our census is 2008 when they were united
      CNTRY_NAME = ifelse(CNTRY_NAME == "South Sudan", "Sudan", CNTRY_NAME),
      CNTRY_CODE = ifelse(CNTRY_CODE == "728", "729", CNTRY_CODE),
      country = sapply(CNTRY_NAME, g_ctry3) # get three letter code
    ) %>% 
    filter(
      country %in% lvl_ctries, 
      !country %in% special_ctries,
      !ADMIN_NAME %in% c(
        "Waterbodies", "Unknown", "Lake Kivu", "Lake Malawi", "Waterbody"
        )
    ) %>% 
    bind_rows(get(paste0("special_sfs_lvl", lvl_i))) %>% 
    select(-c(BPL_CODE, PARENT))
  
  maps[[paste0("lvl", lvl_i)]] <- base_map
}

map1 <- maps$lvl1 
map2 <- maps$lvl2 

for (lvl_i in 1:2) {
  if (lvl_i == 1) {map <- map1} else {map <- map2}
  write_sf(map, here(
    raw.dir, "Africa", "Shapefiles", "IPUMS",
    paste0("level ", lvl_i),
    paste0("world_geolev", lvl_i, "_", "2026b", ".shp")
  ))
}
