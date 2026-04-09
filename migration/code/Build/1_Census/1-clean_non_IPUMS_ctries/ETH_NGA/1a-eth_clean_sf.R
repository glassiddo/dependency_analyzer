#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    December, 2025
#* Title:   Clean shapefile for Ethiopia
#* Desc:    Take the IPUMS shapefile for ETH and clean it slightly
#*******************************************************************************

source("code/SSA_env_SetUp.R")

ctry_str <- "ETH"
ctries_dir <- "Countries"
lvl <- 1

eth_map <- read_sf(here(
  raw.dir, ctries_dir, ctry_str, "Shapefiles", paste0("level ", lvl), 
  "geo1_et2007.shp")
) %>% 
  filter(ADMIN_NAME != "Waterbodies") %>% 
  transmute(
    CNTRY_NAME, 
    CNTRY_CODE,
    ADMIN_NAME,
    GEOLEVEL1 = as.numeric(paste0(CNTRY_CODE, IPUM2007)),
    PARENT
  ) 

write_sf(
  eth_map, 
  here(
    raw.dir, ctries_dir, ctry_str, "Shapefiles", paste0("level ", lvl), 
    paste0(ctry_str, "_adm", lvl, ".shp")
  )
)
