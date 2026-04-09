#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    December, 2025
#* Title:   Clean shapefile for Nigeria
#* Desc:    Take the IPUMS shapefile for NGA and clean it slightly
#*******************************************************************************

source("code/SSA_env_SetUp.R")

ctry_str <- "NGA"
ctries_dir <- "Countries"
lvl <- 1

nga_map <- read_sf(here(
  raw.dir, ctries_dir, ctry_str, "Shapefiles", paste0("level ", lvl), 
  "geo1_ng2006_2010.shp"
)) %>%
  filter(ADMIN_NAME != "Lake Chad") %>% 
  transmute(
    CNTRY_NAME, 
    CNTRY_CODE,
    ADMIN_NAME,
    GEOLEVEL1 = as.numeric(GEOLEVEL1),
    PARENT
  ) 

write_sf(
  nga_map, 
  here(
    raw.dir, ctries_dir, ctry_str, "Shapefiles", paste0("level ", lvl), 
    paste0(ctry_str, "_adm", lvl, ".shp")
  )
)

