#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    December, 2025
#* Title:   Clean shapefile for Gabon
#* Desc:    Take the IPUMS shapefile for GAB and clean it slightly
#*******************************************************************************

source("code/SSA_env_SetUp.R")

gab_sf1 <- read_sf(
  here(raw.dir, "Countries", "GAB", "Shapefiles", "pre_cleaning",
       "gab_admbndp1_1m_salb.shp"))
# there is an issue around the provinces of Nyanga and Ogooue-Maritime
# they were changed proabbly around 2016, i was unable to easily download a correct map; 
# here and in wiki correct borders - https://www.citypopulation.de/en/gabon/admin/
# but in both the census maps of 1993 and of 2013 it appears to be part of Ogooue-Maritime
# the issue is small and there are no large municipalities in the affected area
# and there's no easy fix, so stick with it

gab_sf1 <- gab_sf1 %>% 
  mutate(
    CNTRY_NAME = "Gabon",
    CNTRY_CODE = "266",
    REGION_CODE = str_sub(ADM1_CODE, -3, -1),
    GEOLEVEL1 = as.character(paste0(CNTRY_CODE, REGION_CODE)),
  ) %>% 
  select(
    CNTRY_NAME, 
    ADMIN_NAME = ADM1_NAME, 
    CNTRY_CODE,
    GEOLEVEL1, 
    #PARENT = CNTRY_CODE, 
    geometry
  )

write_sf(gab_sf1, here(
  raw.dir, "Countries", "GAB", "Shapefiles", "level 1", 
  "GAB_adm1.shp")
)
