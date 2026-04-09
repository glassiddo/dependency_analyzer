#* Project: Migration Africa
#* Author:  Sam Marshall, edited by Iddo Glass
#* Date:    Sep 19, 2023
#* Title:   Create sample units map
#* Desc:    Shape Files from: https://geoportal.icpac.net/
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

#*******************************************************************************
# read data ----
#*******************************************************************************

id_df <- readRDS(here(out.dir, "R", "id.rds"))

# sample geography lists
l2_list <- id_df %>% filter(geo_lvl == 2) 
l2_list <- unique(l2_list$country)
l1_list <- id_df %>% filter(geo_lvl == 1) 
l1_list <- unique(l1_list$country)

#shapefiles
l2_sf <- readRDS(here(build.dir, "Africa", "Maps", "l2_sf.rds")) %>%
  filter(country %in% l2_list)
l1_sf <- readRDS(here(build.dir, "Africa", "Maps", "l1_sf.rds")) %>% 
  filter(country %in% l1_list)

#*******************************************************************************
# Merge and clean ----
#*******************************************************************************

sample_sf <- bind_rows(l1_sf, l2_sf) %>%
  inner_join(id_df, by = c("country", "ipums_id")) %>% 
  # the following are only necessary if using left_join... 
  # filter(!st_is_empty(.)) %>% 
  # filter(str_sub(ipums_id, -2) != "88") %>% # ignore waterbodies/unknown
  select(country, ipums_id, admin_name, geometry)

st_write(sample_sf, here(build.dir, "Africa", "Maps", "SampleUnits.shp"), 
         append = FALSE)
