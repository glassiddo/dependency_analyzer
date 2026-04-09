# Author:  Sam Marshall
# Date:    July 12, 2022
# Title:   Build centroids
# Output:  
# Desc:    Build district centroids and spatial dfs from IPUMS data         
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

#*******************************************************************************
# World ----
#*******************************************************************************

## level 1 ----
# import shape file data
africa_sf <- read_sf(
    here(raw.dir, "Africa", "Shapefiles", "IPUMS", "level 1",
    "world_geolev1_2026b.shp")
  ) %>%  
  rowwise() %>% 
  select(
    country, 
    region_name = ADMIN_NAME,
    ipums_id = GEOLEVEL1
  ) %>%
  mutate(
    region_name = iconv(region_name, "latin1", "ASCII", "byte") %>%
      str_replace_all(. , "<c3><a1>", "a") %>%
      str_replace_all(. , "<c3><ad>", "a") %>%
      str_replace_all(. , "<c3><a8>", "e") %>%
      str_replace_all(. , "<c3><a9>", "e") %>%
      str_replace_all(. , "<c3><89>", "e") %>%
      str_replace_all(. , "<c3><af>", "i") %>%
      str_replace_all(. , "<c3><b3>", "o") %>%
      str_replace_all(. , "<c5><93>", "e") %>%
      str_replace_all(. , "<c3><ba>", "u") %>%
      str_replace_all(. , "<c3><a7>", "c") %>%
      str_to_title(.)
  ) 

# get centriods of each region (level 1 geometry)
#NB st_centroid doesn't work for Senegal because point doesn't fall on surface
sf_use_s2(FALSE)
centroids_sf <- africa_sf %>%
  #filter(!st_is_empty(geometry)) %>% # ignore empty geoms (e.g. "special region")
  #filter(str_sub(ipums_id, -2) != "88") %>%  # ignore waterbodies/lakes/unknown
  st_point_on_surface()

sf_use_s2(TRUE)
saveRDS(centroids_sf, here(build.dir, "Africa", "Maps", "l1_centriods.rds"))
saveRDS(africa_sf, here(build.dir, "Africa", "Maps", "l1_sf.rds"))

rm(africa_sf, centroids_sf)

## level 2 ----
# import shape file data
africa_sf <- read_sf(
    here(raw.dir, "Africa", "Shapefiles", "IPUMS", "level 2",
    "world_geolev2_2026b.shp")
  ) %>% 
  rowwise() %>% 
  select(
    country,
    district_name = ADMIN_NAME,
    ipums_id = GEOLEVEL2
  ) %>%
  mutate(
    district_name = iconv(district_name, "latin1", "ASCII", "byte") %>%
      str_replace_all(. , "<c3><a1>", "a") %>%
      str_replace_all(. , "<c3><ad>", "a") %>%
      str_replace_all(. , "<c3><a8>", "e") %>%
      str_replace_all(. , "<c3><a9>", "e") %>%
      str_replace_all(. , "<c3><89>", "e") %>%
      str_replace_all(. , "<c3><af>", "i") %>%
      str_replace_all(. , "<c3><b3>", "o") %>%
      str_replace_all(. , "<c5><93>", "e") %>%
      str_replace_all(. , "<c3><ba>", "u") %>%
      str_replace_all(. , "<c3><a7>", "c") %>%
      str_to_title(.)
  )

moz_df <- africa_sf %>% 
  filter(country == "MOZ") %>% 
  ren_moz(lake = FALSE) %>% 
  moz_id(district_name, ipums_id) %>% 
  group_by(country, district_name, ipums_id) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop') 

part_sf <- africa_sf %>% filter(country != "MOZ")

africa_sf <- bind_rows(part_sf, moz_df)

sf_use_s2(FALSE)
centroids_sf <- africa_sf %>%
  st_point_on_surface()

sf_use_s2(TRUE)

saveRDS(centroids_sf, here(build.dir, "Africa", "Maps", "l2_centriods.rds"))
saveRDS(africa_sf, here(build.dir, "Africa", "Maps", "l2_sf.rds"))

rm(africa_sf, centroids_sf)
