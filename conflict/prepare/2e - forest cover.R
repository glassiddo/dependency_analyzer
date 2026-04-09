#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   3 - Get baseline forest cover at 2000
#* Note:    TBF  
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid)

forest <- fread(here(raw_dir, "climate/ForestCover2000_Africa.csv"))
forest <- st_as_sf(forest, coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(st_crs(afr_crs))

africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf") %>% 
  st_union() %>% 
  st_transform(st_crs(afr_crs))

forest_afr <- st_intersection(forest, africa)

merged_forest <- st_join(
  grid, forest_afr, 
  join = st_nearest_feature # st_intersects may >1 value per gid
  ) %>% 
  st_drop_geometry() %>% 
  select(-year)

write_csv(merged_forest, file.path(int_dir, "grid_forestcover.csv"), append = FALSE)
