#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 
#* Title:   Merge nightlight area into the grid
#* Note:    The code that generates the nl.csv is in GEE
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

nl <- fread(here(raw_dir, "gridDMSP_VIIRS.csv")) %>% 
  select(gid, starts_with("ntlla")) 

grid_nl <- read_sf(here(int_dir, "grid.gpkg")) %>% 
  st_drop_geometry() %>% 
  select(gid, area_grid) %>% 
  mutate(area_grid = area_grid / 10000 ) %>% # to hectares
  left_join(nl, by = "gid") %>% 
  pivot_longer(
    cols = starts_with("ntlla"),
    names_to = "year_sensor",
    values_to = "lit_area"
  ) %>%
  mutate(
    year = as.numeric(str_extract(year_sensor, "[0-9]{4}$"))
  ) %>% 
  group_by(gid, year, area_grid) %>%
  slice(1) %>%  # just use the first sensor in case of two - need to figure it out
  ungroup() %>% 
  mutate(
    ntlla_share = pmin(1, lit_area / area_grid) # make sure not over 100%
  ) %>% 
  filter(year > 1996) %>% 
  select(gid, year, ntlla_share)

fwrite(grid_nl, here(int_dir, "grid_nl.csv"), append = FALSE)
