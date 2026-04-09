#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    January 2026 
#* Title:   Get wildlife measure
#* Note:    Merges the "Wildlife watching species richness index" into our grid 
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

grid <- read_sf(here(int_dir, "grid.gpkg")) %>%
  select(gid)

# https://africa-knowledge-platform.ec.europa.eu/dataset/wildlife-watching-species-richness-index
wildlife <- read_sf(here(raw_dir, "wildlife", "gridsafprim_50.shp")) %>% 
  st_transform(st_crs(grid)) %>% 
  select(
    #N, # quite similar to TotScore but seems less detailed 
    wdlf_indx = TotScore
    )

wl_inters <- grid %>% 
  st_intersection(wildlife) %>% 
  mutate(int_area = as.numeric(st_area(geom))) %>%
  st_drop_geometry() %>% 
  group_by(gid) %>% 
  reframe(
    wdlf_indx_w = weighted.mean(wdlf_indx, w = as.numeric(int_area), na.rm = TRUE),
    wdlf_indx = mean(wdlf_indx, na.rm = TRUE)
  ) 

grid_wl <- grid %>% 
  st_drop_geometry() %>% 
  left_join(wl_inters, by = "gid") %>% 
  transmute(
    gid,
    wdlf_indx_w = replace_na(wdlf_indx_w, 0),
    wdlf_indx = replace_na(wdlf_indx, 0)
  )

fwrite(grid_wl, here(int_dir, "wildlife.csv"), append = FALSE)
