#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   3 - Process mines data from Berman et al. (2017)
#* Note:    TBF  
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid)

grid_expanded <- fread(here(int_dir, "grid_years.csv")) %>% 
  select(gid, year)

mines <- read_dta(here(raw_dir, "mines/BCRT_baseline.dta"))
mines <- st_as_sf(mines, coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(st_crs(afr_crs))

mines_baseline <- mines %>%
  st_set_geometry(NULL) %>% 
  filter(year == 1997) %>%
  rename(gid_mines = gid) %>%
  select(
    gid_mines, mainmineral, mainmineral_around, mines, mines_a, mines_around, 
    nb_mines_a, nb_diamond, some_diamond, share_prod_main
    ) %>% 
  rename_with(~ paste0(., "_baseline"), -gid_mines) 

mines_current <- mines %>%
  rename(gid_mines = gid) %>%
  select(
    year, gid_mines,
    mainmineral, mainmineral_around, mines, mines_a, mines_around, 
    nb_mines_a, nb_diamond, some_diamond, share_prod_main,
    mining_area_prior5, mining_area_priorall
    ) %>%
  left_join(mines_baseline, by = "gid_mines")

mines_merg <- st_join(grid, mines_current, join = st_intersects) %>% 
  st_drop_geometry()
  # most coastal cells remain empty
  # might be a reasonable assumption here - unlike pastoral areas, 
  # mines are stationary and presumably less likely to be in small coastal areas
  # this leaves a lot of NAs; they can be replaced with zeros, perhaps?
  
mines_years <- grid_expanded %>% 
  left_join(mines_merg, by = c("year", "gid"))

write_csv(mines_years, file.path(int_dir, "mines.csv"), append = FALSE)
