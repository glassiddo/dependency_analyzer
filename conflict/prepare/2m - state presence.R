#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   Predicted state presence from Agneman et al. (2025) 
#* Note:    
#*******************************************************************************

source("do/conf_biodiversity_setup.R")
### This code takes the predicted local level state presence from Agneman et al. (2025) 
### "The uneven reach of the state: A novel approach to mapping local state presence"
grid <- read_sf(here(int_dir, "grid.gpkg")) %>% 
  select(gid)

# read the predicted state presence TIF and project to the corresponding crs of the grid
state <- rast(here(raw_dir, "local_state_capacity", "predicted_state_presence.tif")) %>% 
  project(afr_crs)

# take the mean value of each 0.5*0.5 grid cell based on the 0.05*0.05 state presence
grid_values <- extract(state, grid, fun=mean, na.rm=TRUE) 

grid_state <- grid %>% 
  left_join(grid_values %>% 
              rename(gid = ID), 
            by = "gid") %>% 
  st_drop_geometry()

fwrite(grid_state, here(int_dir, "state_presence.csv"), append = FALSE)
