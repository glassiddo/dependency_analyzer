#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   1 - Create grid 
#* Note:    This creates the grid cells, at 0.5*0.5 degress and larger          
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

### create grid -----
### load data that is used in many occasions
africa_union <- ne_countries(
  scale = "medium", returnclass = "sf", continent = "Africa"
  ) %>% 
  #ignore country borders, 
  st_union() %>%  # as the grids need to be continuous across territories
  st_sf() 

### function to change the cell size
generate_africa_grid <- function(cell_size = 0.5) {
  large_grid <- st_make_grid(
    africa_union, 
    cellsize = cell_size, 
    what = "polygons"
    ) %>% 
  st_sf() # create basic grid around africa

  grid <- st_intersection(large_grid, africa_union) %>% # only take land
    st_transform(
      crs =  st_crs(afr_crs) # all of Africa CS, for area computation
    ) %>%  
    mutate(
      gid = row_number(),
      area_grid = st_area(.) %>% as.numeric()
    )
  
  return(grid)
}

grid <- generate_africa_grid(cell_size = 0.5)

centroids_grid_0_5 <- st_centroid(grid) %>% 
  select(geometry, gid)

## create smaller grids for clustering ----
# define grid sizes and their corresponding names
grid_sizes <- c(1, 2, 3, 4, 5, 10)
grid_names <- paste0("gid_", grid_sizes, "deg")

process_grid_match <- function(grid_0_5_centroids, grid_size, grid_name) {
  grid <- generate_africa_grid(cell_size = grid_size) %>% 
    rename(!!grid_name := gid)
  
  gids_match <- st_join(grid_0_5_centroids, grid, join = st_nearest_feature) %>% 
    st_set_geometry(NULL) %>% 
    select(-area_grid)
  
  return(gids_match)
}

gids_matches <- list()
for (i in seq_along(grid_sizes)) {
  grid_size <- grid_sizes[[i]]
  grid_name <- grid_names[[i]]
  
  # Call the function to process each grid match
  gids_matches[[i]] <- process_grid_match(centroids_grid_0_5, grid_size, grid_name)
}

# Join all the gids_match results with the original grid
grid <- generate_africa_grid(cell_size = 0.5) %>%
  left_join(gids_matches[[1]], by = "gid") %>%
  left_join(gids_matches[[2]], by = "gid") %>%
  left_join(gids_matches[[3]], by = "gid") %>% 
  left_join(gids_matches[[4]], by = "gid") %>% 
  left_join(gids_matches[[5]], by = "gid") %>% 
  left_join(gids_matches[[6]], by = "gid") 


## expand and write ----
grid_expanded_years <- grid %>%
  st_drop_geometry() %>% 
  crossing(year = years)

st_write(grid, 
         file.path(int_dir, "grid.gpkg"), 
         append = FALSE) 
fwrite(grid_expanded_years, 
         file.path(int_dir, "grid_years.csv"),
         append = FALSE) 