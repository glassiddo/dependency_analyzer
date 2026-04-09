#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   1 - Create grid 
#* Note:    This creates the grid cells, at 0.5*0.5 degress and larger          
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

# read data ----
pastor <- fread(here(int_dir, "grid_with_pastor_data.csv"))
grid <- st_read(here(int_dir, "grid.gpkg"))

grid_pastor <- grid %>% 
  left_join(pastor %>% 
              filter(year == "2010"), # could be another year as well
            by = "gid"
            ) %>% 
  select(gid, ethnic, geom)

# identify neighbors
neighbor_matrix <- st_touches(grid_pastor)

# define a function for majority ethnicity tests
check_majority_ethnicity_threshold <- function(
    index, neighbor_matrix, dataset, threshold) {
  cell_ethnic <- dataset$ethnic[index]
  neighbors <- neighbor_matrix[[index]]
  
  if (length(neighbors) == 0) {
    return(NA)
  }
  
  neighbor_ethnic <- dataset$ethnic[neighbors]
  same_ethnicity <- sum(neighbor_ethnic == cell_ethnic, na.rm = TRUE)
  total_neighbors <- length(neighbor_ethnic)
  
  return(ifelse(same_ethnicity / total_neighbors >= threshold, 1, 0))
}

# use a wrapper function to add variables for multiple thresholds
add_majority_ethnicity_variables <- function(data, neighbor_matrix, thresholds) {
  for (threshold in thresholds) {
    col_name <- paste0("majority_ethnic_neighbors_", 
                       gsub("\\.", "_", as.character(threshold)))
    data[[col_name]] <- sapply(1:nrow(data), 
                               check_majority_ethnicity_threshold, 
                               neighbor_matrix = neighbor_matrix, 
                               dataset = data, 
                               threshold = threshold)
  }
  return(data)
}

# add vars for thresholds
thresholds <- c(0.25, 0.5, 0.75, 1)
grid_pastor_threshold <- add_majority_ethnicity_variables(
  grid_pastor, neighbor_matrix, thresholds
  ) %>% 
  select(-ethnic) %>% 
  st_drop_geometry() 

write_csv(
  grid_pastor_threshold, 
  file.path(int_dir, "ethnic_neighbours.csv"), 
  append = FALSE
  )
