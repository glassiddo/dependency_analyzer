#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   3 - Process population data
#* Note:    TBF  
#* TODO - code is a bit of a mess
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

# population data ----
grid_expanded_years <- fread(here(int_dir, "grid_years.csv"))
grid <- st_read(here(int_dir, "grid.gpkg"))
nc_file <- nc_open(
  here(raw_dir, "population/gpw_v4_population_count_rev11_30_min.nc")
  )
pop_data <- ncvar_get(
  nc_file, 
  "Population Count, v4.11 (2000, 2005, 2010, 2015, 2020): 30 arc-minutes"
  )
lon <- ncvar_get(nc_file, "longitude")
lat <- ncvar_get(nc_file, "latitude")
pop_years <- c(2000, 2005, 2010, 2015, 2020)
pop_list <- list()

for (i in seq_along(pop_years)) {
  year_data <- pop_data[,,i]
  year_df <- reshape2::melt(
    year_data, varnames = c("lon", "lat"), value.name = as.character(pop_years[i]))
  pop_list[[i]] <- year_df
}

pop_df <- Reduce(function(x, y) merge(x, y, by = c("lon", "lat")), pop_list)
pop_df$lon <- lon[pop_df$lon]
pop_df$lat <- lat[pop_df$lat]

pop_df <- pop_df %>% 
  drop_na() %>% 
  rename(
    xcoord = lon,
    ycoord = lat,
  )
pop_sf <- st_as_sf(pop_df, coords = c("xcoord", "ycoord"), crs = 4326)

grid <- grid %>% 
  select(gid, geom)

##### what to do with missing cells -----
# merged_data <- st_join(grid, pop_sf_buffered, 
#                             join = st_intersects) 
# st_intersects may >1 value per gid
# st_nearest_feature guarantees all gids will have a close value but overestimates

pop_squares <- st_make_grid(pop_sf, 
                            cellsize = c(0.5, 0.5),
                            what = "polygons",
                            square = TRUE) %>%
  st_sf() %>%
  st_filter(pop_sf)

squares <- st_make_grid(pop_sf, 
                        cellsize = c(0.5, 0.5),
                        what = "polygons",
                        square = TRUE) %>%
  st_sf() %>%
  mutate(grid_id = row_number())

points_with_grid <- pop_sf %>%
  mutate(grid_id = sapply(st_intersects(geometry, squares), function(x) ifelse(length(x) > 0, x[1], NA))) %>% 
  st_set_geometry(NULL)

pop_sf_buffered <- squares %>%
  inner_join(points_with_grid, by = "grid_id") %>%
  select(-grid_id) %>%  
  mutate(
    id = row_number(),
    cent = st_centroid(geometry)
  )


africa_bbox <- st_bbox(
    c(xmin = -18.0, ymin = -35.0, xmax = 52.0, ymax = 38.0), crs = st_crs(4326)
    )
africa_sf <- st_intersection(pop_sf_buffered, st_as_sfc(africa_bbox)) %>% 
  st_transform(st_crs(afr_crs))

rm(pop_sf, points_with_grid, squares, pop_squares, africa_bbox, 
   pop_sf_buffered, pop_list, nc_file, year_data, year_df, pop_df)

nearest <- st_join(grid, africa_sf, join = st_nn, maxdist = 50000, k = 1)

nearest_duplicates <- nearest %>%
  select(-cent) %>%  # Assuming 'cent' is a column to exclude
  group_by(id) %>%
  filter(n() > 1) %>%
  ungroup()

summarized_data <- nearest_duplicates %>%
  st_set_geometry(NULL) %>% 
  group_by(id) %>%
  summarise(
    X2000 = mean(`2000`, na.rm = TRUE) / n(), 
    X2005 = mean(`2005`, na.rm = TRUE) / n(), 
    X2010 = mean(`2010`, na.rm = TRUE) / n(), 
    X2015 = mean(`2015`, na.rm = TRUE) / n(), 
    X2020 = mean(`2020`, na.rm = TRUE) / n()
  )

# Step 3: Join the summarized data with the filtered dataset and replace values
result <- nearest_duplicates %>%
  drop_na() %>% 
  left_join(summarized_data, by = "id") %>%
  mutate(
    `2000` = X2000,
    `2005` = X2005,
    `2010` = X2010,
    `2015` = X2015,
    `2020` = X2020
  ) %>%
  select(-X2000, -X2005, -X2010, -X2015, -X2020) %>%  # Drop the temporary columns
  st_set_geometry(NULL)  # Remove spatial component if needed 

# Step 4: Update year values in the original data frame
updated_nearest <- nearest %>%
  left_join(result %>% select(gid, `2000`, `2005`, `2010`, `2015`, `2020`), by = "gid") %>%
  mutate(
    `2000` = coalesce(`2000.y`, `2000.x`),
    `2005` = coalesce(`2005.y`, `2005.x`),
    `2010` = coalesce(`2010.y`, `2010.x`),
    `2015` = coalesce(`2015.y`, `2015.x`),
    `2020` = coalesce(`2020.y`, `2020.x`)
  ) %>%
  select(-`2000.x`, -`2000.y`, -`2005.x`, -`2005.y`, 
         -`2010.x`, -`2010.y`, -`2015.x`, -`2015.y`, -`2020.x`, -`2020.y`, -cent)

merged_data <- updated_nearest
rm(nearest, summarized_data, result, 
   nearest_duplicates, africa_sf, updated_nearest)

##### fill missing years and convert data format ----

replace_na <- function(x) {
  x[is.na(x)] <- quantile(x, probs = 0.0, na.rm = TRUE)
  return(x)
} 

merged_data <- merged_data %>%
  mutate(across(c(`2000`, `2005`, `2010`, `2015`, `2020`), 
                replace_na # replace grids with NA population with 0
  ))
pop_long <- merged_data %>%
  pivot_longer(cols = starts_with("2"), names_to = "year", values_to = "population") %>% 
  mutate(
    year = as.numeric(year)
  )

# Function to perform log-linear interpolation and extrapolation for a single group
log_linear_interpolate <- function(df, specific_years) {
  if (any(df$year %in% specific_years & df$population == 0)) {
    # If any specific years have zero population, set population to zero for all years
    return(data.frame(year = seq(1997, 2024), population = 0))
  }
  
  years <- df$year
  log_population <- log(df$population)
  
  model <- lm(log_population ~ years)
  
  # Interpolating for each year between 1997 and 2024
  full_years <- seq(1997, 2024)
  interpolated_log_population <- predict(model, newdata = data.frame(years = full_years))
  interpolated_population <- exp(interpolated_log_population)
  
  data.frame(year = full_years, population = interpolated_population)
}

interpolated_df <- pop_long %>%
  group_by(gid) %>%
  filter(!is.na(population) & is.finite(population)) %>%
  group_modify(~ log_linear_interpolate(.x, pop_years)) %>%
  ungroup()

pop_data_full <- grid_expanded_years %>%
  left_join(interpolated_df[, c("population", "year", "gid")], by = c("gid", "year")) %>% 
  select(gid, year, population)

write_csv(pop_data_full, 
          file.path(int_dir, "pop_data_full.csv"), append = FALSE)
