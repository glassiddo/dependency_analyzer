#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   3 - Process precipitation data 
#* Note:    Processes precipitation data and matches to grid cells and years  
#* TODO make sure I have the code
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

## read, clean merge etc ----
### load data that is used in many occasions
africa_union <- ne_countries(
  scale = "medium", returnclass = "sf", continent = "Africa"
  ) %>% 
  st_transform(st_crs(afr_crs)) %>% 
  st_union()

## extracted from GEE 
precip <- read_csv(here(raw_dir, "climate/Yearly_Precipitation_AFR_2001_2023.csv"))
precip_sf <- st_as_sf(precip, coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(st_crs(afr_crs))

grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid)

grid_expanded_years <- fread(here(int_dir, "grid_years.csv")) %>% 
  select(gid, year)

# only take values in the africa map
precip_africa <- st_intersection(precip_sf, africa_union)

results_list <- list()

for (yr in 2001:2023) {
  
  precip_sf_year <- precip_africa %>% 
    filter(year == !!yr) %>% 
    select(-year) %>% 
    mutate(
      precipitation = precipitation / 10 / 12 # average monthly in cm
    )
  
  grid_year <- grid %>%
    mutate(year = yr)
  
  # Spatial join of grid_wdpa_year and precip_sf_year
  merged_data_year <- st_join(
    grid_year, precip_sf_year, 
    join = st_nearest_feature
  ) %>% # st_intersects may >1 value per gid
  st_drop_geometry()
  
  # Store the result in the list with the year as the name
  results_list[[as.character(yr)]] <- merged_data_year
}

combined_precip_data <- bind_rows(results_list)

fwrite(combined_precip_data, file.path(int_dir, "grid_precip.csv"))