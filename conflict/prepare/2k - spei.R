#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   Get SPEI values from  Harari and La Ferrara (2018)
#* Note:    original code is in GEE
#* TODO get the GEE code
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid)

spei <- read_dta(
  here(
    raw_dir, 
    "HarariLaFerrara Replication Folder", "dataset","geoconflict_main.dta"
    )
  )

spei_flt <- spei %>% 
  select(
    cell, year, lat, lon, 
    SPEI4pg, L1_SPEI4pg, L2_SPEI4pg, GSmain_ext_SPEI4pg, L1_GSmain_ext_SPEI4pg, 
    L2_GSmain_ext_SPEI4pg, maincrop_harea
  ) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(st_crs(afr_crs))

results_list <- list()

for (yr in 1997:2011) { # data ends at 2011
  
  harrari_year <- spei_flt %>% 
    filter(year == yr) %>% 
    select(-year) 

  grid_year <- grid %>% 
    mutate(year = yr)
  
  # Spatial join of grid_wdpa_year and precip_sf_year
  merged_data_year <- st_join(
    grid_year, harrari_year, 
    join = st_nearest_feature
    ) # st_intersects may >1 value per gid
  
  # potentially more accurate (allows for empty values) but slower:
  # merged_data_year <- st_join(ha10, grid10, join = st_nn, maxdist = 50000, k = 1)
  
  # Store the result in the list with the year as the name
  results_list[[as.character(yr)]] <- merged_data_year
}

combined_data <- bind_rows(results_list) %>% 
  st_drop_geometry() %>% 
  select(-cell)

write_csv(combined_data, here(int_dir, "spei_hararilaf11.csv"), append = FALSE)
