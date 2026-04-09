#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   3 - Process McGuirk and Nunn (2024) data about pastoralism
#* Note:    Processes their data and matches to grid cells and years  
#* TODO - code is a bit of a mess
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

# reading and cleaning -----
mcguirk_nunn <- read_dta(
  here(raw_dir, "ethnographic/EMG_NN_THPCCC_replication_data.dta")
  )
mcguirk_nunn_mod <- mcguirk_nunn %>% 
  rename(
    pastor_narrow = herdXEApXn12,
    pastor_broad = herdXEApXn1234,
    nbor_pastor_narrow = n1_herdXEApXn12,
    nbor_pastor_broad = n1_herdXEApXn1234,
    xcoord = x,
    ycoord = y,
    ethnic = MAP_name,
    nbor_rain = n1_g_prec_gpcc,
    rain = prec_gpcc,
    ethnic_rain = g_prec_gpcc,
    thp_power_share = cy_np_power, 
    total_agriculture_aid = AD_CORE_TAG_cl, 
    total_non_agriculture_aid = AD_CORE_TNG_cl, 
    other_agricultural_projects = AD_CORE_OAG_cl, 
    other_non_agricultural_projects = AD_CORE_ONG_cl, 
    irrigation_projects = AD_CORE_IR_cl, 
    forestry_projects = AD_CORE_FR_cl, 
    conservation_projects = AD_CORE_CR_cl, 
    land_projects = AD_CORE_LD_cl
    ) %>% 
  select(
    cell, year, xcoord, ycoord, ethnic, Christian, Muslim,
    pastor_narrow, pastor_broad, nbor_pastor_narrow, nbor_pastor_broad,
    rain, nbor_rain, ethnic_rain, thp_power_share,
    total_agriculture_aid, total_non_agriculture_aid, 
    other_agricultural_projects, other_non_agricultural_projects, 
    irrigation_projects, forestry_projects, conservation_projects, land_projects
  )

mcguirk_nunn_sf <- st_as_sf(
  mcguirk_nunn_mod, coords = c("xcoord", "ycoord"), crs = 4326
  ) %>% 
  st_transform(st_crs(afr_crs))

grid_expanded_years <- fread(here(int_dir, "grid_years.csv"))
grid <- st_read(here(int_dir, "grid.gpkg"))

mcguirk_nunn_sf_pastor <- mcguirk_nunn_sf %>% 
  select(-c(
    rain, nbor_rain, ethnic_rain, 
    thp_power_share,
    total_agriculture_aid, total_non_agriculture_aid, 
    other_agricultural_projects, other_non_agricultural_projects, 
    irrigation_projects, forestry_projects, conservation_projects, land_projects
    )) %>% # variables that change by year
  filter(year == 2012) # could be any year

pastor_data_nearest_50km <- st_join(
  grid, mcguirk_nunn_sf_pastor, 
  join = st_nn, # nearest neighbour within 50km
  maxdist = 50000, k = 1) %>%
  select(
    gid, cell, ethnic, Christian, Muslim,
    pastor_narrow, pastor_broad, 
    nbor_pastor_narrow, nbor_pastor_broad # variables that are consistent over years
  )

keys_for_yearly_variation_match <- pastor_data_nearest_50km %>% 
  select(gid, cell) %>% 
  st_set_geometry(NULL)

mcguirk_nunn_yearly_variation <- mcguirk_nunn_sf %>% 
  select(c(rain, nbor_rain, ethnic_rain, 
           thp_power_share,
           total_agriculture_aid, total_non_agriculture_aid, 
           other_agricultural_projects, other_non_agricultural_projects, 
           irrigation_projects, forestry_projects, conservation_projects, land_projects,
           year, cell)) %>% 
  st_set_geometry(NULL)

grid_expanded_years_with_keys <- grid_expanded_years %>% 
  left_join(keys_for_yearly_variation_match, by = "gid") %>% 
  select(gid, cell, year) 

results_list <- list()

for (yr in 1997:2019) {
  grid_year <- grid_expanded_years_with_keys[grid_expanded_years_with_keys$year == yr, ]
  mcguirk_nunn_year <- mcguirk_nunn_yearly_variation[mcguirk_nunn_yearly_variation$year == yr, ]
  
  # Perform the join for the current year
  pastor_data_yearly <- grid_year %>% 
    left_join(mcguirk_nunn_year, by = c("cell", "year"))
  
  # Store the result in the list with the year as the key
  results_list[[as.character(yr)]] <- pastor_data_yearly
}

pastor_data_nearest_50km_for_join <- pastor_data_nearest_50km %>% 
  select(-cell) %>% 
  st_set_geometry(NULL)

yearly_vars_df <- bind_rows(results_list)

final_pastoralism_df <- grid_expanded_years %>%
  select(gid, year) %>% 
  left_join(yearly_vars_df, by = c("gid", "year")) %>% 
  left_join(pastor_data_nearest_50km_for_join, by = "gid") %>% 
  select(-cell)

fwrite(
  final_pastoralism_df, 
  file.path(int_dir, "grid_with_pastor_data.csv")
)
