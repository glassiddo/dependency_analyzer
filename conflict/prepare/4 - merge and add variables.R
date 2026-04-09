#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   Merge all datasets to a single df
#* Note:
#*******************************************************************************

source("do/conf_biodiversity_setup.R")
# setwd("C:/Users/o.vandeneynde/Nextcloud/conflict and protected areas/")

grid_expanded <- fread(here(int_dir, "grid_years.csv")) %>% 
  filter(year < 2021)

centroids <- st_read(here(int_dir, "grid.gpkg")) %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% # to extract grid cells' centroids 
  mutate(
    centroids = st_centroid(geom),
    lon = st_coordinates(centroids)[,1],
    lat = st_coordinates(centroids)[,2]
  ) %>% 
  st_drop_geometry() %>% 
  select(gid, lon, lat)

### read everything ----
constants_file_list <- list( # constant over time, join over gid only
  ## TODO the trees code was done by guy, need to ask for code
  trees = "africa_grid_expensaive_trees_ESRI_102022.csv",
  animals = "animals2.csv", 
  countries = "country_climatezones.csv",
  forestcover = "grid_forestcover.csv",
  ethnic_nbors = "ethnic_neighbours.csv",
  subsistance = "subsistance.csv",
  state_pres = "state_presence.csv",
  distances = "distances_capital_coast.csv",
  wildlife = "wildlife.csv"
)

varying_file_list <- list( # varying over time, join over gid and year
  acled = "acled_summary.csv",
  tourism = "tourism.csv",
  spei = "spei_hararilaf11.csv",
  #birds = "birds.csv",
  inat = "inaturalist.csv",
  mines = "mines.csv", # both baseline and yearly vars
  pastor = "grid_with_pastor_data.csv",
  precipitaiton = "grid_precip.csv",
  population = "pop_data_full.csv",
  wdpa = "grid_wdpa.csv",
  nightlight = "grid_nl.csv"
)

# load datasets
constants_data <- lapply(constants_file_list, function(f) fread(here(int_dir, f))) 
constants_data <- modifyList(constants_data, list(centroids = centroids))

varying_data <- lapply(varying_file_list, function(f) fread(here(int_dir, f)))

# join constants over gid
grid_with_constants <- reduce(
  constants_data, ~ full_join(.x, .y, by = "gid"), 
  .init = grid_expanded
  )

# join constants over gid and year
grid_joined <- reduce(
  varying_data, ~ full_join(.x, .y, by = c("gid", "year")), 
  .init = grid_with_constants
  )

rm(grid_expanded, grid_with_constants, constants_data, varying_data)

### create many variables and 

big_game_animals <- c("cape_buffalo", "elephant", "lion", "leopard", "rhino")

grid_detailed <- grid_joined %>% 
  rename_with(str_to_lower) %>% # rename to lowercase
  clean_names() %>% # ensure all names are valid syntactically
  setNames(str_replace_all(names(.), "^\\d+", "v")) %>%  # Ensure names don’t start with numbers
  rename_with(~ gsub("/", "_", .x))  %>%        # replace slashes with underscores
  rename_with(~ gsub(" ", "_", .x)) %>%        # replace spaces with underscores
  filter(year < 2024) %>%  # cause conflict data is incomplete that year
# could be updated with the new data
  mutate(
    country_id = as.numeric(as.factor(country)),
    country_year = interaction(country, year, sep = "_"),
    cls_year = interaction(cls, year, sep = "_"),
    over5 = ifelse(share_area_protected > 0.05, 1, 0),
    over10 = ifelse(share_area_protected > 0.10, 1, 0),
    over20 = ifelse(share_area_protected > 0.20, 1, 0),
    over30 = ifelse(share_area_protected > 0.30, 1, 0),
    over40 = ifelse(share_area_protected > 0.40, 1, 0),
    over50 = ifelse(share_area_protected > 0.50, 1, 0),
    area_grid = area_grid / 1000000, # convert to km2
    across(
      matches("^(fatalities|events)"),  
      ~ as.integer(. > 0),   
      # add binary versions to all cols that start with events|fatalities
      .names = "{.col}_dum" 
    ),
    fatalities_dum = ifelse(total_fatalities > 0, 1, 0),
    conflicts_dum = ifelse(total_events > 0, 1, 0),
    lpop = ifelse(population==0, NA, log(population)),
    ihs_pop = asinh(population),
    any_tree = ifelse(
      (  
      pterocarpus_indicator == 1 | 
      diospyros_indicator == 1 | 
      dalbergia_indicator == 1
      ), 1, 0
    ),
    climate = case_when(
      str_starts(cls, "C") ~ "temperate",
      str_starts(cls, "E") ~ "polar",
      str_starts(cls, "BW") ~ "desert",
      str_starts(cls, "BS") ~ "semi-arid",
      str_starts(cls, "Am") ~ "tropic monsoon",
      str_starts(cls, "Aw") | str_starts(cls, "As") ~ "savannah",
      str_starts(cls, "Af") ~ "tropic wet",
      TRUE ~ NA_character_  # Default to NA if none of the conditions match
    ),
    country_group = case_when(
      iso_a3 %in% c("DZA", "EGY", "LBY", "MAR", "TUN", "SDN") ~ "North Africa",
      iso_a3 %in% c("ETH", "KEN", "SOM", "UGA", "TZA", "RWA", "BDI", "ERI", "DJI", "SSD") ~ "East Africa",
      iso_a3 %in% c("AGO", "CMR", "CAF", "COD", "COG", "GAB", "GNQ", "TCD") ~ "Central Africa",
      iso_a3 %in% c("BWA", "LSO", "NAM", "ZAF", "ZMB", "ZWE", "SWZ", "MOZ", "MWI") ~ "Southern Africa",
      iso_a3 %in% c("GHA", "NGA", "SEN", "CIV", "MLI", "NER", "BEN", 
                    "TGO", "BFA", "GMB", "GIN", "MRT", "SLE", "LBR", "GNB") ~ "West Africa",
      iso_a3 %in% c("MDG", "COM", "MUS", "SYC", "STP", "CPV") ~ "Islands",
      TRUE ~ "Other"
    ),
    big_game = ifelse(num_big_game > 0, 1, 0),
    big5 = ifelse(num_big_game == 5, 1, 0),
    big4 = ifelse(num_big_game >= 4, 1, 0),
    big3 = ifelse(num_big_game >= 3, 1, 0),
    big2 = ifelse(num_big_game >= 2, 1, 0),
    big4only = ifelse(num_big_game == 4, 1, 0),
    big3only = ifelse(num_big_game == 3, 1, 0),
    big2only = ifelse(num_big_game == 2, 1, 0),
    across(
      all_of(big_game_animals),
      ~ ifelse(big3 == 1 & .x == 0, 1, 0),
      .names = "big3_missing_{col}"
      ),
    across(
      all_of(big_game_animals),
      ~ ifelse(big4 == 1 & .x == 0, 1, 0),
      .names = "big4_missing_{col}"
    ),
    across(
      all_of(big_game_animals),
      ~ ifelse(big4 == 1 & .x == 1, 1, 0),
      .names = "big4_incl_{col}"
    ),
    hunting = ifelse(hunting_geom > 0, 1, 0),
    gathering = ifelse(gathering_geom > 0, 1, 0),
    animal_hus = ifelse(animal_husbandry_geom > 0, 1, 0),
    fishing = ifelse(fishing_geom > 0, 1, 0),
    majority_nbors_diff = ifelse(majority_ethnic_neighbors_0_5 == 1, 0, 1),
    ihs_conflict = asinh(total_events),
    ihs_fatalities = asinh(total_fatalities),
    west = ifelse(country_group == "West Africa", 1, 0),
    north = ifelse(country_group == "North Africa", 1, 0),
    east = ifelse(country_group == "East Africa", 1, 0),
    centre = ifelse(country_group == "Central Africa", 1, 0),
    south = ifelse(country_group == "Southern Africa", 1, 0),
    islands = ifelse(country_group == "Islands", 1, 0)
  ) %>%  
  group_by(gid) %>%
  mutate(
    mean_precip = mean(precipitation, na.rm = TRUE), 
    sd_precip = sd(precipitation, na.rm = TRUE),
    rainfall_shock = ifelse(precipitation <= mean_precip - sd_precip, 1, 0)
  ) %>%
  ungroup() %>% 
  select(gid, year, country, iso_a3, everything()) 

rm(grid_joined)

grid_detailed <- grid_detailed %>% 
  rename(
    # avg_distinct_birds_m_roll_10yr = avg_distinct_birds_month_roll_10yr,
    # avg_distinct_birds_m_roll_10_5yr = avg_distinct_birds_month_roll_10yr,
    # avg_distinct_birds_m_roll_5yr = avg_distinct_birds_month_roll_10yr,
    # avg_distinct_birds_m_roll_2yr = avg_distinct_birds_month_roll_2yr,
    eve_violence_ag_civ_dum = events_violence_against_civilians_dum,
    eve_explosions_dum = events_explosions_remote_violence_dum,
    eve_strategic_dev_dum = events_strategic_developments_dum,
    fat_violence_ag_civ_dum = fatalities_violence_against_civilians_dum,
    fat_explosions_dum = fatalities_explosions_remote_violence_dum,
    fat_strategic_dev_dum = fatalities_strategic_developments_dum
  ) %>% 
  rename_with(~ substr(.x, 1, 32))          # truncate names to 32 chars

rm(centroids, varying_file_list, constants_file_list)
gc()

# get bird watchers from all non african countries
bird_o_d_y <- readRDS(here(int_dir, "bird origins", "bird_by_o_d_y.rds")) 

non_ssa_ctries <- wbstats::wb_cachelist$countries %>%
  filter(region != "Sub-Saharan Africa", region != "Aggregates") %>%
  unique() %>% 
  pull(iso3c)

bird_o_d_y <- bird_o_d_y %>% 
  filter(year < 2021, year > 1996)

gc()

bird_watchers_ag <- bird_o_d_y %>% 
  mutate(
    non_ssa = ifelse(iso3c_o %in% non_ssa_ctries, bird_watchers, 0),
    usa = ifelse(iso3c_o == "USA", bird_watchers, 0),
    non_usa_ssa = ifelse(iso3c_o %in% non_ssa_ctries & iso3c_o != "USA", bird_watchers, 0)
    ) %>% 
  group_by(year, gid) %>% 
  reframe(
    bird_watchers = sum(bird_watchers, na.rm = T),
    bird_watchers_non_ssa = sum(non_ssa, na.rm = T),
    bird_watchers_usa = sum(usa, na.rm = T),
    bird_watchers_non_usa_ssa = sum(non_usa_ssa, na.rm = T)
    )

grid_detailed <- grid_detailed %>% 
  left_join(bird_watchers_ag, by = c("gid", "year")) 

rm(bird_o_d_y, bird_watchers_ag)
gc()

fwrite(grid_detailed, here(cln_dir, "data_unlabeled.csv"), append = FALSE)
