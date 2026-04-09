#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    Sep 19, 2023
#* Title:   Clean Mine data
#* Desc:    Read african mines data and clean
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

#*******************************************************************************
# read data ----
#*******************************************************************************
sample_sf <- st_read(here(build.dir, "Africa", "Maps", "SampleUnits.shp")) %>% 
  select(ipums_id, country)

## Mines ----
mines_df <- read_xls(
  paste0(raw.dir, "/Africa/Mines/SPGlobal_Export_9-18-2023", 
         "_a77f020c-5d4f-49cc-b405-4fb80f56d545.xls"),
  col_types = "text",
  skip = 4) %>% 
  rename_all(~tolower(.x)) %>%
  rename(production_tonnes = commodity_production_tonne_by_period) %>%
  drop_na(prop_id) %>%
  mutate(across(c(prop_name:actual_closure_yr), ~ifelse(.x == "NA", NA, .x))) %>%
  mutate(across(c(latitude, longitude, contains("yr")), ~as.numeric(.x))) %>%
  drop_na(latitude) %>% 
  mutate(
    # chromium and ferrochrome is chromite
    primary_commodity = ifelse(primary_commodity %in% c("Chromium", "Ferrochrome"),
                               "Chromite", primary_commodity),
    # titanium and rutile are ilmenite
    primary_commodity = ifelse(primary_commodity %in% c("Titanium","Rutile"), 
                               "Ilmenite", primary_commodity),
    # palladium is classed as platinum group
    primary_commodity = ifelse(primary_commodity == "Palladium", 
                               "Platinum", primary_commodity),
    # Niobium always occurs with tantalum. Price series ends in 2000
    primary_commodity = ifelse(primary_commodity == "Niobium", 
                               "Tantalum", primary_commodity),
    commodity = ifelse(
      primary_commodity %in% c("Heavy Mineral Sands","Molybdenum", "Niobium",
                               "Silver", "Tungsten", "Zircon","Vanadium", 
                               "Tantalum"),
      "other", primary_commodity)) %>%
  select(-c(locale,full_work_history, production_tonnes, est_mine_life_yr,
            study_yr, event_yr, country_name))

mines_sf <- st_as_sf(mines_df, coords = c("longitude","latitude"),
                     crs = 4326, agr = "constant") 

#*******************************************************************************
# Match ----
#*******************************************************************************

sf_use_s2(FALSE)

sample_mines <- mines_sf %>% 
  st_join(sample_sf, join = st_within) %>%
  st_drop_geometry() %>%
  drop_na(ipums_id) 

saveRDS(mines_sf, here(build.dir, "Africa", "Mines", "mines_sf.rds"))

saveRDS(sample_mines, here(build.dir, "Africa", "Mines", "sample_mines.rds"))

# commodity count
commod_df <- sample_mines %>% as.data.frame() %>%
  mutate(N = 1) %>%
  group_by(primary_commodity) %>%
  summarise(mine_count = sum(N))
  
saveRDS(commod_df, here(build.dir, "Africa", "Mines", "commodity_counts.rds"))
