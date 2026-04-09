#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   Get data from iNaturalist - can be used for animals and tourists
#* Note:    A bit slow
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

threshold_for_views_animal <- 3
threshold_for_months <- 3
threshold_of_views <- 3


grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid)

grid_expanded_years <- fread(here(int_dir, "grid_years.csv")) %>% 
  select(gid, year)

animals <- 
  fread(
    here(raw_dir, "ebird and inaturalist", "inaturalist", "0004907-250227182400228.csv")
  ) %>% 
  filter(
    !is.na(decimalLatitude), 
    (
      !is.na(eventDate) & !is.na(year) 
      # check after that at other time vars to make sure
    ),
    year > 1990,
    !basisOfRecord %in% c("FOSSIL_SPECIMEN") # not interested in fossils
  ) 

animals_flt <- animals %>% 
  filter(
    family %in% c(
      "Bovidae", # buffalos
      "Equidae", # zebras
      "Giraffidae", # giraffes
      "Hippopotamidae", # hippos
      "Elephantidae", # elephants
      "Felidae", # cats - lions, leopards 
      "Rhinocerotidae" # rhinos
      ),
    genus %in% c(
      "Equus", # zebras
      "Syncerus", # african buffalos
      #"Bubalus", # water buffalo
      "Giraffa", # giraffes
      "Ceratotherium", # white rhinos
      "Diceros", # black rhinos
      "Panthera", # lions, leopards etc
      "Acinonyx", # cheetas
      "Giraffa", # giraffe
      "Hippopotamus", # hippos
      "Loxodonta" # african elephant
    ),
    !species %in% c( # most are relevant, so excluding rather than including
      "Equus asinus", # donkey
      "Equus caballus", # horse
      "Panthera tigris" # tiger, doesnt exist in the wild in africa so if it exists it's a mistake
     )
  ) %>% 
  mutate(
    category = case_when(
      genus == "Syncerus" ~ "Buffalo",
      genus == "Equus" ~ "Zebra", 
      genus == "Giraffa" ~ "Giraffe",
      (genus == "Diceros" | genus == "Ceratotherium") ~ "Rhino",
      genus == "Hippopotamus" ~ "Hippo",
      genus == "Loxodonta" ~ "Elephant", 
      genus == "Acinonyx" ~ "Cheeta",
      species == "Panthera leo" ~ "Lion",
      species == "Panthera pardus" ~ "Leopard",
      TRUE ~ "Other"
    )
  )

animals_flt_sf <- animals_flt %>% 
  st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"), crs = 4326
  ) %>% 
  st_transform(crs = st_crs(afr_crs))

rm(animals, animals_flt)

grid_for_animals_key <- st_join(animals_flt_sf, grid, join = st_within)

### filter out observations in cities (might be zoos) ----
my_geodata_path <- here(int_dir, "distances to cities")
travel_time_1mcity <- travel_time(to="city", size=3, up=TRUE, path=my_geodata_path)

pts_vect <- vect(st_geometry(animals_flt_sf))
extracted_values <- extract(travel_time_1mcity, pts_vect)

grid_for_animals_key$distance_to_min1m_city <- extracted_values[,2] 

animals_flt_no_cities <- grid_for_animals_key %>% filter(
  is.na(coordinateUncertaintyInMeters) |
    (distance_to_min1m_city > 10 & 
       (coordinateUncertaintyInMeters < 10000 | 
          is.na(coordinateUncertaintyInMeters))) |
    (distance_to_min1m_city > 30 & 
       (coordinateUncertaintyInMeters < 30000 | 
          is.na(coordinateUncertaintyInMeters)))
) 

animals_by_year <- animals_flt_no_cities %>%
  st_drop_geometry() %>% 
  group_by(gid, year, category) %>%
  summarise(n_animal = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = category, 
    values_from = n_animal, 
    values_fill = 0
  ) 

grid_with_animals <- grid_expanded_years %>% 
  left_join(animals_by_year, by = c("gid", "year")) %>% 
  select(-Other) %>% 
  mutate(across(where(is.numeric) & !c("year", "gid"), ~replace_na(., 0))) 

animal_exists <- grid_with_animals %>% 
  group_by(gid) %>% 
  summarise(across(where(is.numeric) & !matches("year"), ~sum(.))) %>% 
  mutate(across(where(is.numeric) & !matches("gid"), 
                ~as.integer(. > threshold_for_views_animal)))

### tourists count - filter out park workers / universities
common_observers <- grid_for_animals_key %>% 
  st_drop_geometry() %>% 
  filter(identifiedBy != "", !is.na(gid)) %>% 
  group_by(month, year, identifiedBy, gid) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(year, identifiedBy, gid) %>%  # same but without month
  summarise(num_months = sum(n >= threshold_of_views), .groups = "drop") %>%  # Count months with at least x observations
  filter(num_months >= threshold_for_months) %>% 
  distinct(identifiedBy)

tourists_locations <- grid_for_animals_key %>% 
  filter(!identifiedBy %in% common_observers$identifiedBy) %>%
  st_drop_geometry() %>% 
  group_by(gid, year) %>% 
  summarise(n_distinct_viewers_year = n_distinct(identifiedBy), .groups = "drop") %>% 
  filter(!is.na(gid))

grid_tourists <- grid_expanded_years %>% 
  left_join(tourists_locations, by = c("gid", "year")) %>% 
  mutate(n_distinct_viewers_year = replace_na(n_distinct_viewers_year, 0))

grid_for_export <- grid_tourists %>% 
  left_join(animal_exists, by = "gid") %>% 
  rename_with(~ paste0(., "_inat"), .cols = where(is.numeric) & 
                !c("year", "gid")) %>% 
  mutate(
    num_big_game_inat = Leopard_inat + Lion_inat + Elephant_inat + Rhino_inat + Buffalo_inat
    )

write_csv(grid_for_export, here(int_dir, "inaturalist.csv"), append = FALSE)
