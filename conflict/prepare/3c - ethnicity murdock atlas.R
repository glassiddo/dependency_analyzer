#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   Get data about ethnic groups from the Murdoch atlas
#* Note:    Also uses the main activity of subsistance
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

grid <- st_read(here(int_dir, "grid.gpkg"))
pastor <- fread(here(int_dir, "grid_with_pastor_data.csv"))

grid_pastor <- grid %>% 
  left_join(pastor %>% 
              filter(year == "2010"), # could be another year as well
            by = "gid"
  ) %>% 
  select(gid, ethnic, geom)

### using murdock package ----
# devtools::install_github("sboysel/murdock")
library(murdock)
murdock <- st_as_sf(murdock::murdock) %>% 
  st_transform(st_crs(afr_crs))
murdock_nogeom <- st_set_geometry(murdock, NULL)

# Function to clean and process variables
process_variables <- function(df, suffix = "") {
  df %>%
    rename(
      !!paste0("gathering", suffix) := V1,
      !!paste0("hunting", suffix) := V2,
      !!paste0("fishing", suffix) := V3,
      !!paste0("animal_husbandry", suffix) := V4,
      !!paste0("agriculture", suffix) := V5
    ) %>%
    mutate(
      across(
        c(starts_with("gathering"), starts_with("hunting"), 
          starts_with("fishing"), starts_with("animal_husbandry"), 
          starts_with("agriculture")),
        ~case_when(
          gsub(' Dependence', '', .) == "0-5%" ~ 0,
          gsub(' Dependence', '', .) == "6-15%" ~ 1,
          gsub(' Dependence', '', .) == "16-25%" ~ 2,
          gsub(' Dependence', '', .) == "26-35%" ~ 3,
          gsub(' Dependence', '', .) == "36-45%" ~ 4,
          gsub(' Dependence', '', .) == "46-55%" ~ 5,
          gsub(' Dependence', '', .) == "56-65%" ~ 6,
          gsub(' Dependence', '', .) == "66-75%" ~ 7,
          gsub(' Dependence', '', .) == "76-85%" ~ 8,
          gsub(' Dependence', '', .) == "86-100%" ~ 9,
          TRUE ~ NA_real_
        )
      )
    )
}

m1 <- grid_pastor %>%  
  mutate(NAME = gsub('"', '', ethnic)) %>% 
  select(gid, NAME)

# Geographical join
m1_geomjoin <- m1 %>% 
  st_join(murdock, join = st_intersects, left = T, largest = T) %>%
  process_variables("_geom") %>%
  select(gid, ends_with("_geom")) %>% 
  st_drop_geometry()

# Text join
m1_textjoin <- m1 %>% 
  st_drop_geometry() %>% 
  left_join(murdock_nogeom, by = "NAME") %>% 
  process_variables("_text") %>%
  select(gid, ends_with("_text"))

# merge
m1_mixed <- m1_geomjoin %>%
  left_join(m1_textjoin, by = "gid") 

murdock_keys <- murdock %>% 
  select(CODE, NAME) %>% 
  st_drop_geometry() %>% 
  rename(id = CODE)

### using dominant activitiy downloaded -----
dominant_subsistance_activity <- fread(here(raw_dir, "subsistance.csv")) %>% 
  select(id, name) %>% 
  mutate(id = gsub('EA042-', '', id)) %>% 
  mutate(id = gsub('-1', '', id)) %>% 
  left_join(murdock_keys, by = "id") %>% 
  rename(ethnic = NAME,
         activity = name) %>% 
  select(-id) %>% 
  drop_na()

agric <- c("Extensive agriculture", "Agriculture, type unknown", "Intensive agriculture")
m2 <- grid_pastor %>% 
  st_drop_geometry() %>% 
  select(gid, ethnic) %>% 
  left_join(dominant_subsistance_activity, by = "ethnic") %>%
  select(gid, ethnic, activity) %>% 
  mutate(activity = sub("\\s*\\(.*", "", activity)) %>% 
  mutate(activity = ifelse(activity %in% agric, "Agriculture", activity)) %>% 
  select(-ethnic)

data_subsistance <- m1_mixed %>% 
  left_join(m2, by = "gid")

write_csv(data_subsistance, 
          file.path(int_dir, "subsistance.csv"), 
          append = FALSE)

