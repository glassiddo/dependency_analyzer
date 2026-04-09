#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   3 - Process ACLED data 
#* Note:    Processes ACLED data, and matches it to grid cells and years          
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

acled <- read_csv(here(raw_dir, "acled/ACLED Africa_1997-2024_Apr26.csv"))
acled <- acled[!(duplicated(acled$event_id_cnty)), ] # remove duplicates

#### Merge with grid and create summary stats -----
# create spatial object based on coordinates, for now using EPSG:4326
grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid)

acled_sf <- st_as_sf(acled, coords = c("longitude", "latitude"), crs = 4326) %>% 
  # initially read at 4326 cause thats how the data is initally given
  st_transform(crs = afr_crs)

# match each row in acled data to a grid cell based on nearest geography
acled_joined <- st_join(acled_sf, grid, join = st_nearest_feature) %>% 
  st_drop_geometry() 

jihadist_groups <- c(
  "Islamic State", "IS", "Al-Qaeda in the Islamic Maghreb", "AQIM", 
  "Movement for Oneness and Jihad in West Africa", "MUJAO", 
  "Benghazi Revolutionaries Shura Council", "Ansar Dine", 
  "Ansaroul Islam", "Mujahideen", "Signed-in-Blood Battalion", 
  "Ansar al-Sharia in Libya", "ASL", "al-Murabitun", 
  "Macina Liberation Front", "FLM", "Jama’at Nasr al-Islam wal Muslimin", 
  "JNIM", "JNIM: Group for Support of Islam and Muslims",
  "Ansar al-Sunnah", "Derna Protection Force", "DPF", 
  "Al-Shabaab", "Shabaab", "Al Qaida", "Qaeda", "Qaida"
  )

acled_joined$is_jihadist <- as.integer(
  grepl("jihad", acled_joined$actor1, ignore.case = TRUE) |
    grepl("jihad", acled_joined$actor2, ignore.case = TRUE) |
    rowSums(sapply(jihadist_groups, function(group) 
      grepl(group, acled_joined$actor1, ignore.case = FALSE) |
        grepl(group, acled_joined$actor2, ignore.case = FALSE)
    )) > 0
) # presumably imperfect as not case sensitive but should be ok enough for now

# summarize acled data based on grid cell and year
acled_filtered <- acled_joined %>% 
  filter(
    event_date <= as.Date(paste0(last_yr_of_analysis, "-12-31")), 
  ) %>% 
  mutate(
    state_forces_involved = ifelse(grepl("1", interaction), 1, 0),
    type_state = ifelse(
      state_forces_involved == 1, paste0("inv_state"), paste0("non_state")),
    type_jihad = ifelse(
      is_jihadist == 1, paste0("inv_jihad"), paste0("non_jihad")),
    is_battle_or_violence = ifelse(
      event_type %in% c("Violence against civilians", "Battles"),
      1, 0
    )
    # following mcguirk + nunn -> 
    # exclude protests, riots, explosions/remote violence, and strategic developments
  ) 

acled_summary <- acled_filtered %>% 
  filter(is_battle_or_violence == 1) %>% 
  group_by(
    gid, year, type_state
  ) %>% 
  summarise(
    fatalities  = sum(fatalities),
    events = n(),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = type_state,
    values_from = c(fatalities, events),
    values_fill = list(fatalities = 0, events = 0)
  ) %>% 
  mutate(
    total_fatalities = rowSums(select(., starts_with("fatalities"))),
    total_events = rowSums(select(., starts_with("events")))
  )

acled_summary_jihad <- acled_filtered %>% 
  filter(is_battle_or_violence == 1) %>% 
  group_by(
    gid, year, type_jihad
  ) %>% 
  summarise(
    fatalities  = sum(fatalities),
    events = n(),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = type_jihad,
    values_from = c(fatalities, events),
    values_fill = list(fatalities = 0, events = 0)
  )

acled_summary_event_type <- acled_filtered %>% 
  group_by(
    gid, year, event_type
  ) %>% 
  summarise(
    fatalities  = sum(fatalities),
    events = n(),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = event_type,
    values_from = c(fatalities, events),
    values_fill = list(fatalities = 0, events = 0)
  )

acled_summary <- acled_summary %>% 
  left_join(acled_summary_jihad, by = c("gid", "year")) %>% 
  left_join(acled_summary_event_type, by = c("gid", "year")) 

# # take the summarized data and join it to the expanded grid
acled_summary_for_export <- fread(here(int_dir, "grid_years.csv")) %>% 
  select(gid, year) %>% 
  left_join(acled_summary, by = c("gid", "year")) %>% 
  replace(is.na(.), 0) 

write_csv(acled_summary_for_export, file.path(int_dir, "acled_summary.csv"))