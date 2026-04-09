source("code/SSA_env_SetUp.R")

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  filter(country == "MDG") 

mdg_sf <- read_sf(
  here(raw.dir, "Countries", "MDG", "Shapefiles", "pre_cleaning",
       "geoBoundaries-MDG-ADM2.geojson")) %>% 
  select(
    district_name = shapeName
  )

ant_main <- mdg_sf %>% 
  filter(
    grepl("Arrondissement", district_name)
  ) %>% 
  summarise(
    district_name = "Antananarivo Renivohitra",
    geometry = st_union(geometry),
    .groups = "drop"
  )

mdg_sf <- mdg_sf %>% 
  filter(
    !grepl("Arrondissement", district_name)
  ) %>% 
  bind_rows(ant_main) %>% 
  mutate(
    district_name = case_when(
      district_name == "Toliary-I" ~ "Toliary I",
      district_name == "Toliary-II" ~ "Toliary Ii",
      district_name == "Antsiranana II" ~ "Antsiranana Ii",
      district_name == "Antsirabe II" ~ "Antsirabe Ii",
      district_name == "Toamasina II" ~ "Toamasina Ii",
      district_name == "Mahajanga II" ~ "Mahajanga Ii",
      TRUE ~ district_name
    )
  ) %>% 
  left_join(id_df %>% select(district_name, ipums_id), by = "district_name") %>% 
  mutate(
    CNTRY_NAME = "Madagascar",
    ADMIN_NAME = district_name,
    CNTRY_CODE = "450",
    GEOLEVEL2 = as.character(ipums_id),
    PARENT = str_sub(GEOLEVEL2, 1, 6)
  ) %>% 
  select(
    CNTRY_NAME, ADMIN_NAME, CNTRY_CODE, 
    GEOLEVEL2, PARENT, geometry
  )
  
write_sf(mdg_sf, here(
  raw.dir, "Countries", "MDG", "Shapefiles", "level 2", 
  "MDG_adm2.shp")
  )
