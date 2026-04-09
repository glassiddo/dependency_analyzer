#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   3 - set cells to countries, colonial origins and climate zones
#* Note:    TBF  
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

#### read grid ----
grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid)

#### KÖPPEN-GEIGER climate zone  ------
# downloaded from https://koeppen-geiger.vu-wien.ac.at/present.htm
kpg <- read.table(here(raw_dir, "kpg/Koeppen-Geiger-ASCII.txt"), header = TRUE)
kpg <- st_as_sf(kpg, coords = c("Lon", "Lat"), crs = 4326) %>% 
  st_transform(st_crs(afr_crs)) %>% 
  rename(cls = Cls)
merg <- st_join(grid, kpg, join = st_nearest_feature) 

#### pick country for each cell -----
africa <- ne_countries(
  continent = "Africa", scale = "medium", returnclass = "sf"
  ) %>%
  st_transform(st_crs(afr_crs)) %>% 
  select(name, geometry, iso_a3) %>% 
  rename(country = name) %>% 
  mutate( # Only use UN recognized countries
    iso_a3 = case_when(
      country == "South Sudan" ~ "SSD", # iso_a3 code for South Sudna
      country == "Somaliland" ~ "SOM",   # Classify Somaliland as Somalia
      country == "W. Sahara" ~ "MAR", # Classify Western Sahara as Morocco
      TRUE ~ iso_a3  # Keep original value otherwise
    ),
    country = case_when(
      country == "Somaliland" ~ "Somalia",   # Classify Somaliland to Somalia
      country == "W. Sahara" ~ "Morocco", # Classify Western Sahara to Morocco
      TRUE ~ country  # Keep original value otherwise
    )
  )

#### colonial origin ----
colonial <- read_dta(here(raw_dir, "colonial/qogdata_30_08_2024.dta")) %>% 
  filter(year == 2000) %>% 
  rename(country = cname,
         iso_a3 = ccodealp) %>% 
  mutate(colonial_origin = case_when(
    ht_colonial == 0 ~ "Never colonized by a Western overseas colonial power",
    ht_colonial == 2 ~ "Spanish",
    ht_colonial == 3 ~ "Italian",
    ht_colonial == 5 ~ "British",
    ht_colonial == 6 ~ "French",
    ht_colonial == 7 ~ "Portuguese",
    ht_colonial == 8 ~ "Belgian",
    TRUE ~ NA_character_  # to handle any other values not listed
  )) %>% 
  select(
    iso_a3, colonial_origin
  ) 

add_south_sudan_origin <- data.frame( 
# https://exploringafrica.matrix.msu.edu/colonial-exploration-and-conquest-in-africa-explore/
  iso_a3 = c("SSD"),
  colonial_origin = c("British") # was part of sudan
)

# Combine the original data with the new rows
colonial <- colonial %>%
  bind_rows(add_south_sudan_origin)

africa <- africa %>% 
  left_join(colonial, by = "iso_a3")

# Perform spatial join
result <- st_join(merg, africa, join = st_intersects, largest = TRUE)

cls_country_df <- result %>%
  st_drop_geometry() %>%
  select(gid, cls, country, iso_a3, colonial_origin)

#### ranger data ----
## need to run the following if not already installed
## from https://github.com/courtiol/rangeRinPA

# install.packages("remotes")
# remotes::install_github("courtiol/rangeRinPA", dependencies = TRUE, build_vignettes = TRUE)
library(rangeRinPA)
ranger_data <- data_rangers %>% 
  dplyr::select(
    countryname_iso, countryname_eng, data_year_info, country_UN_continent,
    #staff_rangers_others_known, staff_others_rangers_known, staff_rangers_others_unknown, staff_total_details_unknown,
    staff_total, staff_rangers, staff_others, PA_area_surveyed, PA_area_unsurveyed,
    area_PA_WDPA, reliability, notes, sampled_coverage
  ) %>% 
  dplyr::filter(country_UN_continent == "Africa",
                #!is.na(staff_total)
  ) %>% 
  dplyr::mutate(
    share_ranger = staff_rangers / staff_total,
    ranger_per_area = staff_rangers / PA_area_surveyed
  ) 

ranger_for_join <- ranger_data %>% 
  select(
    countryname_iso, share_ranger, ranger_per_area
  ) %>% 
  rename(
    iso_a3 = countryname_iso
  ) %>% 
  mutate(
    share_ranger = ifelse(share_ranger == "NaN", NA, share_ranger),
    ranger_per_area = ifelse(ranger_per_area == "NaN", NA, ranger_per_area)
  )

cls_country_ranger_df <- cls_country_df %>% 
  left_join(ranger_for_join, by = "iso_a3")

write_csv(cls_country_ranger_df, 
         file.path(int_dir, "country_climatezones.csv"), 
         append = FALSE)
