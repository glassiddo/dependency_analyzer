#* Project: Migration Africa
#* Author:  Iddo Glass (editor, initially by Kenneth (?) in stata)
#* Date:    January 22, 2025, converted from older stata code
#* Title:   Create data of builtup area and volume 1975-2020 from GHSL
#* Desc:    
#*******************************************************************************

source("code/SSA_env_SetUp.R")

sample_ctries <- census_info %>% pull(country)
# IMPORTING GHSL BUILT-UP AREA DATA FROM GEE
ghsl_area <- read_csv(here(
  raw.dir, "AFRICA", "GHSL", "sampleUnitsGHSLBUArea.csv"
  ))
ghsl_volume <- read_csv(here(
  raw.dir, "AFRICA", "GHSL", "sampleUnitsGHSLBUVolume.csv"
  ))

### create row rows
world_area <- read_csv(here(
  raw.dir, "WORLD", "GEE", "worldUnitsGHSLBUArea.csv"
  ))

world_volume <- read_csv(here(
  raw.dir, "WORLD", "GEE", "worldUnitsGHSLBUVolume.csv"
  ))

make_row_summary <- function(df, sample_ctries) {
  df %>%
    filter(!ISO3 %in% sample_ctries) %>% 
    summarise(
      areaha = sum(areaha),
      across(
        matches("(1975|1980|1985|1990|1995|2000|2005|2010|2015|2020)"),
        sum
      )
    ) %>%
    mutate(
      country = "Rest of the World",
      admin_name = "ROW",
      ipums_id = "1"
    )
}

row_area   <- make_row_summary(world_area, sample_ctries)
row_volume <- make_row_summary(world_volume, sample_ctries)
ghsl_area_full <- bind_rows(ghsl_area, row_area)
ghsl_volume_full <- bind_rows(ghsl_volume, row_volume)

# COMBINING GHSL BUILT UP AREA AND VOLUME DATA INTO A SINGLE FILE
builtup <- ghsl_volume_full %>%
  left_join(
    ghsl_area_full %>% select(ipums_id, starts_with("bua")),
    by = "ipums_id"
  ) %>% 
  pivot_longer(
    cols = matches("(buatotal|buanres|buvtotal|buvnres)\\d{4}"),
    names_to = c(".value", "year"),
    names_pattern = "(buatotal|buanres|buvtotal|buvnres)(\\d{4})"
    ) 

saveRDS(builtup, here(build.dir, "AFRICA", "GHSL", "builtup_cleaned.rds"))