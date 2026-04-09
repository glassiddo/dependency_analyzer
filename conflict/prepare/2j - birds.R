#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   Get eBird data
#* Note:    This is sample data (from François), need to get full data
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

# overwrite it (from the source file) as we want -10 years as well
years <- seq(first_yr_of_analysis-10, last_yr_of_analysis)
year_combinations <- expand.grid(year = years)

grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid)
  
grid_expanded_years <- grid %>% 
  st_drop_geometry() %>% 
  select(gid) %>% 
  crossing(year = years) 

birds <- fread(here(
  raw_dir, "ebird and inaturalist/birddata/africa_sampledec24_byobslocday.csv")
  ) %>% 
  select(observationdate, country, longitude, latitude, observerid, year) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = st_crs(afr_crs))

grid_for_birds_key <- st_join(birds, grid, join = st_within) %>% 
  st_drop_geometry() %>% 
  mutate(
    date = as.Date(observationdate),
    week = week(date),
    month = month(date),
    year = year(date),
    week_year = paste(week,'_',year),
    month_year = paste(month,'_',year)
  )

na_vals <- grid_for_birds_key %>% filter(is.na(gid)) # seems to be mostly islands

# count number of distinct bird viewers in period x
avg_distinct_birds <- function(data, time_unit) {
  data %>%
    group_by(gid, !!sym(time_unit), year) %>%
    summarise(n_obs = n_distinct(observerid), .groups = "drop") %>%
    group_by(gid, year) %>%
    summarise(
      !!paste0("avg_distinct_birds_", time_unit) := mean(n_obs), 
      .groups = "drop"
    )
}

birds_by_month <- avg_distinct_birds(grid_for_birds_key, "month")
birds_by_week  <- avg_distinct_birds(grid_for_birds_key, "week")
birds_by_date  <- avg_distinct_birds(grid_for_birds_key, "date")

### join 
grid_for_birds_for_join <- reduce(
  list(birds_by_date, birds_by_week, birds_by_month),
  left_join,
  by = c("gid", "year")
)

# join all
grid_for_birds_for_join <- birds_by_date %>%
  left_join(birds_by_week, by = c("gid", "year")) %>%
  left_join(birds_by_month, by = c("gid", "year"))

grid_bird_expanded_years <- grid_expanded_years %>% 
  left_join(grid_for_birds_for_join, by = c("gid", "year")) %>% 
  mutate(across(starts_with("avg_distinct_birds_"), replace_na, 0)) %>% 
  # compute lags
  group_by(gid) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    across(
      starts_with("avg_distinct_birds_"),
      list(
        lag1 = ~lag(.x, 1),
        lag2 = ~lag(.x, 2),
        lag3 = ~lag(.x, 3)
      ),
      .names = "{.col}_{.fn}"
    ),
    avg_distinct_birds_month_lag5  = lag(avg_distinct_birds_month, 5),
    avg_distinct_birds_month_lag10 = lag(avg_distinct_birds_month, 10),
    # rolling averages using slider::
    avg_distinct_birds_month_roll_2yr   = slide_dbl(avg_distinct_birds_month, mean, .before = 2,  .after = -1, .complete = FALSE),
    avg_distinct_birds_month_roll_5yr   = slide_dbl(avg_distinct_birds_month, mean, .before = 5,  .after = -1, .complete = FALSE),
    avg_distinct_birds_month_roll_10yr  = slide_dbl(avg_distinct_birds_month, mean, .before = 10, .after = -1, .complete = FALSE),
    avg_distinct_birds_month_roll_10_5yr= slide_dbl(avg_distinct_birds_month, mean, .before = 10, .after = -6, .complete = FALSE)
  ) %>%
  ungroup() %>% 
  filter(year > 1996, year < 2024) # for export

write_csv(grid_bird_expanded_years, 
          file.path(int_dir, "birds.csv"), append = FALSE)
