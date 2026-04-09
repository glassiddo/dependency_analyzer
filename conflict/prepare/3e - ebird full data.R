setwd("C:/Users/iddo2/Nextcloud2/conflict and protected areas")
source("do/conf_biodiversity_setup.R")
library(duckdb)

grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid)

watchers_af <- readRDS(here(
  int_dir, "bird origins", "origin_africa.rds"
))

watchers_oc <- readRDS(here(
  int_dir, "bird origins", "origin_oc.rds"
))

watchers_na <- readRDS(here(
  int_dir, "bird origins", "origin_northamerica.rds"
))

watchers_us <- readRDS(here(
  int_dir, "bird origins", "origin_us_novdec.rds"  
)) %>% 
  mutate(
    # original data is just november december
    # so (prob. unreasonable) assumption that 
    # the number of unique months/days could be * by 6
    unique_months = unique_months * 6,
    unique_days = unique_days * 6
  )

watchers_eu <- readRDS(here(
  int_dir, "bird origins", "origin_europe.rds"
))

watchers_sa <- readRDS(here(
  int_dir, "bird origins", "origin_southamerica.rds"
))

watchers_as <- readRDS(here(
  int_dir, "bird origins", "origin_asia.rds"
))

all_watchers <- bind_rows(
  watchers_eu, watchers_oc, watchers_sa, watchers_as, 
  watchers_na, watchers_us, watchers_af
)

gc()
rm(watchers_af, watchers_eu, watchers_oc, watchers_sa, watchers_as, 
   watchers_us, watchers_na)

## detect the main country of residence of each bird watcher
library(dtplyr)
country_of_origin <- all_watchers %>% 
  lazy_dt() %>% 
  group_by(recordedBy) %>%
  slice_max(unique_months, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(recordedBy, estCountryOfOrigin, unique_months) %>% 
  as.data.frame()

## observe the top countries overall
top_ctries <- country_of_origin %>% 
  group_by(estCountryOfOrigin) %>%
  summarise(
    n = n_distinct(recordedBy)
  ) %>% 
  ungroup() %>% 
  arrange(desc(n))

### allocate to bird watchers in africa

### read the african bird data using duckdb
con <- dbConnect(duckdb::duckdb())

# Register the CSV with DuckDB
duckdb::duckdb_register(con, "country_of_origin", country_of_origin)

# Let DuckDB read and filter the CSV directly
birds_af <- dbGetQuery(con, "
  SELECT 
    b.gbifID, 
    b.eventDate, 
    b.day, 
    b.month, 
    b.year,
    COALESCE(b.countryCode, 'NA') as countryCode,
    b.decimalLongitude, 
    b.decimalLatitude, 
    b.recordedBy,
    c.estCountryOfOrigin
  FROM read_csv_auto(?) b
  LEFT JOIN country_of_origin c 
    ON b.recordedBy = c.recordedBy
  WHERE c.estCountryOfOrigin IS NOT NULL
", params = list(here(raw_dir, "ebird and inaturalist/birddata/ebird_afr.csv"))) %>% 
  as_tibble()

dbDisconnect(con, shutdown = TRUE)

#### merge with the african data
birds_af <- birds_af %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  st_transform(crs = st_crs(afr_crs))

rm(con, all_watchers)
## get the grid cell for each observation
grid_for_birds_key <- st_join(birds_af, grid, join = st_within, left = FALSE) %>% 
  st_drop_geometry() %>% 
  mutate(
    week = week(as.Date(eventDate)),
    week_year = paste(week,'_',year),
    month_year = paste(month,'_',year)
  )

### observe the countries of origin in the african data
leading_ctries <- birds_af %>%
  st_drop_geometry() %>%
  group_by(estCountryOfOrigin) %>%
  summarise(
    n = n_distinct(recordedBy)
  ) %>%
  ungroup() %>%
  arrange(desc(n))

print(leading_ctries, n = 25)
rm(birds_af)
# count number of distinct bird viewers in period x
avg_distinct_birds <- function(data, time_unit) {
  data %>%
    group_by(gid, !!sym(time_unit), year, estCountryOfOrigin) %>%
    summarise(n_obs = n_distinct(recordedBy), .groups = "drop") %>%
    group_by(gid, year, estCountryOfOrigin) %>%
    summarise(
      !!paste0("avg_distinct_birds_", time_unit) := mean(n_obs), 
      .groups = "drop"
    )
}

birds_by_month <- avg_distinct_birds(grid_for_birds_key, "month") %>% 
  mutate(
    iso3 = countrycode::countrycode(estCountryOfOrigin, "iso2c", "iso3c"),
  )

all_origins <- unique(birds_by_month$iso3)
all_grid_cells <- unique(grid$gid)
all_years <- unique(birds_by_month$year)

full_matrix_bird_o_d_y <- crossing(
  iso3 = all_origins, 
  gid = all_grid_cells,
  year = all_years
  ) %>% 
  full_join(birds_by_month, by = c("gid", "iso3", "year")) %>% 
  mutate(
    bird_watchers = replace_na(avg_distinct_birds_month, 0)
  ) %>% 
  select(iso3c_o = iso3, gid, year, bird_watchers)

saveRDS(full_matrix_bird_o_d_y, here(int_dir, "bird origins", "bird_by_o_d_y.rds"))

ctries <- wbstats::wb_cachelist$countries %>%
  filter(region != "Sub-Saharan Africa", region != "Aggregates") %>%
  unique() %>% 
  pull(iso3c)

full_matrix_bird_o_d_y <- full_matrix_bird_o_d_y %>% 
  filter(year < 2020, year > 1996, iso3c_o %in% ctries)

main_ctries <- full_matrix_bird_o_d_y %>% 
  group_by(iso3c_o) %>% 
  reframe(s = sum(bird_watchers)) %>% 
  arrange(desc(s)) %>% 
  slice_head(n = 30) %>% 
  pull(iso3c_o) 

full_matrix_bird_o_d_y <- full_matrix_bird_o_d_y %>% 
  filter(iso3c_o %in% main_ctries)

saveRDS(full_matrix_bird_o_d_y, here(int_dir, "bird origins", "bird_by_o_d_y_sample.rds"))
