#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   3 - Get number of tourists by country from WB
#* Note:    TBF  
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

# read grid with countries
ctries_grid <- fread(here(int_dir, "country_climatezones.csv")) %>% 
  select(gid, country, iso_a3) %>% 
  crossing(year = years) # get one observations for each year

# read data ----

# wb number of tourists
tourism <- fread(here(raw_dir, "tourism/tourism.csv"))
population <- fread(here(raw_dir, "tourism/population.csv"))

clean_wb_data <- function(data) {
  data %>%
    slice(1:(n()-5)) %>%
    `colnames<-`(sub(".*?(\\d{4}).*", "\\1", colnames(.))) %>%
    select(matches("^(19[9][6-9]|20\\d{2})|^[^0-9]")) %>%
    mutate(across(everything(), ~ifelse(. == "..", NA, .))) %>%
    select(-'Series Code') %>%
    rename(iso_a3 = 'Country Code', country = 'Country Name') %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "year",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = `Series Name`,
      values_from = value
    ) %>%
    mutate(
      year = as.integer(year),
    ) %>% 
    select(-country)
}

# clean datasets
population_clean <- clean_wb_data(population)
tourism_clean <- clean_wb_data(tourism)

rm(tourism, population)

tourism <- tourism_clean %>% 
  filter(year < 2021) %>% # no data
  left_join(population_clean, by = c("year", "iso_a3")) %>% 
  rename(
    arrivals = `International tourism, number of arrivals`,
    receipts = `International tourism, receipts (current US$)`,
    country_population = `Population, total`
  ) %>%
  mutate(
    # initally convert to thousands to enable changing it to numeric
    arrivals = as.integer(as.numeric(arrivals)/1000),  
    receipts = as.integer(as.numeric(receipts)/1000),  
    country_population = as.integer(as.numeric(country_population)/1000),  
    arrivals_pc = arrivals / country_population,
    receipts_pc = receipts / country_population,
    arrivals = arrivals / 1000,
    receipts = receipts / 1000
  ) %>% 
  select(iso_a3, year, arrivals, receipts, arrivals_pc, receipts_pc)

baseline_tourism <- tourism %>%
  filter(year == 1996) %>%
  select(iso_a3, arrivals, arrivals_pc, receipts, receipts_pc) %>%
  rename(
    arrivals_baseline = arrivals,
    arrivals_pc_baseline = arrivals_pc,
    receipts_baseline = receipts,
    receipts_pc_baseline = receipts_pc
  )

ctries_grid_tourism <- ctries_grid %>%
  left_join(tourism, by = c("iso_a3", "year")) %>% 
  left_join(baseline_tourism, by = "iso_a3") %>% 
  select(-c(country, iso_a3))

write_csv(ctries_grid_tourism, file.path(int_dir, "tourism.csv"), append = FALSE)
