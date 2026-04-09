# Project: Migration Africa
# Author:  Sam Marshall
# Created: Jan 20, 2022
# Title:   Benin Census Populations
# Output:  population.rds
###############################################################################

source("code/SSA_env_SetUp.R")
library(stringi)

###############################################################################
##### 1. District
###############################################################################

pop_df <- read_excel(paste0(ben_raw.dir, "Census_populations.xlsx"),
                     range = "A1:F91") %>%
  filter(Status == "Commune") %>%
  mutate(Name = stri_trans_general(str = Name, id = "Latin-ASCII"),
         Name = str_to_sentence(Name),
         Name = ifelse(Name == "Adjarra", "Adjara", Name),
         Name = ifelse(Name == "Aguegues", "Aguegue", Name),
         Name = ifelse(Name == "Dassa-zoume", "Dassa", Name),
         Name = ifelse(Name == "Djakotomey", "Djakotome", Name),
         Name = ifelse(Name == "Dogbo", "Dogbo-tota", Name),
         Name = ifelse(Name == "N'dali", "Ndali", Name),
         Name = ifelse(Name == "Pehonko (pehunco)", "Pehonko", Name),
         Name = ifelse(Name == "Toucountouna", "Toukountouna", Name),
         Name = ifelse(Name == "Zangnanado (zagnanado)", "Zangnanado", Name),
         Name = ifelse(Name == "Zogbodomey", "Zogbodome", Name),
         Name = ifelse(Name == "Cobly", "Kobli", Name),
         Name = ifelse(Name == "Copargo", "Kopargo", Name)) 

names(pop_df) <- c("district_name", "type", "pop_79", "pop_92","pop_02", "pop_13")

dist_id_df <- readRDS( paste0(ben.dir, "Forest Cover/loss_5yr.rds")) %>%
  select(district_name, region_name, district) %>%
  unique() %>%
  full_join(pop_df) %>%
  arrange(district_name) %>%
  select(-type, -pop_79)

urb13_df <- read_dta( paste0(ben_raw.dir, "migration_2013.dta" ) ) %>%
  mutate(pop = 1) %>%
  group_by(commune_now) %>%
  summarise(across(c(pop, urban), ~sum(.x)), .groups = 'drop') %>%
  mutate(urban = round(urban / pop)) %>%
  select(-pop)

urb02_df <- read_dta( paste0(ben_raw.dir, "migration_2002.dta" ) ) %>%
  mutate(pop = 1) %>%
  group_by(commune_now) %>%
  summarise(across(c(pop, urban), ~sum(.x)), .groups = 'drop') %>%
  mutate(urban_02 = round(urban / pop)) %>%
  select(-pop, -urban)

urban_df <- full_join(urb13_df, urb02_df) %>%
  rename(district = commune_now) 
  
dist_id_df <- full_join(dist_id_df, urban_df)

saveRDS( dist_id_df, paste0(ben.dir, "Support/population.rds"))

###############################################################################
##### 2. Region
###############################################################################

reg_pop_df <- read_excel(paste0(ben_raw.dir, "Census_populations.xlsx"),
                         range = "A1:F91") %>%
  filter(Status == "Department") %>%
  mutate(Name = stri_trans_general(str = Name, id = "Latin-ASCII"),
         Name = ifelse(Name == "Atacora", "Atakora", Name))

names(reg_pop_df) <- c("region_name", "type", "pop_79", "pop_92","pop_02", "pop_13")

reg_id_df <- readRDS( paste0(ben.dir, "Forest Cover/loss_5yr.rds")) %>%
  mutate(region = floor(district / 10)) %>%
  select(region_name, region) %>%
  unique() %>%
  full_join(reg_pop_df) %>%
  arrange(region_name) %>%
  select(-type, -pop_79)

saveRDS( reg_id_df, paste0(ben.dir, "Support/region_population.rds"))

