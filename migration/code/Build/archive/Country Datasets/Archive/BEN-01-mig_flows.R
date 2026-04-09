# Project: Migration Africa
# Author:  Sam Marshall
# Created: Jan 20, 2022
# Title:   Benin Migration Panel
# Output:  BEN_5yr_flows.rds
# Notes:   2002 is the whole thing, 2013 is a 10% sample      
# commune_before codes: 
# 997 = international, 998 = I don't know, 999 = "not specified", NA is just missing
###############################################################################

source("code/SSA_env_SetUp.R")
library(stringi)

###############################################################################
##### 1. Functions
###############################################################################

build_5yr_inflow <- function(dat_df, yr_end) {
  
  # 5 year windows
  yr_start <- yr_end - 4
  out_df <- dat_df %>%
    mutate(migrant = ifelse(mig_year >= yr_start & mig_year <= yr_end, 1, 0)) %>%
    drop_na(district) %>%
    group_by(district) %>%
    summarise(immig_flow = sum(migrant),
              pop = mean(pop),
              .groups = "drop") %>%
    mutate(range = paste0(yr_start, "-", yr_end)) %>%
    # calculate migration rates for full/10% sample
    {if(dat_df$year[1] == 2013) mutate(., immig_flow = 10 * immig_flow ) else . } %>%
    mutate(immig_rate = immig_flow / pop)
}

build_5yr_outflow <- function(dat_df, yr_end) {
  
  # 5 year window
  yr_start <- yr_end - 4
  
  # get population of previous district
  dist_pop_df <- dat_df %>%
    select(district, pop) %>%
    unique()
  
  out_dt <- dat_df %>%
    mutate(migrant = ifelse(mig_year >= yr_start & mig_year <= yr_end, 1, 0)) %>%
    drop_na(prev_district) %>%
    group_by(prev_district) %>%
    summarise(emig_flow = sum(migrant),
              .groups = "drop") %>%
    mutate(range = paste0(yr_start, "-", yr_end)) %>%
    rename(district = prev_district) %>%
    left_join(dist_pop_df) %>%
    drop_na(pop) %>%
    # calculate migration rates for full/10% sample
    {if(dat_df$year[1] == 2013) mutate(., emig_flow = 10 * emig_flow ) else . } %>%
    mutate(emig_rate = emig_flow / pop)
}

###############################################################################
##### Census populations
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

dist_id_df <- readRDS( paste0(ben_forest.dir, "BEN_5yr_loss.rds")) %>%
  select(district_name, region_name, district) %>%
  unique() %>%
  full_join(pop_df) %>%
  arrange(district_name) %>%
  select(-type, -pop_79)


###############################################################################
##### 2. Read and clean raw data
###############################################################################

benin13_df <- read_dta( paste0(ben_raw.dir, "migration_2013.dta" )) %>%
  remove_all_labels() %>%
  rename(district = commune_now, prev_district = commune_before, 
         duration = moved_x_years_ago) %>%
  drop_na( duration ) %>%
  # make sure dist is the same for people who have lived there since birth
  mutate(prev_district = ifelse(duration == 98, district, prev_district),
         year = 2013,
         mig_year = year - duration) %>%
  left_join(dist_id_df) %>%
  select(-pop_92) %>%
  rename(pop = pop_02, curr_pop = pop_13)

benin02_df <- read_dta( paste0(ben_raw.dir, "migration_2002.dta" )) %>%
  remove_all_labels() %>%
  rename(district = commune_now, prev_district = commune_before, 
         duration = moved_x_years_ago) %>%
  drop_na( duration ) %>%
  # make sure dist is the same for people who have lived there since birth
  mutate(prev_district = ifelse(duration == 98, district, prev_district),
         year = 2002,
         mig_year = year - duration) %>%
  left_join(dist_id_df) %>%
  select(-pop_13) %>%
  rename(pop = pop_92, curr_pop = pop_02)

###############################################################################
##### 3. Create migration rate and flow panels
###############################################################################

inmig_df <- bind_rows(
  build_5yr_inflow(benin13_df, 2013),
  build_5yr_inflow(benin13_df, 2008),
  build_5yr_inflow(benin02_df, 2002),
  build_5yr_inflow(benin02_df, 1997)
)

outmig_df <- bind_rows(
  build_5yr_outflow(benin13_df, 2013),
  build_5yr_outflow(benin13_df, 2008),
  build_5yr_outflow(benin02_df, 2002),
  build_5yr_outflow(benin02_df, 1997)
)

mig_df <- full_join(inmig_df, outmig_df) 

saveRDS( mig_df, paste0(ben_mig.dir, "BEN_5yr_flows.rds"))