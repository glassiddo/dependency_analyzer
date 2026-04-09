# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Nov 29, 2021
# Edited:  Nov 29, 2021
# Title:   Tanzania Panel
# Output:  
#          
###############################################################################

source("code/SSA_env_SetUp.R")

###############################################################################
##### Create panel of location and migration dates
###############################################################################

# dataset of all migration years and beginning and ending location
panel_df <- bind_rows(
  read_dta( paste0(tza_lsms.dir, "Tanzania_2008.dta" )),
  read_dta( paste0(tza_lsms.dir, "Tanzania_2010.dta" )),
  read_dta( paste0(tza_lsms.dir, "Tanzania_2012.dta" )),
  read_dta( paste0(tza_lsms.dir, "Tanzania_2014.dta" )),
  # don't use wave 5, it is too small
  #read_dta( paste0(tza_lsms.dir, "Tanzania_2019.dta" )) 
  ) %>%
  drop_na(duration) %>%
  mutate(migration_year = irw_year - duration,
         # code non-migrants as 1999
         migration_year = ifelse(migration_year < 1999, 1999, migration_year)) %>%
  select(district, district_name, region_name, prev_district, prev_district_name,
         migration_year, panel_wgt, move_reason, xwalk_id, year, irw_year) %>%
  mutate(district_name = str_to_title(district_name),
         prev_district_name = str_to_title(prev_district_name))

# create a list of the districts that were sampled in each wave, so that we only use
# those that were sampled for creating migration rates
dist_set_df <- panel_df %>% select(district, year) %>% unique() 

###############################################################################
##### Census populations
###############################################################################

tza_pop_df <- read_excel(paste0(tza_raw.dir, "Census_populations_2012.xlsx"),
                     range = "A1:E202")

names(tza_pop_df) <- c("district_name", "type", "pop_88", "pop_02", "pop_12")

dist_pop_df <- tza_pop_df %>%
  filter(type != "Region") %>%
  mutate(
    district_name = sub("Municipal", "Urban", district_name),
    district_name = sub("City", "Urban", district_name),
    district_name = ifelse(district_name == "Babati Rural", "Babati", district_name),
    district_name = ifelse(district_name == "Chake Chake", "Chakechake", district_name),
    district_name = ifelse(district_name == "Handeni Rural", "Handeni", district_name),
    district_name = ifelse(district_name == "Ilala Urban", "Ilala", district_name),
    district_name = ifelse(district_name == "Ilemela Urban", "Ilemela", district_name),
    district_name = ifelse(district_name == "Kahama Rural", "Kahama", district_name),
    district_name = ifelse(substr(district_name, 1,11)  == "Kaskazini A", "Kaskazini ‘A’", district_name),
    district_name = ifelse(substr(district_name, 1,11)  == "Kaskazini B", "Kaskazini ‘B’", district_name),
    district_name = ifelse(district_name == "Kasulu Rural", "Kasulu", district_name),
    district_name = ifelse(substr(district_name, 1, 4) == "Kati", "Kati", district_name),
    district_name = ifelse(district_name == "Kigoma-Ujiji Urban", "Kigoma Urban", district_name),
    district_name = ifelse(district_name == "Kinondoni Urban", "Kinondoni", district_name),
    district_name = ifelse(district_name == "Korogwe Rural", "Korogwe", district_name),
    district_name = ifelse(substr(district_name, 1, 6) == "Kusini", "Kusini", district_name),
    district_name = ifelse(district_name == "Mafinga Town", "Mafinga", district_name),
    district_name = ifelse(substr(district_name, 1, 9) == "Magharibi", "Magharibi", district_name),
    district_name = ifelse(district_name == "Makambako Town", "Makambako", district_name),
    district_name = ifelse(district_name == "Masasi Rural", "Masasi", district_name),
    district_name = ifelse(district_name == "Mbarali", "Mbalali", district_name),
    district_name = ifelse(district_name == "Micheweni", "Michweweni", district_name),
    district_name = ifelse(substr(district_name, 1, 5) == "Mjini", "Mjini", district_name),
    district_name = ifelse(district_name == "Mpanda Town", "Mpanda Urban", district_name),
    district_name = ifelse(district_name == "Mtwara Urban", "Mtwara Mikindani", district_name),
    district_name = ifelse(district_name == "Njombe Town", "Njombe Urban", district_name),
    district_name = ifelse(district_name == "Nyamagana Urban", "Nyamagana", district_name),
    district_name = ifelse(district_name == "Temeke Urban", "Temeke", district_name),
    district_name = ifelse(district_name == "Urambo", "Uramba", district_name),
  )

region_pop_df <- tza_pop_df %>%
  filter(type == "Region") %>%
  select(-type, -pop_88) %>%
  rename(region_name = district_name, r_pop_02 = pop_02, r_pop_12 = pop_12) %>%
  mutate(
    region_name = ifelse(region_name == "Dar es Salaam", "Dar Es Salaam", region_name),
    region_name = ifelse(substring(region_name, 1, 15) == "Kaskazini Pemba", 
                         "Kaskazini Pemba", region_name),
    region_name = ifelse(substring(region_name, 1, 16) == "Kaskazini Unguja", 
                         "Kaskazini Unguja", region_name),
    region_name = ifelse(substring(region_name, 1, 12) == "Kusini Pemba", 
                         "Kusini Pemba", region_name),
    region_name = ifelse(substring(region_name, 1, 13) == "Kusini Unguja", 
                         "Kusini Unguja", region_name),
    region_name = ifelse(substring(region_name, 1, 5) == "Mjini", 
                         "Mjini/Magharibi Unguja", region_name),
    r_pop_02 = as.numeric(r_pop_02),
    r_pop_12 = as.numeric(r_pop_12))

pop_df <- full_join(
  dist_pop_df, 
  # get district names from panel
  panel_df %>% 
    select(district, district_name, region_name) %>% 
    unique() ) %>%
  arrange(district_name) %>%
  drop_na(district) %>%
  select(-type) %>%
  full_join(region_pop_df) %>%
  drop_na(district) %>%
  mutate(
    pop_02 = ifelse(pop_02 == "...", NA, pop_02),
    pop_02 = as.numeric(pop_02),
    # step 1. get the implied growth rate - use region growth rate if new district
    growth_rate = ifelse(is.na(pop_02) == FALSE, 
                         (pop_12 / pop_02)^(1/10) - 1,
                         (r_pop_12 / r_pop_02)^(1/10) - 1 ),
    # step 2. population at time start
    pop_00 = pop_12 / (1 + growth_rate)^12 ) %>%
  select(district, growth_rate, pop_00)

rm(tza_pop_df, region_pop_df)

###############################################################################
##### Year-location in migration rates
###############################################################################

one_yr_panel <- data.frame()
for (yr in 2001:2015) {
  samp_df <- panel_df %>%
    filter(irw_year >= yr) %>%
    mutate(district_name = ifelse(migration_year <= yr, district_name, prev_district_name),
           prev_district_name = ifelse(migration_year < yr, district_name, prev_district_name),
           district = ifelse(migration_year <= yr, district, prev_district),
           prev_district = ifelse(migration_year < yr, district, prev_district),
           migrant = ifelse(district != prev_district, 1, 0) ) 
  
  immig_df <- samp_df %>%
    # first summarise by year so only count waves where the dist was sampled
    group_by(district, district_name, year) %>%
    summarise(flow = sum(migrant * panel_wgt),
              .groups = "drop") %>%
    drop_na(district) %>%
    filter(district != 999) %>%
    # keep only the set of districts that were sampled
    inner_join(dist_set_df) %>%
    group_by(district, district_name) %>%
    summarise(immigration_flow = mean(flow),
              .groups = "drop") 
  
  emig_df <- samp_df %>%
    group_by(prev_district, prev_district_name, year) %>%
    summarise(flow = sum(migrant * panel_wgt),
              .groups = "drop") %>%
    drop_na(prev_district) %>%
    filter(prev_district != 999) %>%
    # rename to merge
    rename(district = prev_district, district_name = prev_district_name) %>%
    # keep only the set of districts that were sampled
    inner_join(dist_set_df) %>%
    group_by(district, district_name) %>%
    summarise(emigration_flow = mean(flow),
              .groups = "drop") %>%
    mutate(year = yr)
  
  # join dfs and add to full df
  migration_df <- full_join(immig_df, emig_df) %>%
    mutate(year = yr) 
  
  one_yr_panel <- bind_rows(one_yr_panel, migration_df)
}

one_yr_panel %<>%
  left_join(pop_df) %>%
  mutate(
    # create population at start
    pop_start = pop_00 * (1 + growth_rate)^(year - 2000 - 1),
    # use populations at baseline to create rates
    immigration_rate = immigration_flow / pop_start,
    emigration_rate = emigration_flow / pop_start ) %>%
  select(-growth_rate, -pop_00) %>%
  group_by(year) %>%
  mutate(
    across(c(immigration_rate, emigration_rate, immigration_flow, emigration_flow),
           ~Winsorize(.x, probs = c(0.01, 0.99), na.rm = TRUE),
           .names = "{.col}_W"),
    across(c(immigration_flow_W, emigration_flow_W),
           ~ifelse(.x != 0, log(.x), 0),
           .names = "log_{.col}"),
    .groups = 'drop') %>%
  rename(log_immigration_flow = log_immigration_flow_W,
         log_emigration_flow = log_emigration_flow_W)

saveRDS( one_yr_panel, paste0(tza_mig.dir, "TZA_1yr_migration.rds"))

rm(emig_df, immig_df, migration_df, samp_df)

###############################################################################
##### 5 year migration flows and rates
###############################################################################

yr_end <- c(2005, 2010, 2016)

five_yr_panel <- data.frame()
for (i in 1:3) {
  
  yr_start <- yr_end[i] - 5
  
  samp_df <- panel_df %>%
    filter(irw_year > yr_start) %>%
    mutate(
      district_name = ifelse(migration_year <= yr_end[i], district_name, prev_district_name),
      prev_district_name = ifelse(migration_year >= yr_start, prev_district_name, district_name),
      district = ifelse(migration_year <= yr_end[i], district, prev_district),
      prev_district = ifelse(migration_year >= yr_start, prev_district, district),
      migrant = ifelse(district != prev_district, 1, 0) )
  
  immig_df <- samp_df %>%
    # first summarise by year so only count waves where the dist was sampled
    group_by(district, district_name, year) %>%
    summarise(flow = sum(migrant * panel_wgt),
              .groups = "drop") %>%
    drop_na(district) %>%
    filter(district != 999) %>%
    # keep only the set of districts that were sampled
    inner_join(dist_set_df) %>%
    group_by(district, district_name) %>%
    summarise(immigration_flow = mean(flow),
              .groups = "drop")
  
  emig_df <- samp_df %>%
    group_by(prev_district, prev_district_name, year) %>%
    summarise(flow = sum(migrant * panel_wgt),
              .groups = "drop") %>%
    drop_na(prev_district) %>%
    filter(prev_district != 999) %>%
    # rename to merge
    rename(district = prev_district, district_name = prev_district_name) %>%
    # keep only the set of districts that were sampled
    inner_join(dist_set_df) %>%
    group_by(district, district_name) %>%
    summarise(emigration_flow = mean(flow),
              .groups = "drop") 
  
  # join dfs and add to full df
  migration_df <- full_join(immig_df, emig_df) %>%
    mutate(range = paste0(yr_start, "-", yr_end[i]),
           year = yr_start) 
  
  five_yr_panel <- bind_rows(five_yr_panel, migration_df)
}

five_yr_panel %<>%
  left_join(pop_df) %>%
  mutate(
    # create population at start
    pop_start = pop_00 * (1 + growth_rate)^(year - 2000),
    # use populations at baseline to create rates
    immigration_rate = immigration_flow / pop_start,
    emigration_rate = emigration_flow / pop_start) %>%
  select(-year, -growth_rate, -pop_00) %>%
  group_by(range) %>%
  mutate(
    across(c(immigration_rate, emigration_rate, immigration_flow, emigration_flow),
           ~Winsorize(.x, probs = c(0.01, 0.99), na.rm = TRUE),
           .names = "{.col}_W"),
    across(c(immigration_flow_W, emigration_flow_W),
           ~ifelse(.x != 0, log(.x), 0),
           .names = "log_{.col}"),
    .groups = 'drop') %>%
  rename(log_immigration_flow = log_immigration_flow_W,
         log_emigration_flow = log_emigration_flow_W)

saveRDS( five_yr_panel, paste0(tza_mig.dir, "TZA_5yr_migration.rds"))


