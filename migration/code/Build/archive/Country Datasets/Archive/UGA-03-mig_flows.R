# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Nov 29, 2021
# Title:   Uganda Panel Migration Flows
# Output:  
#          
###############################################################################

source("code/SSA_env_SetUp.R")

###############################################################################
##### Create panel of location and migration dates
###############################################################################

# dataset of all migration years and beginning and ending location
panel_df <- bind_rows(
  read_dta( paste0(uga_lsms.dir, "Uganda_2005.dta" )) %>%
    remove_all_labels(),
  read_dta( paste0(uga_lsms.dir, "Uganda_2009.dta" )) %>%
    remove_all_labels(),
  read_dta( paste0(uga_lsms.dir, "Uganda_2010.dta" )) %>%
    remove_all_labels(),
  read_dta( paste0(uga_lsms.dir, "Uganda_2011.dta" )) %>%
    remove_all_labels(),
  read_dta( paste0(uga_lsms.dir, "Uganda_2013.dta" )) %>%
    remove_all_labels() ) %>%
  mutate(migration_year = ifelse(is.na(migration_year) == TRUE, irw_year - duration, migration_year),
         # code non-migrants as 1999
         migration_year = ifelse(migration_year < 1999, 1999, migration_year),
         # make 2014 moves in 2013
         migration_year = ifelse(migration_year == 2014, 2013, migration_year)) %>%
  drop_na(migration_year) %>%
  select(district, district_name, prev_district, prev_district_name, duration,
         migration_year, p_wgt, age, year, irw_year, urban, region, prev_region) %>%
  drop_na(p_wgt)

# # stocks of migrants and flows
# stock_df <- panel_df %>%
#   filter(duration != 100) %>%
#   mutate(migrant = ifelse(district != prev_district, 1, 0)) %>%
#   filter(migrant == 1) %>%
#   group_by(district, district_name, region, year, migration_year) %>%
#   summarise(stock = sum(p_wgt))

# create a list of the districts that were sampled in each wave, so that we only use
# those that were sampled for creating migration rates
dist_set_df <- panel_df %>% 
  select(district, year) %>% 
  unique() %>%
  rename(curr_dist_code = district)

# get urban/rural status of each district
urban_df <- panel_df %>% 
  group_by(district, year) %>% 
  summarise(urban = round(weighted.mean(urban, w = p_wgt)),
            .groups = 'drop') %>%
  # make urban consistent across waves in case of change
  group_by(district) %>%
  summarise(urban = median(urban)) 

###############################################################################
##### Census populations
###############################################################################

pop_df <- read_excel(paste0(uga_raw.dir, "Census_populations_2014.xlsx"),
                     range = "A1:F140") %>%
  filter(Status != "Region") 

names(pop_df) <- c("district_name", "type", "pop_91", "pop_02", "pop_14", "pop_20")

pop_df %<>%
  mutate(
    district_name = ifelse(substr(district_name, 1, 6) == "Luwero", "Luwero", district_name),
    district_name = ifelse(substr(district_name, 1, 10) ==  "Ssembabule", "Ssembabule", district_name) 
    ) %>%
  full_join(panel_df %>% select(district, district_name) %>% unique() ) %>%
  drop_na(district) %>%
  select(-type)

###############################################################################
##### 5 year migration in migration rates
###############################################################################

# starting with creating flows in each year and then go to five year windows...
# still working on this
# immig_df <- panel_df %>%
#   filter(migration_year > 1999) %>%
#   mutate(migrant = ifelse(district != prev_district, 1, 0)) %>%
#   # first summarise by year so only count waves where the dist was sampled
#   group_by(district, district_name, migration_year, year) %>%
#   summarise(flow = sum(migrant * p_wgt),
#             .groups = "drop") %>%
#   drop_na(district) %>%
#   # keep only the set of districts that were sampled
#   # inner_join(dist_set_df) %>%
#   group_by(district, district_name, migration_year) %>%
#   summarise(immigration_flow = mean(flow),
#             .groups = "drop")

yr_end <- c(2005, 2010, 2016)

five_yr_panel <- data.frame()
for (i in 1:3) {
  yr_start <- yr_end[i] - 5
  
  samp_df <- panel_df %>%
    filter(irw_year >= yr_start) %>%
    mutate(
      curr_dist = ifelse(migration_year <= yr_end[i], district_name, prev_district_name),
      prev_dist = ifelse(migration_year >= yr_start, prev_district_name, district_name),
      curr_dist_code = ifelse(migration_year <= yr_end[i], district, prev_district),
      prev_dist_code = ifelse(migration_year >= yr_start, prev_district, district),
      migrant = ifelse(curr_dist != prev_dist, 1, 0)) 
  
  immig_df <- samp_df %>%
    # first summarise by year so only count waves where the dist was sampled
    group_by(curr_dist, curr_dist_code, year) %>%
    summarise(flow = sum(migrant * p_wgt),
              .groups = "drop") %>%
    drop_na(curr_dist_code) %>%
    # keep only the set of districts that were sampled
    inner_join(dist_set_df) %>%
    group_by(curr_dist, curr_dist_code) %>%
    summarise(immigration_flow = mean(flow),
              .groups = "drop")
  
  emig_df <- samp_df %>%
    group_by(prev_dist, prev_dist_code, year) %>%
    summarise(flow = sum(migrant * p_wgt),
              .groups = "drop") %>%
    drop_na(prev_dist_code) %>%
    # rename to merge
    rename(curr_dist = prev_dist, curr_dist_code = prev_dist_code) %>%
    # keep only the set of districts that were sampled
    inner_join(dist_set_df) %>%
    group_by(curr_dist, curr_dist_code) %>%
    summarise(emigration_flow = mean(flow),
              .groups = "drop") %>%
    mutate(range = paste0(yr_start[i], "-", yr_end[i]))
  
  # join dfs and add to full df
  migration_df <- full_join(immig_df, emig_df) %>%
    mutate(range = paste0(yr_start, "-", yr_end[i])) %>% 
    rename(district = curr_dist_code, district_name = curr_dist)
  
  five_yr_panel <- bind_rows(five_yr_panel, migration_df)
}

panel_5y <- five_yr_panel %>%
  left_join(pop_df) %>%
  mutate(pop_02 = ifelse(pop_02 == "...", NA, pop_02),
         pop_02 = as.numeric(pop_02),
         # use population projection to calculate estimated populations for 
         # districts that didn't exist
         growth_rate = ifelse(is.na(pop_02) == FALSE, 
                              (pop_14 / pop_02)^(1/12) - 1,
                              (pop_20 / pop_14)^(1/6) - 1 ),
         pop_00 = pop_14 / (1 + growth_rate)^14,
         # for missing use projection
         # pop_00 = ifelse(is.na(pop_00) == TRUE, pop_20 / (1 + growth_rate_p)^20, pop_00),
         pop_05 = pop_14 / (1 + growth_rate)^9,
         #pop_05 = ifelse(is.na(pop_05) == TRUE, pop_20 / (1 + growth_rate_p)^15, pop_05 ),
         pop_11 = pop_14 / (1 + growth_rate)^3,
         #pop_11 = ifelse(is.na(pop_11) == TRUE, pop_20 / (1 + growth_rate_p)^9, pop_11 ),
         # create population at period start
         pop_start = ifelse(range == "2000-2005", pop_00,
                            ifelse(range == "2005-2010", pop_05, pop_11)),
         #use populations to create rates
         immigration_rate = immigration_flow / pop_start,
         emigration_rate = emigration_flow / pop_start) %>%
  select(-c(pop_91:pop_11)) %>%
  left_join(urban_df) %>%
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

saveRDS( panel_5y, paste0(uga_mig.dir, "UGA_5yr_migration.rds"))

rm(samp_df, immig_df, emig_df, five_yr_panel, migration_df)


###############################################################################
##### 5 year immigration rates by wave
###############################################################################
uga18_df <- read_dta( paste0(uga_lsms.dir, "Uganda_2018.dta" )) %>%
  remove_all_labels() %>%
  drop_na(district) %>%
  drop_na(panel_wgt)

# add districts to set
dist_set_df <- bind_rows(panel_df, uga18_df) %>%
  select(district, year) %>% unique() 

# add urban rural status to new districts
urban18_df <- uga18_df %>% 
  group_by(district) %>% 
  summarise(urban2 = round(weighted.mean(urban, w = panel_wgt)),
            .groups = 'drop') 

urban_df <- full_join(urban_df, urban18_df) %>%
  mutate(urban = ifelse(is.na(urban) == TRUE, urban2, urban)) %>%
  select(-urban2)

# five year immigration and emigration rates and flows from each wave
wave_panel <- full_join(
  # immigration flows
  panel_df %>%
    mutate(
      prev_district_name = ifelse(migration_year >= year - 5, prev_district_name, district_name),
      prev_district = ifelse(migration_year >= year - 5, prev_district, district),
      migrant = ifelse(district != prev_district, 1, 0),
      flow = migrant * p_wgt) %>%
    group_by(district, district_name, year) %>%
    summarise(immigration_flow = sum(flow),
              .groups = "drop") %>%
    drop_na(district),
  # emigration flows
  panel_df %>%
    mutate(
      prev_district_name = ifelse(migration_year >= year - 5, prev_district_name, district_name),
      prev_district = ifelse(migration_year >= year - 5, prev_district, district),
      migrant = ifelse(district != prev_district, 1, 0),
      flow = migrant * p_wgt) %>%
    group_by(prev_district, prev_district_name, year) %>%
    summarise(emigration_flow = sum(flow),
              .groups = "drop") %>%
    drop_na(prev_district) %>%
    filter(prev_district < 500) %>%
    rename(district = prev_district, district_name = prev_district_name)
) 

# do for 2018 -- this one is from 2015 survey (so tracked households)
uga18_mig <- full_join(
  # immigration flows
  uga18_df %>%
    mutate(migrant = ifelse(district != prev_district, 1, 0),
           # use panel weight b/c of attrition and using panel
           flow = migrant * panel_wgt) %>%
    group_by(district, district_name, year) %>%
    summarise(immigration_flow = sum(flow),
              .groups = "drop") %>%
    drop_na(district),
  # emigration flows
  uga18_df %>%
    mutate(migrant = ifelse(district != prev_district, 1, 0),
           flow = migrant * panel_wgt) %>%
    group_by(prev_district, prev_district_name, year) %>%
    summarise(emigration_flow = sum(flow),
              .groups = "drop") %>%
    drop_na(prev_district) %>%
    filter(prev_district < 500) %>%
    rename(district = prev_district, district_name = prev_district_name)
)

wave_panel <- bind_rows( wave_panel, uga18_mig) %>%
  # keep only the set of districts that were sampled
  inner_join(dist_set_df) %>%
  # add pop data to create rates
  left_join(pop_df) %>%
  mutate(
    pop_02 = ifelse(pop_02 == "...", NA, pop_02),
    pop_02 = as.numeric(pop_02),
    # use population projection to calculate estimated populations for 
    # districts that didn't exist
    # step 1. get the implied growth rate - use 2002 data if exists, otherwise 2020 projection
    growth_rate = ifelse(is.na(pop_02) == FALSE, 
                         (pop_14 / pop_02)^(1/12) - 1,
                         (pop_20 / pop_14)^(1/6) - 1 ),
    #growth_rate = ifelse(is.na(growth_rate) == TRUE, (pop_20 / pop_14)^(1/6) - 1, growth_rate),
    # step 2. population at time start
    pop_00 = pop_14 / (1 + growth_rate)^14,
    # step 3. create population at baseline
    pop_start = pop_00 * (1 + growth_rate)^(year - 2000 - 5),
    # step 4. use populations at baseline to create rates
    immigration_rate = immigration_flow / pop_start,
    emigration_rate = emigration_flow / pop_start
    ) %>%
  select(-c(pop_91:pop_00)) %>%
  left_join(urban_df) %>%
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


saveRDS( wave_panel, paste0(uga_mig.dir, "UGA_5yr_wave_migration.rds"))

