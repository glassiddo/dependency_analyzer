# Project: Migration Africa
# Author:  Sam Marshall
# Created: Nov 29, 2021
# Title:   Tanzania Migration Flows
# Output:  
###############################################################################

source("code/SSA_env_SetUp.R")
source("code/Functions/F-migration.R")
source("code/Functions/F-ts.R")
source("code/Functions/F-Forest.R")

###############################################################################
##### Read data
###############################################################################

panel_df <- readRDS( paste0(tza.dir, "LSMS/panel.rds"))

pop_df <- readRDS( paste0(tza.dir, "Support/census_population.rds"))
distance_df <- readRDS(paste0(tza.dir, "Support/district_distance.rds"))
cover_df <- readRDS( paste0(tza.dir, "Forest Cover/forest_cover.rds"))

###############################################################################
##### Create panel of location and migration dates
###############################################################################

mig1y_df <- mig_1y(panel_df, "district", 2001, 2014, w_low = 0.01, w_high = 0.99)

mig5y_df <- bind_rows(
  mig_agg(mig1y_df, 2001, 2005, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2006, 2010, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2011, 2015, w_low = 0.01, w_high = 0.99) 
) 

saveRDS( mig1y_df, paste0(tza.dir, "Migration/migration_annual.rds"))
saveRDS( mig5y_df, paste0(tza.dir, "Migration/migration_5yr.rds"))


###############################################################################
##### regional migration
###############################################################################

mig1y_df <- mig_1y(panel_df, "region", 2001, 2014, w_low = 0.01, w_high = 0.99)

mig5y_df <- bind_rows(
  mig_agg(mig1y_df, 2001, 2005, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2006, 2010, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2011, 2015, w_low = 0.01, w_high = 0.99) 
) 

saveRDS( mig5y_df, paste0(tza.dir, "Migration/regional_migration_5yr.rds"))

###############################################################################
##### migration into Dar es Salaam and Mwanza
###############################################################################

# make panel of all districts for each year to merge in with immigration flows
# so that missing are properly recorded as zero
miss_dist <- bind_rows(
  panel_df %>% select(district, district_name) %>% unique() %>% 
    mutate(range = "2001-2005"),
  panel_df %>% select(district, district_name) %>% unique() %>% 
    mutate(range = "2006-2010"),
  panel_df %>% select(district, district_name) %>% unique() %>% 
    mutate(range = "2011-2015"),
)

# Dar es Salaam
dsm_df <- panel_df %>%
  filter(district %in% c(70:79)) %>%
  filter(prev_district %!in% c(70:79))
 
mig1y_df <- mig_1y(dsm_df, "district", 2001, 2015, w_low = 0.01, w_high = 0.99) 

mig5y_df <- bind_rows(
  mig_agg(mig1y_df, 2001, 2005, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2006, 2010, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2011, 2015, w_low = 0.01, w_high = 0.99) 
) %>%
  full_join(miss_dist) %>%
  filter(district %!in% c(70:79)) %>%
  mutate(log_emigration_flow = ifelse(is.na(log_emigration_flow), 0, log_emigration_flow))

saveRDS( mig5y_df, paste0(tza.dir, "Migration/DSM_immigration.rds"))

# Mwanza
mwanza_df <- panel_df %>%
  filter(district %in% c(193,196)) %>%
  filter(prev_district %!in% c(193,196))

mig1y_df <- mig_1y(mwanza_df, "district", 2001, 2015, w_low = 0.01, w_high = 0.99)

mig5y_df <- bind_rows(
  mig_agg(mig1y_df, 2001, 2005, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2006, 2010, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2011, 2015, w_low = 0.01, w_high = 0.99) 
) %>%
  full_join(miss_dist) %>%
  filter(district %!in% c(193,196)) %>%
  mutate(log_emigration_flow = ifelse(is.na(log_emigration_flow), 0, log_emigration_flow))

saveRDS( mig5y_df, paste0(tza.dir, "Migration/Mwanza_immigration.rds"))

# Zanzibar
zanz_df <- panel_df %>%
  filter(district %in% c(511:552)) %>%
  filter(prev_district %!in% c(511:552))

mig1y_df <- mig_1y(zanz_df, "district", 2001, 2015, 
                   w_low = 0.01, w_high = 0.99) 

mig5y_df <- bind_rows(
  mig_agg(mig1y_df, 2001, 2005, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2006, 2010, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2011, 2015, w_low = 0.01, w_high = 0.99) 
) %>%
  full_join(miss_dist) %>%
  filter(district %!in% c(511:552)) %>%
  mutate(log_emigration_flow = ifelse(is.na(log_emigration_flow), 0, log_emigration_flow))

saveRDS( mig5y_df, paste0(tza.dir, "Migration/ZNZ_immigration.rds"))


###############################################################################
##### Emigration Flows out of high forest loss areas
###############################################################################
# check for high forest loss areas
#forest_df <- readRDS( paste0(tza.dir, "Forest Cover/loss_5yr.rds")) 
#dist_df <- miss_dist %>% select(district, district_name) %>% unique()
#temp_df <- left_join(forest_df, dist_df) %>% arrange(-ln_loss_area)

emig_df <- data.frame()

for(d in c(522, 91, 118, 147, 65)) {
  d_df <- panel_df %>%
    filter(district != d) %>%
    filter(prev_district == d)
  
  dist_name <- miss_dist %>% select(district, district_name) %>% unique() %>%
    filter(district == d) %>% select(district_name) %>% as.character()
  
  mig1y_df <- mig_1y(d_df, "district", 2001, 2015, w_low = 0.01, w_high = 0.99) 
  
  mig5y_df <- bind_rows(
    mig_agg(mig1y_df, 2001, 2005, w_low = 0.01, w_high = 0.99),
    mig_agg(mig1y_df, 2006, 2010, w_low = 0.01, w_high = 0.99),
    mig_agg(mig1y_df, 2011, 2015, w_low = 0.01, w_high = 0.99) 
  ) %>%
    full_join(miss_dist) %>%
    filter(district != d) %>%
    mutate(log_immigration_flow = ifelse(is.na(log_immigration_flow), 0, log_immigration_flow),
           emigration_district = d,
           emigration_district_name = dist_name)
  
  emig_df <- bind_rows(emig_df, mig5y_df)
}

saveRDS( emig_df, paste0(tza.dir, "Migration/forest_loss_emigration.rds"))

###############################################################################
##### 5. Migration Time Series
###############################################################################

base_pop_df <- pop_df %>%
  select(district, starts_with("urban"), pop_00)

all_df <- mig_1y(panel_df, "district", 1998, 2014, w_low = 0.01, w_high = 0.99)
s08_df <- mig_1y(panel_df %>% filter(surv_year == 2008), "district", 1998, 2008)
s10_df <- mig_1y(panel_df %>% filter(surv_year == 2010), "district", 1998, 2010)
s12_df <- mig_1y(panel_df %>% filter(surv_year == 2012), "district", 1998, 2012)
s14_df <- mig_1y(panel_df %>% filter(surv_year == 2014), "district", 1998, 2014)

# all together
tza_ts <- mig_ts(all_df, 1998, 2014)

tza_surv_ts <- bind_rows(
  mig_ts(s08_df, 1998, 2008) %>% mutate(Survey = 2008),
  mig_ts(s10_df, 1998, 2010) %>% mutate(Survey = 2010),
  mig_ts(s12_df, 1998, 2012) %>% mutate(Survey = 2012),
  mig_ts(s14_df, 1998, 2014) %>% mutate(Survey = 2014))

saveRDS( tza_ts, paste0(tza.dir, "Migration/migration_timeseries.rds"))
saveRDS( tza_surv_ts, paste0(tza.dir, "Migration/svy_migration_timeseries.rds"))

###############################################################################
##### 6. Origin-destination migration flows
###############################################################################

# get tree loss for each period
ann_treeloss <- readRDS(paste0(tza.dir, "Forest Cover/loss_ann.rds"))
loss_05_14 <- f_custom_loss(ann_treeloss, district_name, 2005, 2014, avg = TRUE)
lag_loss <- f_custom_loss(ann_treeloss, district_name, 2001, 2004, avg = TRUE) %>%
  select(district_name, log_loss) %>%
  rename(lag_log_loss = log_loss)
loss30_df <- readRDS(paste0(tza.dir, "Forest Cover/loss30.rds")) %>%
  select(district_name, loss) %>%
  rename(loss30 = loss) %>%
  mutate(log_loss30 = log((loss30+1) / 20))
# loss going five years back and five years foward with overlap
fb_loss <- full_join(
  f_custom_loss(ann_treeloss, district_name, 2001, 2009, avg = TRUE) %>%
    select(district_name, log_loss) %>%
    rename(start_log_loss = log_loss),
  f_custom_loss(ann_treeloss, district_name, 2010, 2019, avg = TRUE) %>%
    select(district_name, log_loss) %>%
    rename(end_log_loss = log_loss) 
)

# origin destination flows for each period
od_95_04 <- od_mig_flow(panel_df, 1995, 2004) 
od_05_14 <- od_mig_flow(panel_df, 2005, 2014)

Bartik_df <- bartik_inst(od_95_04, od_05_14, loss_05_14) %>%
  full_join(lag_loss) %>%
  full_join(fb_loss) %>%
  full_join(loss30_df)


saveRDS( Bartik_df, paste0(tza.dir, "Migration/Bartik_migration_05_14.rds"))
write_dta(Bartik_df, file.path(tza.dir, "Migration/Bartik_migration_05_14.dta"))


