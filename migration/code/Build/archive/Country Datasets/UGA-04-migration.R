# Project: Migration Africa
# Author:  Sam Marshall
# Created: Nov 29, 2021
# Title:   Uganda Migration Flows
# Output:  
###############################################################################

source("code/SSA_env_SetUp.R")
source("code/Functions/F-migration.R")
source("code/Functions/F-ts.R")
source("code/Functions/F-Forest.R")

###############################################################################
##### Read data
###############################################################################

panel_df <- readRDS( paste0(uga.dir, "LSMS/panel.rds"))

pop_df <- readRDS( paste0(uga.dir, "Support/population.rds"))
distance_df <- readRDS(paste0(uga.dir, "Support/district_distance.rds"))
cover_df <- readRDS( paste0(uga.dir, "Forest Cover/forest_cover.rds"))

###############################################################################
##### Create panel of location and migration dates
###############################################################################

mig1y_df <- mig_1y(panel_df, "district", 2001, 2013, w_low = 0.01, w_high = 0.99)

mig5y_df <- bind_rows(
  mig_agg(mig1y_df, 2001, 2005, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2006, 2010, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2011, 2015, w_low = 0.01, w_high = 0.99) 
) %>%
  left_join(cover_df %>% select(district_name, area) %>% unique()) %>%
  mutate(log_emigration_flow_area = log_emigration_flow - log(area),
         net_migration_area = net_migration / area,
         log_net_migration_area = log_net_migration - log(area),
         log_delta_p_area = log_delta_p - log(area))

saveRDS( mig1y_df, paste0(uga.dir, "Migration/migration_annual.rds"))
saveRDS( mig5y_df, paste0(uga.dir, "Migration/migration_5yr.rds"))

###############################################################################
##### regional migration
###############################################################################

mig1y_df <- mig_1y(panel_df, "region", 2001, 2013, w_low = 0.01, w_high = 0.99)

mig5y_df <- bind_rows(
  mig_agg(mig1y_df, 2001, 2005, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2006, 2010, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2011, 2015, w_low = 0.01, w_high = 0.99) 
) %>%
  left_join(cover_df %>% select(district_name, area) %>% unique()) %>%
  mutate(log_emigration_flow_area = log_emigration_flow - log(area))

saveRDS( mig5y_df, paste0(uga.dir, "Migration/regional_migration_5yr.rds"))

###############################################################################
##### Kampala migration
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

kmp_df <- panel_df %>%
  filter(district == 102) %>%
  filter(prev_district != 102)

mig1y_df <- mig_1y(kmp_df, "district", 2001, 2013, 
                   w_low = 0.01, w_high = 0.99) 

mig5y_df <- bind_rows(
  mig_agg(mig1y_df, 2001, 2005, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2006, 2010, w_low = 0.01, w_high = 0.99),
  mig_agg(mig1y_df, 2011, 2015, w_low = 0.01, w_high = 0.99) 
) %>%
  full_join(miss_dist) %>%
  filter(district != 102) %>%
  mutate(log_emigration_flow = ifelse(is.na(log_emigration_flow), 0, log_emigration_flow))

saveRDS( mig5y_df, paste0(uga.dir, "Migration/Kampala_immigration.rds"))

###############################################################################
##### Emigration Flows out of high forest loss areas
###############################################################################
# check for high forest loss areas
forest_df <- readRDS( paste0(uga.dir, "Forest Cover/loss_5yr.rds")) 
#dist_df <- miss_dist %>% select(district, district_name) %>% unique()
temp_df <- left_join(
  forest_df, 
  miss_dist %>% select(district, district_name) %>% unique()) 

emig_df <- data.frame()

for(d in c(407, 116, 120)) {
  d_df <- panel_df %>%
    filter(district != d) %>%
    filter(prev_district == d)
  
  dist_name <- miss_dist %>% select(district, district_name) %>% unique() %>%
    filter(district == d) %>% select(district_name) %>% as.character()
  
  mig1y_df <- mig_1y(d_df, "district", 2001, 2013, w_low = 0.01, w_high = 0.99) 
  
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

saveRDS( emig_df, paste0(uga.dir, "Migration/forest_loss_emigration.rds"))

###############################################################################
##### 5. Migration Time Series
###############################################################################

base_pop_df <- pop_df %>%
  select(district, starts_with("urban"), pop_00)

all_df <- mig_1y(panel_df, "district", 1999, 2013)
s05_df <- mig_1y(panel_df %>% filter(surv_year == 2005), "district", 1999, 2005)
s09_df <- mig_1y(panel_df %>% filter(surv_year == 2009), "district", 1999, 2009)
s10_df <- mig_1y(panel_df %>% filter(surv_year == 2010), "district", 1999, 2010)
s11_df <- mig_1y(panel_df %>% filter(surv_year == 2011), "district", 1999, 2011)
s13_df <- mig_1y(panel_df %>% filter(surv_year == 2013), "district", 1999, 2013)

# all together
uga_ts <- mig_ts(all_df, 1999, 2013)

uga_surv_ts <- bind_rows(
  mig_ts(s05_df, 1999, 2005) %>% mutate(Survey = 2005),
  mig_ts(s09_df, 1999, 2009) %>% mutate(Survey = 2009),
  mig_ts(s10_df, 1999, 2010) %>% mutate(Survey = 2010),
  mig_ts(s11_df, 1999, 2011) %>% mutate(Survey = 2011),
  mig_ts(s13_df, 1999, 2013) %>% mutate(Survey = 2013))

saveRDS( uga_ts, paste0(uga.dir, "Migration/migration_timeseries.rds"))
saveRDS( uga_surv_ts, paste0(uga.dir, "Migration/svy_migration_timeseries.rds"))

###############################################################################
##### 6. Origin-destination migration flows
###############################################################################

# get tree loss for each period
ann_treeloss <- readRDS(paste0(uga.dir, "Forest Cover/loss_ann.rds"))
loss_04_13 <- f_custom_loss(ann_treeloss, district_name, 2004, 2013, avg = TRUE)
lag_loss <- f_custom_loss(ann_treeloss, district_name, 2001, 2003, avg = TRUE) %>%
  select(district_name, log_loss) %>%
  rename(lag_log_loss = log_loss)

loss30_df <- readRDS(paste0(uga.dir, "Forest Cover/loss30.rds")) %>%
  select(district_name, loss) %>%
  rename(loss30 = loss) %>%
  mutate(log_loss30 = log((loss30+1) / 20))
# loss going five years back and five years foward with overlap
fb_loss <- full_join(
  f_custom_loss(ann_treeloss, district_name, 2001, 2008, avg = TRUE) %>%
    select(district_name, log_loss) %>%
    rename(start_log_loss = log_loss),
  f_custom_loss(ann_treeloss, district_name, 2009, 2018, avg = TRUE) %>%
    select(district_name, log_loss) %>%
    rename(end_log_loss = log_loss) 
)

# origin destination flows for each period
od_94_03 <- od_mig_flow(panel_df, 1994, 2003) 
od_04_13 <- od_mig_flow(panel_df, 2004, 2013)

#temp_df <- od_04_13 %>% filter(e_ot == 0 & i_ot == 0)

Bartik_df <- bartik_inst(od_94_03, od_04_13, loss_04_13) %>%
  full_join(lag_loss) %>%
  full_join(fb_loss) %>%
  full_join(loss30_df)

saveRDS( Bartik_df, paste0(uga.dir, "Migration/Bartik_migration_04_13.rds"))
write_dta(Bartik_df, file.path(uga.dir, "Migration/Bartik_migration_04_13.dta"))


