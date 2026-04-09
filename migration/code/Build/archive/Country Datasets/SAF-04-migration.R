# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Dec 4, 2021
# Title:   South Africa Migration Panel
# Output:  
###############################################################################

source("code/SSA_env_SetUp.R")
source("code/Functions/F-SAF.R")
source("code/Functions/F-ts.R")
source("code/Functions/F-Forest.R")

pop_df <- readRDS( paste0(saf.dir, "Support/population.rds"))

# migration 2011 - 2015
sa_2016 <- readRDS(paste0(saf.dir, "Census/Census_2016.rds"))
# migration 2001 - 2010
sa_2011 <- readRDS(paste0(saf.dir, "Census/Census_2011.rds"))
# migration 1996 - 2000
sa_2001 <- readRDS(paste0(saf.dir, "Census/Census_2001.rds"))

distance_df <- readRDS(paste0(saf.dir, "Support/district_distance.rds"))

cover_df <- readRDS( paste0(saf.dir, "Forest Cover/forest_cover.rds"))

###############################################################################
##### 2. five-year migration flows
###############################################################################

saf16_ann <- mig_flow(sa_2016, 2011, 2015)
saf11_ann <- mig_flow(sa_2011, 2001, 2010)
saf01_ann <- mig_flow(sa_2001, 1996, 2000)

mig5y_df <- bind_rows(
  mig_agg(saf16_ann, 2011, 2015),
  mig_agg(saf11_ann, 2006, 2010),
  mig_agg(saf11_ann, 2001, 2005),
  mig_agg(saf01_ann, 1996, 2000)
) 

#temp_df <- mig5y_df %>% filter(emigration_rate > 15)

saveRDS( saf16_ann, paste0(saf.dir, "Migration/Cens16_migration_annual.rds"))
saveRDS( saf11_ann, paste0(saf.dir, "Migration/Cens11_migration_annual.rds"))
saveRDS( saf01_ann, paste0(saf.dir, "Migration/Cens01_migration_annual.rds"))
saveRDS( mig5y_df, paste0(saf.dir, "Migration/migration_5yr.rds"))

###############################################################################
##### 3. Migration in specific districts
###############################################################################

dist_set <- sa_2016 %>% select(district, district_name) %>% unique()
miss_dist <- bind_rows(dist_set %>% mutate(range = "1996-2000"),
                       dist_set %>% mutate(range = "2001-2005"),
                       dist_set %>% mutate(range = "2006-2010"),
                       dist_set %>% mutate(range = "2011-2015"))

city_immig_df <- bind_rows(
  # Johannesburg
  district_flows(61) %>% mutate(immigration_district_name = "Johannesburg"),
  # Cape Town
  district_flows(228) %>% mutate(immigration_district_name = "Cape Town"),
  # Durban
  district_flows(77) %>% mutate(immigration_district_name = "Durban"),
  # Pretoria
  district_flows(62) %>% mutate(immigration_district_name = "Pretoria")
)

saveRDS( city_immig_df, paste0(saf.dir, "Migration/city_immigration.rds"))

###############################################################################
##### 4. Emigration Flows out of high forest loss areas
###############################################################################
# check for high forest loss areas
forest_df <- readRDS( paste0(saf.dir, "Forest Cover/loss_5yr.rds")) 

emig_df <- data.frame()

for(d in c(161, 166, 46, 92, 226, 24)) {
  emig_df <- bind_rows(emig_df, district_flows(d, immigration = FALSE))
}

saveRDS( emig_df, paste0(saf.dir, "Migration/forest_loss_emigration.rds"))

###############################################################################
##### 5. Migration Time Series
###############################################################################

base_pop_df <- pop_df %>%
  mutate(growth_rate = (pop_01 / pop_96)^(1/5) - 1,
         pop_00 = pop_01 / (1 + growth_rate)^1 ) %>%
  select(district_name, urban, pop_00)

saf_ts <- bind_rows(mig_ts(saf01_ann, 1996, 2000),
                    mig_ts(saf11_ann, 2001, 2010),
                    mig_ts(saf16_ann, 2011, 2016))

saveRDS( saf_ts, paste0(saf.dir, "Migration/migration_timeseries.rds"))

###############################################################################
##### 6. Origin-destination migration flows
###############################################################################

# get tree loss for each period
ann_treeloss <- readRDS(paste0(saf.dir, "Forest Cover/loss_ann.rds"))
loss_01_10 <- f_custom_loss(ann_treeloss, district_name, 2001, 2010, avg = TRUE)
loss_06_15 <- f_custom_loss(ann_treeloss, district_name, 2006, 2015, avg = TRUE)
loss_01_15 <- f_custom_loss(ann_treeloss, district_name, 2001, 2015, avg = TRUE)

loss30_df <- readRDS(paste0(saf.dir, "Forest Cover/loss30.rds")) %>%
  select(district_name, loss) %>%
  rename(loss30 = loss) %>%
  mutate(log_loss30 = log((loss30+1) / 20))
# loss going five years back and five years foward with overlap
fb_loss <- full_join(
  f_custom_loss(ann_treeloss, district_name, 2001, 2010, avg = TRUE) %>%
    select(district_name, log_loss) %>%
    rename(start_log_loss = log_loss),
  f_custom_loss(ann_treeloss, district_name, 2011, 2020, avg = TRUE) %>%
    select(district_name, log_loss) %>%
    rename(end_log_loss = log_loss) 
)

# origin destination flows for each period
od_00 <- od_mig_flow(sa_2001, 1996, 2000) %>% 
  drop_na(distance)
od_01_10 <- od_mig_flow(sa_2011, 2001, 2010)
od_06_15 <- od_mig_flow(bind_rows(sa_2011, sa_2016), 2006, 2015)
od_01_15 <- od_mig_flow(bind_rows(sa_2011, sa_2016), 2001, 2015)

Bartik_01_10 <- bartik_inst(od_00, od_01_10, loss_01_10) %>% full_join(loss30_df)
Bartik_06_15 <- bartik_inst(od_00, od_06_15, loss_06_15) %>% full_join(loss30_df)
Bartik_01_15 <- bartik_inst(od_00, od_01_15, loss_01_15) %>% full_join(loss30_df)


saveRDS( Bartik_01_10, paste0(saf.dir, "Migration/Bartik_migration_01_10.rds"))
write_dta(Bartik_01_10, file.path(saf.dir, "Migration/Bartik_migration_01_10.dta"))

saveRDS( Bartik_06_15, paste0(saf.dir, "Migration/Bartik_migration_06_15.rds"))
write_dta(Bartik_06_15, file.path(saf.dir, "Migration/Bartik_migration_06_15.dta"))

saveRDS( Bartik_01_15, paste0(saf.dir, "Migration/Bartik_migration_01_15.rds"))
write_dta(Bartik_01_15, file.path(saf.dir, "Migration/Bartik_migration_01_15.dta"))

# 2006-2015 using 2001-2005 migration as a control and with lagged forest loss
od_01_05 <- od_mig_flow(sa_2011, 2001, 2005)
loss_01_05 <- f_custom_loss(ann_treeloss, district_name, 2001, 2005, avg = TRUE)
Bartik_01_05 <- bartik_inst(od_00, od_01_05, loss_01_05)

Bartik_lag_df <- full_join(
  Bartik_06_15,
  Bartik_01_05 %>%
    select(district, er_ot, ln_nmr_ot, ln_er_ot, delta_ln_pop_ot, log_loss) %>%
    rename_with( ~paste0("lag_", .x), .cols = !district)
) %>%
  full_join(fb_loss) %>% full_join(loss30_df)

write_dta(Bartik_lag_df, file.path(saf.dir, "Migration/Bartik_migration_06_15_lag.dta"))



# Bartik_df <- bind_rows(
#   bartik_inst(od_00, od_05),
#   bartik_inst(od_05, od_10),
#   bartik_inst(od_10, od_15)
# ) %>%
#   inner_join(forest_df)
# 
# od_flows <- bind_rows(od_00, od_05, od_10, od_15)
# saveRDS( od_flows, paste0(saf.dir, "Migration/od_migration.rds"))

  