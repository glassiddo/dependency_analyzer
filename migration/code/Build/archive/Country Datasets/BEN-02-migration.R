# Project: Migration Africa
# Author:  Sam Marshall
# Created: Jan 20, 2022
# Title:   Benin Migration Panel
# Output:  BEN_5yr_flows.rds
# Notes:   2002 is the whole thing, 2013 is a 10% sample  
# commune_before codes: 
# 997 = international, 998 = I don't know, 999 = "not specified", NA is just missing
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")
source("code/Functions/F-BEN.R")
source("code/Functions/F-ts.R")
source("code/Functions/F-Forest.R")

benin13_df <- readRDS(paste0(build.dir, "Benin/Census/census13.rds")) %>%
  filter(prime_age == 1)
benin02_df <- readRDS(paste0(build.dir, "Benin/Census/census02.rds")) %>%
  filter(prime_age == 1)

cover_df <- readRDS(paste0(build.dir, "Benin/Forest Cover/forest_cover.rds"))
distance_df <- readRDS(paste0(build.dir, "Benin/Support/district_distance.rds"))
pop_df <- readRDS(paste0(build.dir, "Benin/Support/population.rds"))

#*******************************************************************************
# 1. five-year migration flows ----
#*******************************************************************************

ben13_ann <- mig_flow(benin13_df, "district", 1993, smp = TRUE)
ben02_ann <- mig_flow(benin02_df, "district", 1982, smp = FALSE)

mig5y_df <- bind_rows(
  mig_agg(ben02_ann, 1992, 1996),
  mig_agg(ben02_ann, 1997, 2001),
  mig_agg(ben13_ann, 2003, 2007),
  mig_agg(ben13_ann, 2008, 2012)
) 

saveRDS( ben13_ann, paste0(ben.dir, "Migration/Cens13_migration_annual.rds"))
saveRDS( ben02_ann, paste0(ben.dir, "Migration/Cens02_migration_annual.rds"))
saveRDS( mig5y_df, paste0(ben.dir, "Migration/migration_5yr.rds"))

#*******************************************************************************
# 2. Specific location migration flows ----
#*******************************************************************************

dist_set <- benin13_df %>% select(district, district_name) %>% unique()
miss_dist <- bind_rows(dist_set %>% mutate(range = "1992-1996"),
                       dist_set %>% mutate(range = "1997-2001"),
                       dist_set %>% mutate(range = "2003-2007"),
                       dist_set %>% mutate(range = "2008-2012"))

# Kopargo = 73 (high out migration)
kop_df <- district_flows(73, immigration = FALSE)
cot_df <- district_flows(81)
ac_df <- district_flows(31)
pk_df <- district_flows(45)

saveRDS( kop_df, paste0(ben.dir, "Migration/Kopargo_emigration.rds"))
saveRDS( cot_df, paste0(ben.dir, "Migration/Cotonou_immigration.rds"))
saveRDS( ac_df, paste0(ben.dir, "Migration/Abomey_Calavi_immigration.rds"))
saveRDS( pk_df, paste0(ben.dir, "Migration/Parakou_immigration.rds"))

#*******************************************************************************
# 3. Emigration Flows out of high forest loss areas ----
#*******************************************************************************
# check for high forest loss areas
forest_df <- readRDS( paste0(ben.dir, "Forest Cover/loss_5yr.rds")) 
# temp_df <- left_join(forest_df, dist_set) %>% filter(year != 2020) %>%
#   arrange(-ln_loss_area)

emig_df <- data.frame()

for(d in c(48, 16, 12, 106, 37)) {
  emig_df <- bind_rows(emig_df, district_flows(d, immigration = FALSE))
}

saveRDS( emig_df, paste0(ben.dir, "Migration/forest_loss_emigration.rds"))

#*******************************************************************************
# 4. Inter-region migration flows ----
#*******************************************************************************

ben_reg <- bind_rows(
  mig_flow(benin13_df, "region", 2002, smp = TRUE),
  mig_flow(benin02_df, "region", 1991, smp = FALSE)
)

mig5y_df <- bind_rows(
  mig_agg(ben_reg, 1992, 1996),
  mig_agg(ben_reg, 1997, 2001),
  mig_agg(ben_reg, 2003, 2007),
  mig_agg(ben_reg, 2008, 2012)
) %>%
  left_join(cover_df %>% select(district_name, area) %>% unique()) %>%
  mutate(log_emigration_flow_area = log_emigration_flow - log(area))

saveRDS( mig5y_df, paste0(ben.dir, "Migration/regional_migration_5yr.rds"))

#*******************************************************************************
# 5. Migration Time Series ----
#*******************************************************************************

base_pop_df <- pop_df %>%
  mutate(growth_rate = (pop_02 / pop_92)^(1/10) - 1,
         pop_00 = pop_02 / (1 + growth_rate)^2 ) %>%
  select(district, district_name, urban, urban_02, pop_00)

ben_ts <- bind_rows(mig_ts(ben02_ann, 1992, 2001),
                    mig_ts(ben13_ann, 2002, 2012))

saveRDS( ben_ts, paste0(ben.dir, "Migration/migration_timeseries.rds"))

#*******************************************************************************
# 6. Origin-destination migration flows ----
#*******************************************************************************

# get tree loss for each period
ann_treeloss <- readRDS(paste0(ben.dir, "Forest Cover/loss_ann.rds"))
loss_03_12 <- f_custom_loss(ann_treeloss, district_name, 2003, 2012, avg = TRUE)
lag_loss <- f_custom_loss(ann_treeloss, district_name, 2001, 2002, avg = TRUE) %>%
  select(district_name, log_loss) %>%
  rename(lag_log_loss = log_loss)

loss30_df <- readRDS(paste0(ben.dir, "Forest Cover/loss30.rds")) %>%
  select(district_name, loss) %>%
  rename(loss30 = loss) %>%
  mutate(log_loss30 = log((loss30+1) / 20))
# loss going five years back and five years foward with overlap
fb_loss <- full_join(
  f_custom_loss(ann_treeloss, district_name, 2001, 2007, avg = TRUE) %>%
    select(district_name, log_loss) %>%
    rename(start_log_loss = log_loss),
  f_custom_loss(ann_treeloss, district_name, 2008, 2017, avg = TRUE) %>%
    select(district_name, log_loss) %>%
    rename(end_log_loss = log_loss) 
)

# origin destination flows for each period
od_92_01 <- od_mig_flow(benin02_df, 1992, 2001)
od_03_12 <- od_mig_flow(benin13_df, 2003, 2012)

Bartik_df <- bartik_inst(od_92_01, od_03_12, loss_03_12) %>%
  full_join(lag_loss) %>%
  full_join(fb_loss) %>%
  full_join(loss30_df)

saveRDS( Bartik_df, paste0(ben.dir, "Migration/Bartik_migration_03_12.rds"))
write_dta(Bartik_df, file.path(ben.dir, "Migration/Bartik_migration_03_12.dta"))


