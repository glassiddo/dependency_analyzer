#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    March 12, 2023
#* Title:   Forest Loss
#* Desc:    Build Forest Loss Dataset for each admin unit that we use
#* 1 hectare = 10,000 sq m = 1x10^4 sq m
#*         1 km^2 = 100 hectare = 1x10^6 sq m
#*         New countries: Cameroon, Malawi, Rwanda, South Sudan, Sudan, Togo, Zimbabwe
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

fl_df <- readRDS(paste0(build.dir, 'Africa/Sample Aggregates/loss30c.rds')) %>% 
  select(-geolevel1, -geolevel2) %>% 
  filter(
    ipums_id %!in% c("888888", "646008088") # remove lake malawi and kivu 
  )
  
id_df <- readRDS(file.path(out.dir, "R/id.rds")) 

#*******************************************************************************
# Main File ----
samp_df <- inner_join(fl_df, id_df, by = c("ipums_id", "ctry_id")) %>%
  mutate(yr_start = ifelse(census_yr1 >= 2000, census_yr1 + 1, 2001),
         yr_end = ifelse(is.na(census_yr2) == FALSE, census_yr2, NA),
         period_len = yr_end - yr_start + 1)

# baseline treecover
bl_df <- samp_df %>% filter(year < yr_start) %>%
  group_by(ctry_id, ipums_id) %>%
  summarise(
    across(c(treecover, treecover_simple), ~mean(.x)),
    across(c(loss, loss_simple), ~sum(.x)),
    .groups = 'drop') %>%
  mutate(bl_treecover = ifelse(treecover - loss > 0, treecover - loss, 0),
         bl_treecover_simple = treecover_simple - loss_simple) %>%
  select(ctry_id, ipums_id, bl_treecover, bl_treecover_simple) %>%
  # if first year is 2001, no forest loss before period
  bind_rows(
    samp_df %>% filter(yr_start == 2001) %>% 
      select(ctry_id, ipums_id, bl_treecover = treecover, 
             bl_treecover_simple = treecover_simple) %>% 
      unique() 
  )

samp_df %<>%
  filter(year >= yr_start & year <= yr_end) %>%
  mutate(log_loss = ifelse(loss != 0, log(loss), 0)) %>%
  group_by(ctry_id, ipums_id) %>%
  summarise(
    # 1. average log loss
    avg_log_loss = mean(log_loss),
    # 2. average loss
    avg_loss = mean(loss),
    # 3. log total loss
    total_loss = sum(loss),
    # 4. loss using simple measure
    avg_loss_simple = mean(loss_simple),
    treecover = mean(treecover), 
    treecover_simple = mean(treecover_simple), 
    fl_start_yr = mean(yr_start),
    fl_end_yr = mean(yr_end),
    fl_prd_len = mean(period_len),
    .groups = 'drop') %>%
  full_join(bl_df) %>%
  mutate(
    pct_loss = ifelse(bl_treecover != 0, total_loss / bl_treecover, NA),
    ann_pct_loss = ifelse(is.na(pct_loss) == FALSE,
                          pct_loss / fl_prd_len, NA),
    delta_log_loss = ifelse(bl_treecover != 0 & bl_treecover - total_loss > 0,
                            log(bl_treecover) - log(bl_treecover - total_loss), NA),
    # give 1 to places with no loss and take logs
    across(c(avg_loss, total_loss), 
           ~ifelse(.x == 0, 0, log(.x)), .names = "log_{.col}")) %>%
  inner_join(id_df) %>%
  relocate(ctry_id, ipums_id, admin_name, admin_id, geo_lvl, 
           fl_start_yr, fl_end_yr, fl_prd_len) %>%
  #relocate(district_name, region_name, .after = last_col()) %>%
  var_labels(
    avg_log_loss = "mean annual log forest loss, log-hectares",
    avg_loss = "mean annual forest loss, hectares",
    total_loss = "total forest loss during period, hectares",
    avg_loss_simple = "mean annual forest loss, simple measure hectares",
    treecover = "tree cover in 2000, hectares",
    treecover_simple = "tree cover in 2000, simple measure hectares",
    bl_treecover = "tree cover at start of period, hectares",
    bl_treecover_simple = "tree cover at start of period, simple measure hectares",
    pct_loss = "share of baseline treecover lost",
    ann_pct_loss = "share of baseline treecover lost per year",
    delta_log_loss = "log baseline treecover - log endline treecover",
    log_avg_loss = "log mean-annual forest loss, log-hectares",
    log_total_loss = "log total forest loss during period, log-hectares",
    fl_start_yr = "forest loss period start year",
    fl_end_yr = "forest loss period end year",
    fl_prd_len = "forest loss period length")


saveRDS(samp_df, file.path(out.dir, "R/forestloss.rds"))
write_dta(samp_df, file.path(out.dir, "forestloss.dta"))

#*******************************************************************************
# Second Period ----

post_df <- inner_join(fl_df, id_df) %>%
  mutate(yr_start = case_when(
    is.na(census_yr2) == TRUE ~ census_yr1 + 1,
    census_yr2 <= 2000 ~ 2001,
    TRUE ~ census_yr2 + 1
  ),
  yr_end = 2020,
  period_len = yr_end - yr_start + 1)

# baseline treecover
bl_df <- post_df %>% filter(year < yr_start) %>%
  group_by(ctry_id, ipums_id) %>%
  summarise(
    across(c(treecover, treecover_simple), ~mean(.x)),
    across(c(loss, loss_simple), ~sum(.x)),
    .groups = 'drop') %>%
  mutate(bl_treecover = ifelse(treecover - loss > 0, treecover - loss, 0),
         bl_treecover_simple = treecover_simple - loss_simple) %>%
  select(ctry_id, ipums_id, bl_treecover, bl_treecover_simple) 

post_df %<>%
  filter(year >= yr_start & year <= yr_end) %>%
  mutate(log_loss = ifelse(loss != 0, log(loss), 0)) %>%
  group_by(ctry_id, ipums_id) %>%
  summarise(
    # 1. average log loss
    avg_log_loss = mean(log_loss),
    # 2. average loss
    avg_loss = mean(loss),
    # 3. log total loss
    total_loss = sum(loss),
    # 4. loss using simple measure
    avg_loss_simple = mean(loss_simple),
    treecover = mean(treecover), 
    treecover_simple = mean(treecover_simple), 
    fl_start_yr = mean(yr_start),
    fl_end_yr = mean(yr_end),
    fl_prd_len = mean(period_len),
    .groups = 'drop') %>%
  full_join(bl_df) %>%
  mutate(
    pct_loss = ifelse(bl_treecover != 0, total_loss / bl_treecover, NA),
    ann_pct_loss = ifelse(is.na(pct_loss) == FALSE,
                          pct_loss / fl_prd_len, NA),
    delta_log_loss = ifelse(bl_treecover != 0 & bl_treecover - total_loss > 0,
                            log(bl_treecover) - log(bl_treecover - total_loss), NA),
    # give 1 to places with no loss and take logs
    across(c(avg_loss, total_loss), 
           ~ifelse(.x == 0, 0, log(.x)), .names = "log_{.col}")) %>%
  inner_join(id_df) %>%
  relocate(ctry_id, ipums_id, admin_name, admin_id, geo_lvl, 
           fl_start_yr, fl_end_yr, fl_prd_len) %>%
  #relocate(district_name, region_name, .after = last_col()) %>%
  var_labels(
    avg_log_loss = "mean annual log forest loss, log-hectares",
    avg_loss = "mean annual forest loss, hectares",
    total_loss = "total forest loss during period, hectares",
    avg_loss_simple = "mean annual forest loss, simple measure hectares",
    treecover = "tree cover in 2000, hectares",
    treecover_simple = "tree cover in 2000, simple measure hectares",
    bl_treecover = "tree cover at start of period, hectares",
    bl_treecover_simple = "tree cover at start of period, simple measure hectares",
    pct_loss = "share of baseline treecover lost",
    ann_pct_loss = "share of baseline treecover lost per year",
    delta_log_loss = "log baseline treecover - log endline treecover",
    log_avg_loss = "log mean-annual forest loss, log-hectares",
    log_total_loss = "log total forest loss during period, log-hectares",
    fl_start_yr = "forest loss period start year",
    fl_end_yr = "forest loss period end year",
    fl_prd_len = "forest loss period length")


saveRDS(post_df, file.path(out.dir, "R/forestloss_period2.rds"))
write_dta(post_df, file.path(out.dir, "forestloss_period2.dta"))
