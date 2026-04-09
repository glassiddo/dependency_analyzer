#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    March 12, 2023
#* Title:   Forest Loss
#* Desc:    Build Forest Loss Dataset for each admin unit that we use
#* 1 hectare = 10,000 sq m = 1x10^4 sq m
#*         1 km^2 = 100 hectare = 1x10^6 sq m
#*         New countries: Cameroon, Malawi, Rwanda, South Sudan, Sudan, Togo, Zimbabwe
#*******************************************************************************
#* 
# Set Up ----
source("code/SSA_env_SetUp.R")

cuts <- c("00", "10", "20", "30", "40", "50", "60", "70", "80", "90")

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country)

all_datasets <- list()  # Initialize an empty list to store datasets

#*******************************************************************************
#*
# First Period ----

w <- 1

for (t in cuts) {
  fl_df <- readRDS(here(
    build.dir, "Africa", "Forest", "Cuts", paste0("loss", t ,"cut.rds")
  )) 

  fl_yrs <- readRDS(here(
    build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", w, ".rds")
    ))
  
  new_obs <- fl_df %>%
    filter(year == 2001) %>%  # Select Year 2001
    mutate(year = 2000)       # Change to Year 2000
  
  # Bind the new observations to the original dataset
  fl_df_new <- bind_rows(fl_df, new_obs) %>%
    arrange(ipums_id, year)  
  
  # Ensure the dataset is sorted
  fl_df_new <- fl_df_new %>%
    arrange(ipums_id, year) %>%  # Sort by ipums_id and year
    group_by(ipums_id) %>%
    mutate(treecover_new = NA) %>%  # Create an empty column
    ungroup()
  
  # Set treecover_new for Year 2000
  fl_df_new <- fl_df_new %>%
    mutate(treecover_new = ifelse(year == 2000, treecover, treecover_new))
  
  years <- sort(unique(fl_df_new$year))
  
  # Loop over years and update treecover_new
  for (y in years[-1]) {  # Start from the second year (2001)
    fl_df_new <- fl_df_new %>%
      group_by(ipums_id) %>%
      mutate(treecover_new = ifelse(
        year == y, 
        treecover_new[year == y - 1] - loss[year == y], 
        treecover_new)
      ) %>%
      ungroup()
  }
  
  fl_df_new <- fl_df_new %>% 
    select(ipums_id, year, treecover, treecover_new)
  
  var_name <- paste0("treecover_new_", t)  # Construct variable name
  
  fl_df_new <- fl_df_new %>%
    rename(!!var_name := treecover_new)  # Dynamically rename column
  
  
  var_name <- paste0("treecover_", t)  # Construct variable name
  
  fl_df_new <- fl_df_new %>%
    rename(!!var_name := treecover)  # Dynamically rename column
  
  samp_df <- inner_join(fl_df, id_df, by = "ipums_id") %>% 
    left_join(fl_yrs, by = "country")
  
  # baseline treecover
  bl_df <- samp_df %>% 
    filter(year < fl_start_yr) %>%
    group_by(country, ipums_id) %>%
    summarise(
      across(c(treecover), ~mean(.x)),
      across(c(loss), ~sum(.x)),
      .groups = 'drop') %>%
    mutate(bl_treecover = ifelse(treecover - loss > 0, treecover - loss, 0)) %>%
    select(country, ipums_id, bl_treecover) %>%
    # if first year is 2001, no forest loss before period
    bind_rows(
      samp_df %>% filter(fl_start_yr == 2001) %>% 
        select(country, ipums_id, bl_treecover = treecover) %>% 
        unique() 
    )
  
  samp_df %<>%
    filter(year >= fl_start_yr & year <= fl_end_yr) %>%
    mutate(log_loss = ifelse(loss != 0, log(loss), 0)) %>%
    group_by(country, ipums_id) %>%
    summarise(
      # 1. average log loss
      avg_log_loss = mean(log_loss),
      # 2. average loss
      avg_loss = mean(loss),
      # 3. log total loss
      total_loss = sum(loss),
      # 4. loss using simple measure
      treecover = mean(treecover), 
      period_len = mean(fl_period_len),
      fl_start_yr = mean(fl_start_yr),
      fl_end_yr = mean(fl_end_yr),
      .groups = 'drop') %>% 
    full_join(bl_df, by = c("ipums_id", "country")) %>%
    mutate(
      pct_loss = ifelse(bl_treecover != 0, total_loss / bl_treecover, NA),
      ann_pct_loss = ifelse(is.na(pct_loss) == FALSE,
                            pct_loss / period_len, NA),
      delta_log_loss = ifelse(
        bl_treecover != 0 & bl_treecover - total_loss > 0,
        log(bl_treecover) - log(bl_treecover - total_loss), NA),
      # give 1 to places with no loss and take logs
      across(c(avg_loss, total_loss), 
             ~ifelse(.x == 0, 0, log(.x)), .names = "log_{.col}")) %>%
    var_labels(
      fl_start_yr = "forest loss period start year",
      fl_end_yr = "forest loss period end year",
      avg_log_loss = "mean annual log forest loss, log-hectares",
      avg_loss = "mean annual forest loss, hectares",
      total_loss = "total forest loss during period, hectares",
      treecover = "tree cover in 2000, hectares",
      bl_treecover = "tree cover at start of period, hectares",
      pct_loss = "share of baseline treecover lost",
      ann_pct_loss = "share of baseline treecover lost per year",
      delta_log_loss = "log baseline treecover - log endline treecover",
      log_avg_loss = "log mean-annual forest loss, log-hectares",
      log_total_loss = "log total forest loss during period, log-hectares") %>%
    select(country, ipums_id, avg_log_loss, avg_loss, total_loss,
           treecover, bl_treecover, pct_loss, ann_pct_loss, delta_log_loss,
           log_avg_loss, log_total_loss, fl_start_yr, fl_end_yr) %>%
    rename_with(~paste0(.x, '_t',t), 
                !c(country, ipums_id, fl_start_yr, fl_end_yr))
  
  if (t == '00') {fl_wide <- samp_df}
  else { fl_wide %<>% full_join(samp_df)}
  
  # Store dataset in a list for merging later
  all_datasets[[var_name]] <- fl_df_new
  
}

saveRDS(fl_wide, here(out.dir, "R", "forestloss_cuts.rds"))
write_dta(fl_wide, here(out.dir, "forestloss_cuts.dta"))

#*******************************************************************************
# Second Period ----

w <- 2
for (t in cuts) {
  
  fl_df <- readRDS(here(
    build.dir, "Africa", "Forest", "Cuts", paste0("loss", t ,"cut.rds")
  )) 

  fl_yrs <- readRDS(here(
    build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", w, ".rds")
  ))
  
  samp_df <- inner_join(fl_df, id_df, by = "ipums_id") %>% 
    left_join(fl_yrs, by = "country") 

  # baseline treecover
  bl_df <- samp_df %>%
    filter(year < fl_start_yr) %>%
    group_by(country, ipums_id) %>%
    summarise(
      across(c(treecover), ~mean(.x)),
      across(c(loss), ~sum(.x)),
      .groups = 'drop') %>%
    mutate(bl_treecover = ifelse(treecover - loss > 0, treecover - loss, 0)) %>%
    select(country, ipums_id, bl_treecover) %>%
    # if first year is 2001, no forest loss before period
    bind_rows(
      samp_df %>% filter(fl_start_yr == 2001) %>% 
        select(country, ipums_id, bl_treecover = treecover) %>% 
        unique() 
    )
  
  samp_df %<>%
    filter(year >= fl_start_yr & year <= fl_end_yr) %>%
    mutate(log_loss = ifelse(loss != 0, log(loss), 0)) %>%
    group_by(country, ipums_id) %>%
    summarise(
      # 1. average log loss
      avg_log_loss = mean(log_loss),
      # 2. average loss
      avg_loss = mean(loss),
      # 3. log total loss
      total_loss = sum(loss),
      # 4. loss using simple measure
      treecover = mean(treecover), 
      period_len = mean(fl_period_len),
      fl_start_yr = mean(fl_start_yr),
      fl_end_yr = mean(fl_end_yr),
      .groups = 'drop') %>%
    full_join(bl_df, by = c("ipums_id", "country")) %>%
    mutate(
      pct_loss = ifelse(bl_treecover != 0, total_loss / bl_treecover, NA),
      ann_pct_loss = ifelse(is.na(pct_loss) == FALSE,
                            pct_loss / period_len, NA),
      delta_log_loss = ifelse(bl_treecover != 0 & bl_treecover - total_loss > 0,
                              log(bl_treecover) - log(bl_treecover - total_loss), NA),
      # give 1 to places with no loss and take logs
      across(c(avg_loss, total_loss), 
             ~ifelse(.x == 0, 0, log(.x)), .names = "log_{.col}")) %>%
    var_labels(
      fl_start_yr = "forest loss period start year",
      fl_end_yr = "forest loss period end year",
      avg_log_loss = "mean annual log forest loss, log-hectares",
      avg_loss = "mean annual forest loss, hectares",
      total_loss = "total forest loss during period, hectares",
      treecover = "tree cover in 2000, hectares",
      bl_treecover = "tree cover at start of period, hectares",
      pct_loss = "share of baseline treecover lost",
      ann_pct_loss = "share of baseline treecover lost per year",
      delta_log_loss = "log baseline treecover - log endline treecover",
      log_avg_loss = "log mean-annual forest loss, log-hectares",
      log_total_loss = "log total forest loss during period, log-hectares") %>%
    select(country, ipums_id, avg_log_loss, avg_loss, total_loss,
           treecover, bl_treecover, pct_loss, ann_pct_loss, delta_log_loss,
           log_avg_loss, log_total_loss, fl_start_yr, fl_end_yr) %>%
    rename_with(~paste0(.x, '_t',t), 
                !c(country, ipums_id, fl_start_yr, fl_end_yr))
  
  if (t == '00') {fl_wide <- samp_df}
  else { fl_wide %<>% full_join(samp_df)}
  
}

saveRDS(fl_wide, here(out.dir, "R", "forestloss_cuts_period2.rds"))
write_dta(fl_wide, here(out.dir, "forestloss_cuts_period2.dta"))
