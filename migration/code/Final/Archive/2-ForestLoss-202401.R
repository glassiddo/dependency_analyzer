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

#f_forestloss <- function( ctry_str, lvl, yr_start, yr_end) {
f_forestloss <- function( ctry_str, lvl) {  
  
  dat_df <- readRDS(paste0(build.dir,ctry_str, "/Forest Cover/l",lvl,"_loss30c.rds"))
  
  ctry3 <- g_ctry3(ctry_str)
  census_yrs <- g_cens_yr(ctry3)
  yr_start <- max(census_yrs[1] + 1, 2001)
  yr_end <- min(census_yrs[2], 2020, na.rm = TRUE)
  
  period_len <- yr_end - yr_start + 1
  
  if (lvl == 2) { geo <- sym("district_name") }
  else { geo <- sym("region_name") }
  
  ipums <- sym(paste0("geolevel", lvl))
  
  # get baseline forest cover
  if (yr_start > 2001) {
    bl_df <- dat_df %>%
      filter(year < yr_start) %>%
      group_by({{geo}}) %>%
      summarise(
        across(c(treecover, treecover_simple), ~mean(.x)),
        across(c(loss, loss_simple), ~sum(.x)),
        .groups = 'drop') %>%
      mutate(bl_treecover = ifelse(treecover - loss > 0, treecover - loss, 0),
             bl_treecover_simple = treecover_simple - loss_simple) %>%
      select({{geo}}, bl_treecover, bl_treecover_simple)
  }
  # if first year is 2001, no forest loss before period
  else {
    bl_df <- dat_df %>% select({{geo}}, treecover, treecover_simple) %>% 
      unique() %>%
      rename(bl_treecover = treecover, bl_treecover_simple = treecover_simple)
  }
  
  out_df <- dat_df %>%
    filter(year >= yr_start & year <= yr_end) %>%
    mutate(ipums_id = as.character({{ipums}})) %>%
    mutate(log_loss = ifelse(loss != 0, log(loss), 0)) %>%
    group_by({{geo}}, ipums_id) %>%
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
      .groups = 'drop') %>%
    full_join(bl_df) %>%
    mutate(
      pct_loss = ifelse(bl_treecover != 0, total_loss / bl_treecover, NA),
      ann_pct_loss = ifelse(is.na(pct_loss) == FALSE,
                            pct_loss / period_len, NA),
      delta_log_loss = ifelse(bl_treecover != 0 & bl_treecover - total_loss > 0,
                              log(bl_treecover) - log(bl_treecover - total_loss), NA),
      # give 1 to places with no loss and take logs
      across(c(avg_loss, total_loss), 
             ~ifelse(.x == 0, 0, log(.x)), .names = "log_{.col}"),
      country_name = ctry_str,
      fl_start_yr = yr_start,
      fl_end_yr = yr_end,
      fl_prd_len = period_len,) 
}

#*******************************************************************************
# Benin 2 ----
ctry_df <- f_forestloss("Benin", 2)
loss_df <- ctry_df

# Botswana 1 ----
ctry_df <- f_forestloss("Botswana", 1) 
loss_df <- bind_rows(loss_df, ctry_df)

# Burkina Faso ----
ctry_df <- f_forestloss("Burkina Faso", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# Cameroon ----
ctry_df <- f_forestloss("Cameroon", 2)   
loss_df <- bind_rows(loss_df, ctry_df)

# Ghana ----
ctry_df <- f_forestloss("Ghana", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# Guinea ----
ctry_df <- f_forestloss("Guinea", 1) 
loss_df <- bind_rows(loss_df, ctry_df)

# Kenya ----
ctry_df <- f_forestloss("Kenya", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# Lesotho ----
ctry_df <- f_forestloss("Lesotho", 1) 
loss_df <- bind_rows(loss_df, ctry_df)

# Malawi ----
ctry_df <- f_forestloss("Malawi", 1) 
loss_df <- bind_rows(loss_df, ctry_df)

# Mali ----
ctry_df <- f_forestloss("Mali", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# Mauritius ----
ctry_df <- f_forestloss("Mauritius", 1) 
loss_df <- bind_rows(loss_df, ctry_df)

# Mozambique ----
ctry_df <- f_forestloss("Mozambique", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# Rwanda ----
ctry_df <- f_forestloss("Rwanda", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# Senegal ----
ctry_df <- f_forestloss("Senegal", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# Sierra Leone ----
ctry_df <- f_forestloss("Sierra Leone", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# South Africa ----
ctry_df <- f_forestloss("South Africa", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# Sudan & S. Sudan ----
ctry_df <- f_forestloss("Sudan", 1) 
loss_df <- bind_rows(loss_df, ctry_df)

# Tanzania ----
ctry_df <- f_forestloss("Tanzania", 1) 
loss_df <- bind_rows(loss_df, ctry_df)

# Togo ----
ctry_df <- f_forestloss("Togo", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# Uganda ----
ctry_df <- f_forestloss("Uganda", 1) 
loss_df <- bind_rows(loss_df, ctry_df)

# Zambia ----
ctry_df <- f_forestloss("Zambia", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# Zimbabwe ----
ctry_df <- f_forestloss("Zimbabwe", 2) 
loss_df <- bind_rows(loss_df, ctry_df)

# save ----
id_df <- readRDS(file.path(out.dir, "R/id.rds"))

loss_df %<>%
  inner_join(id_df) %>%
  relocate(country_name, ipums_id, admin_name, admin_id, geo_lvl, 
           fl_start_yr, fl_end_yr, fl_prd_len) %>%
  relocate(district_name, region_name, .after = last_col()) %>%
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


saveRDS(loss_df, file.path(out.dir, "R/forestloss.rds"))
write_dta(loss_df, file.path(out.dir, "forestloss.dta"))


# temp_df <- loss_df %>% filter(str_sub(region_name,1,4) == "Lake")

