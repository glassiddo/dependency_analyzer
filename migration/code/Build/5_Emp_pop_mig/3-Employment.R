#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    April 1, 2023
#* Title:   Employment
#* Desc:    Build employment Dataset for each admin unit that we use
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

f_empl <- function(ctry_str) {
  print(ctry_str)
  #* create the following (+ naming convention)
  #* 1. baseline and endline employment, primeage if set so (bl, el)
  #* 2. baseline and endline ag, non-ag, mining and unemployed
  #* 3. delta empl, delta log empl, and annualized 
  
  census_data <- census_info %>% 
    filter(country == ctry_str) %>%
    select(census_yr1, census_yr2, lvl, prime_age)
  
  yr_start <- census_data$census_yr1
  yr_end <- census_data$census_yr2
  lvl <- census_data$lvl
  prime_age <- census_data$prime_age
  
  period_len <- yr_end - yr_start
  
  # get the last two digits for reading files
  yr1 <- str_sub(as.character(yr_start), -2)
  yr2 <- str_sub(as.character(yr_end), -2)

  # read censuses
  start_df <- readRDS(here(
    build.dir, "Countries", ctry_str, "Census", paste0("census", yr1, ".rds")
    ))
  
  end_df <- readRDS(here(
    build.dir, "Countries", ctry_str, "Census", paste0("census", yr2, ".rds")
    ))
  
  if(prime_age == TRUE) {
    start_df <- start_df %>% filter(age >= min_pa & age <= max_pa)
    end_df <- end_df %>% filter(age >= min_pa & age <= max_pa)
  }
  
  geo <- g_sym(lvl)
  ipums <- sym(paste0("geo", lvl))
  
  # population in first period
  empl0 <- start_df %>% 
    rename_with(~"ipums_id", starts_with(paste0("geo", lvl)) & 
                  !ends_with(c("0","1","2","3","4","5","6","7","8","9")) ) %>%
    mutate(
      ipums_id = as.character(ipums_id)
      ) %>%
    drop_na(wgt) %>%
    group_by(ipums_id) %>% 
    summarise(
      # removing all of the na.rm = TRUE
      bl_nonag = sum(wgt * employed * (1 - empl_ag), na.rm = T),
      bl_ag = sum(wgt * empl_ag, na.rm = T),
      bl_mining = sum(wgt * empl_mining, na.rm = T),
      bl_nonmining = sum(wgt * employed * (1 - empl_mining), na.rm = T),
      bl_empl = sum(wgt * employed, na.rm = T),
      bl_u = sum(wgt * (1 - employed), na.rm = T),
      # sectors
      bl_mfg = sum(wgt * empl_mfg, na.rm = T),
      bl_svc = sum(wgt * empl_svc, na.rm = T),
      #bl_trd = sum(wgt * empl_trd, na.rm = T),
      #bl_ntrd = sum(wgt * empl_ntrd, na.rm = T),
      # changes in urban shares
      bl_urban_emp = sum(wgt * employed * urban, na.rm = TRUE),
      .groups = 'drop') 
  
  # population in o and d in second period
  empl1 <- end_df %>% 
    rename_with(~"ipums_id", starts_with(paste0("geo", lvl)) & 
                  !ends_with(c("0","1","2","3","4","5","6","7","8","9")) ) %>%
    # this needs to be one instead of weight and then update elsewhere
    mutate(
      ipums_id = as.character(ipums_id)
      ) %>%
    drop_na(wgt) %>%
    #group_by({{geo}}) %>% 
    group_by(ipums_id) %>%
    summarise(
      el_nonag = sum(wgt * employed * (1 - empl_ag), na.rm = T),
      el_ag = sum(wgt * empl_ag, na.rm = T),
      el_mining = sum(wgt * empl_mining, na.rm = T),
      el_nonmining = sum(wgt * employed * (1 - empl_mining), na.rm = T),
      el_empl = sum(wgt * employed, na.rm = T),
      el_u = sum(wgt * (1 - employed), na.rm = T),
      # sectors
      el_mfg = sum(wgt * empl_mfg, na.rm = T),
      el_svc = sum(wgt * empl_svc, na.rm = T),
      #el_trd = sum(wgt * empl_trd, na.rm = T),
      #el_ntrd = sum(wgt * empl_ntrd, na.rm = T),
      # changes in urban shares
      el_urban_emp = sum(wgt * employed * urban, na.rm = TRUE),
      .groups = 'drop'
      )
  
  empl <- full_join(empl0, empl1, by = "ipums_id") %>%
    mutate(
      country = ctry_str,
      empl_start_yr = yr_start, 
      empl_end_yr = yr_end,
      empl_prd_len = period_len,
      # empl change
      delta_empl = el_empl - bl_empl,
      delta_empl_ann = safe_divide(delta_empl,period_len),
      delta_log_empl = log(el_empl) - log(bl_empl),
      delta_log_empl_ann = safe_divide(delta_log_empl,period_len),
      # non-ag
      delta_nonag = el_nonag - bl_nonag,
      delta_nonag_ann = safe_divide(delta_nonag,period_len),
      delta_log_nonag = log(el_nonag) - log(bl_nonag),
      delta_log_nonag_ann = safe_divide(delta_log_nonag,period_len),
      # ag
      delta_ag = el_ag - bl_ag,
      delta_ag_ann = safe_divide(delta_ag,period_len),
      delta_log_ag = log(el_ag) - log(bl_ag),
      delta_log_ag_ann = safe_divide(delta_log_ag,period_len),
      # mining
      delta_mining = el_mining - bl_mining,
      delta_mining_ann = safe_divide(delta_mining,period_len),
      delta_log_mining = log(el_mining) - log(bl_mining),
      delta_log_mining_ann = safe_divide(delta_log_mining,period_len),
      # change in urban employment
      delta_urban_emp = el_urban_emp - bl_urban_emp,
      delta_urban_emp_ann = safe_divide(delta_urban_emp,period_len),
      # change in logs
      delta_log_urban_emp = ifelse(
        el_urban_emp != 0 & bl_urban_emp != 0,
        log(el_urban_emp) - log(bl_urban_emp), NA),
      delta_log_urban_emp_ann = safe_divide(delta_log_urban_emp,period_len),
      # change in shares
      delta_urban_emp_share = 
        safe_divide(el_urban_emp,el_empl) - safe_divide(bl_urban_emp,bl_empl),
      # clean inf
      across(starts_with("delta_log"), 
             ~ifelse(.x %in% c(-Inf, NaN, Inf), NA, .x))
      )
  
}

# create baseline values for countries with one census
f_empl1 <- function(ctry_str) {
  print(ctry_str)
  #* create the following (+ naming convention)
  #* 1. baseline and endline employment, primeage if set so (bl, el)
  #* 2. baseline and endline ag, non-ag, mining and unemployed
  #* 3. delta empl, delta log empl, and annualized 
  
  census_data <- census_info %>% 
    filter(country == ctry_str) %>%
    remove_empty(which = "cols") %>%  # remove the empty column
    rename(
      census_yr = #if(ctry_str == "MDG") "yr2_suffix" else 
        any_of(c("yr1_suffix", "yr2_suffix"))
    ) %>% # unique condition for MDG [removed]
    select(census_yr, lvl, prime_age)
  
  yr <- census_data$census_yr
  lvl <- census_data$lvl
  prime_age <- census_data$prime_age  
  
  # read census
  df <- readRDS(here(
    build.dir, "Countries", ctry_str, "Census", paste0("census", yr, ".rds")
    ))
  
  if(prime_age == TRUE) {
    df <- df %>% filter(age >= min_pa & age <= max_pa)
  }
  
  geo <- g_sym(lvl)
  ipums <- sym(paste0("geo", lvl))
  
  prefix <- if(ctry_str %in% single_first_cens_ctries) "bl" else "el"

  # population in first period
  empl <- df %>% 
    rename_with(~"ipums_id", starts_with(paste0("geo", lvl)) & 
                  !ends_with(c("0","1","2","3","4","5","6","7","8","9")) ) %>%
    mutate(
      ipums_id = as.character(ipums_id)
      ) %>%
    drop_na(wgt) %>%
    # group_by({{geo}}, ipums_id) %>% 
    group_by(ipums_id) %>%
    summarise(
      # in case someone has missing weight
      "{prefix}_nonag" := sum(wgt * employed * (1 - empl_ag), na.rm = T),
      "{prefix}_ag" := sum(wgt * employed * empl_ag, na.rm = T),
      "{prefix}_mining" := sum(wgt * empl_mining, na.rm = T),
      "{prefix}_nonmining" := sum(wgt * employed * (1 - empl_mining), na.rm = T),
      "{prefix}_empl" := sum(wgt * employed, na.rm = T),
      "{prefix}_u" := sum(wgt * (1 - employed), na.rm = T),
      # sectors
      "{prefix}_mfg" := sum(wgt * empl_mfg, na.rm = T),
      "{prefix}_svc" := sum(wgt * empl_svc, na.rm = T),
      #"{prefix}_trd" := sum(wgt * empl_trd, na.rm = T),
      #"{prefix}_ntrd" := sum(wgt * empl_ntrd, na.rm = T),
      # changes in urban shares
      "{prefix}_urban_emp" := sum(wgt * employed * urban, na.rm = TRUE),
      .groups = 'drop'
      ) %>% 
    mutate(
      country = ctry_str
    )

}

add_aggregate <- function(
    ctry_str, 
    census_position = c("first", "second", "none"), # if there's no census - "none"
    agg_s, # if has aggregates of first census
    agg_e # if has aggregates of second census
    ){
  
  census_data <- census_info %>% 
    filter(country == ctry_str) %>%
    select(census_yr1, census_yr2)
  
  yr_start <- census_data$census_yr1
  yr_end <- census_data$census_yr2
  
  period_len <- yr_end - yr_start
  
  yr_s_suffix <- str_sub(yr_start, -2)
  yr_e_suffix <- str_sub(yr_end, -2)
  
  # initialise df
  base_data <- NULL
  
  # no need to rename columns as this should be done in the agg. cleaning
  if (agg_s) {
    agg_s <- readRDS(here(
      build.dir, "Countries", ctry_str, paste0(yr_s_suffix, "_emp_aggregates.rds")
    )) %>% 
      mutate(ipums_id = as.character(ipums_id))
  }
  
  if (agg_e) {
    agg_e <- readRDS(here(
      build.dir, "Countries", ctry_str, paste0(yr_e_suffix, "_emp_aggregates.rds")
    )) %>% 
      mutate(ipums_id = as.character(ipums_id))
  }
  
  has_census <- census_position %in% c("first", "second")
  
  if (has_census){
    emp_census <- empl_df %>% 
      filter(country == ctry_str) 
    
    # remove the delta columns and the bl/el
    if (census_position == "first") {
      base_data <- emp_census %>% 
        select(-starts_with("el_"), -starts_with("delta"))
    } else if (census_position == "second") {
      base_data <- emp_census %>% 
        select(-starts_with("bl_"), -starts_with("delta"))
    }
  } else{ # if there's no census already, then just aggregates
    if (!is.null(agg_s) && !is.null(agg_e)) {
      base_data <- full_join(agg_s, agg_e, by = "ipums_id")
    } else if (!is.null(agg_s)) {
      base_data <- agg_s
    } else if (!is.null(agg_e)) {
      base_data <- agg_e
    }
  }
  
  # merge if necessary
  if (has_census) { 
    if (census_position == "first" && !is.null(agg_e)) {
      base_data <- base_data %>% 
        left_join(agg_e, by = "ipums_id")
    }
    if (census_position == "second" && !is.null(agg_s)) {
      base_data <- base_data %>% 
        left_join(agg_s, by = "ipums_id")
    }
  }
  
  ## there may be missing columns, so create as NA if doesn't exist
  if (!"bl_mining" %in% names(base_data)) base_data$bl_mining <- NA_real_
  if (!"el_mining" %in% names(base_data)) base_data$el_mining <- NA_real_
  if (!"bl_urban_emp" %in% names(base_data)) base_data$bl_urban_emp <- NA_real_
  if (!"el_urban_emp" %in% names(base_data)) base_data$el_urban_emp <- NA_real_
  
  emp_final <- base_data %>% 
    mutate(
      country = ctry_str,
      empl_start_yr = yr_start, 
      empl_end_yr = yr_end,
      empl_prd_len = period_len,
      # empl change
      delta_empl = el_empl - bl_empl,
      delta_empl_ann = safe_divide(delta_empl,period_len),
      delta_log_empl = log(el_empl) - log(bl_empl),
      delta_log_empl_ann = safe_divide(delta_log_empl,period_len),
      # non-ag
      delta_nonag = el_nonag - bl_nonag,
      delta_nonag_ann = safe_divide(delta_nonag,period_len),
      delta_log_nonag = log(el_nonag) - log(bl_nonag),
      delta_log_nonag_ann = safe_divide(delta_log_nonag,period_len),
      # ag
      delta_ag = el_ag - bl_ag,
      delta_ag_ann = safe_divide(delta_ag,period_len),
      delta_log_ag = log(el_ag) - log(bl_ag),
      delta_log_ag_ann = safe_divide(delta_log_ag,period_len),
      # mining
      delta_mining = el_mining - bl_mining,
      delta_mining_ann = safe_divide(delta_mining,period_len),
      delta_log_mining = log(el_mining) - log(bl_mining),
      delta_log_mining_ann = safe_divide(delta_log_mining,period_len),
      # change in urban employment
      delta_urban_emp = el_urban_emp - bl_urban_emp,
      delta_urban_emp_ann = safe_divide(delta_urban_emp,period_len),
      # change in logs
      delta_log_urban_emp = ifelse(
        el_urban_emp != 0 & bl_urban_emp != 0,
        log(el_urban_emp) - log(bl_urban_emp), NA),
      delta_log_urban_emp_ann = safe_divide(delta_log_urban_emp,period_len),
      # change in shares
      delta_urban_emp_share = 
        safe_divide(el_urban_emp,el_empl) - safe_divide(bl_urban_emp,bl_empl),
      # clean inf
      across(starts_with("delta_log"), 
             ~ifelse(.x %in% c(-Inf, NaN, Inf), NA, .x))
    )
}

#*******************************************************************************

countries <- c( # list of all countries that use census data
  "BEN", "BWA", "BFA", "CMR", "GHA",
  "GIN", "CIV", "KEN", "LSO", "MWI",
  "MLI", "MUS", "MOZ", "RWA", "SEN",
  "SLE", "ZAF", "SDN", "TZA", "TGO",
  "UGA", "ZMB", "ZWE", "AGO", "MDG",
  "ETH", "NGA"
)

single_cens_ctries <- c(
  "AGO", "CIV", "MWI", "RWA", "SDN", "TGO", "ZWE", "MDG",
  "ETH", "NGA"
)
# call variables bl_ rather than el_
##single_first_cens_ctries <- c("CIV") ## for now actually define all apart for MDG
single_first_cens_ctries <- c(
  "AGO", "CIV", "MWI", "RWA", "SDN", "TGO", "ZWE", "ETH", "NGA"
  )

get_country_data <- function(ctry_str) {
  if (ctry_str %in% single_cens_ctries) f_empl1(ctry_str) 
  else f_empl(ctry_str)
}

empl_df <- purrr::map_dfr(countries, get_country_data)

### add countries with info from aggregates
agg_ctries <- c("GAB")

agg_ctries_updated <- list(
  # syntax - country,
  # census_positon is if there's already a census (like MDG) - first/second
  # if there's no census, 'none'
  # agg_s = if there are aggregates of the first census, agg_e for the second 
  #add_aggregate("MDG", census_position = "second", agg_s = T, agg_e = F),
  add_aggregate("GAB", census_position = "none", agg_s = T, agg_e = T)
)

empl_df <- empl_df %>% 
  filter(!country %in% agg_ctries) %>% 
  bind_rows(agg_ctries_updated)

# save ----
id_df <- readRDS(here(out.dir, "R", "id.rds"))

empl_df %<>%
  left_join(id_df, by = c("ipums_id", "country")) %>%
  relocate(ipums_id, admin_name, country_name, country, geo_lvl,
           empl_start_yr, empl_end_yr, empl_prd_len) %>%
  var_labels(
    bl_nonag = "Non-ag employment, baseline",
    bl_ag = "agricultural employment, baseline",
    bl_mining = "mining employment, baseline",
    bl_nonmining = "non-mining employment, baseline",
    bl_empl = "employment, baseline",
    bl_u = "unemployment, baseline",
    el_nonag = "Non-ag employment, endline",
    el_ag = "agricultural employment, endline",
    el_mining = "mining employment, endline",
    el_nonmining = "non-mining employment, endline",
    el_empl = "employment, endline",
    el_u = "unemployment, endline",
    empl_start_yr = "empl data baseline year",
    empl_end_yr = "empl data endline year",
    empl_prd_len = "empl data period length",
    delta_empl = "change in employment, total",
    delta_empl_ann = "change in employment, annual",
    delta_log_empl = "change in log employment, total",
    delta_log_empl_ann = "change in log employment, annual",
    delta_ag = "change in ag empl, total",
    delta_ag_ann = "change in ag empl, annual",
    delta_log_ag = "change in log ag empl, total",
    delta_log_ag_ann = "change in log ag empl, annual",
    delta_nonag = "change in non-ag empl, total",
    delta_nonag_ann = "change in non-ag empl, annual",
    delta_log_nonag = "change in log non-ag empl, total",
    delta_log_nonag_ann = "change in log non-ag empl, annual",
    delta_mining = "change in mining empl, total",
    delta_mining_ann = "change in mining empl, annual",
    delta_log_mining = "change in log mining empl, total",
    delta_log_mining_ann = "change in log mining empl, annual",
    # urban variables
    bl_urban_emp = "Urban employment, prime-aged baseline",
    delta_urban_emp = "change in urban employment, prime-aged total",
    delta_urban_emp_ann = "change in urban employment, prime-aged annual",
    delta_log_urban_emp = "change in log urban employment, prime-aged total",
    delta_log_urban_emp_ann = "change in log urban employment, prime-aged annual",
    delta_urban_emp_share = "change in urban employment, prime-aged share"
  )

saveRDS(empl_df, here(out.dir, "R", "employment.rds"))
write_dta(empl_df, here(out.dir, "employment.dta"))

