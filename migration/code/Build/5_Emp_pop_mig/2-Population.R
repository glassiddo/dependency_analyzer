#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    April 1, 2023
#* Title:   Population
#* Desc:    Build population Dataset for each admin unit that we use
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

f_pop <- function(ctry_str) {
  print("Processing:")
  print(ctry_str)
  #* create the following (+ naming convention)
  #* 1. baseline and endline population, primeage if set like it (bl, el)
  #* 2. delta population, delta log pop, and annualized 
  
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
  
  # filter to prime age
  if(prime_age == TRUE) {
    start_df <- start_df %>% filter(age >= min_pa & age <= max_pa)
    end_df <- end_df %>% filter(age >= min_pa & age <= max_pa)
  }
  
  geo <- g_sym(lvl)
  ipums <- sym(paste0("geo", lvl))
  
  # population in first period
  pop0 <- start_df %>% 
    rename_with(~"ipums_id", starts_with(paste0("geo", lvl)) & 
                  !ends_with(c("0","1","2","3","4","5","6","7","8","9")) ) %>%
    mutate(
      ipums_id = as.character(ipums_id)
      ) %>% 
    group_by(ipums_id) %>%
    summarise(
      # in case someone has missing weight
      bl_pop = round(sum(wgt, na.rm = TRUE)),
      # if people have no age reported, they are removed if prime_age = TRUE
      bl_rural_pop = sum(wgt * (1 - urban), na.rm = TRUE),
      bl_urban_pop = sum(wgt * urban, na.rm = TRUE),
      across(starts_with("age_"), ~round(sum(.x * wgt, na.rm = TRUE))),
      .groups = 'drop'
      ) 
  
  # population in o and d in second period
  pop1 <- end_df %>% 
    rename_with(~"ipums_id", starts_with(paste0("geo", lvl)) & 
                  !ends_with(c("0","1","2","3","4","5","6","7","8","9")) ) %>% 
    mutate(
      ipums_id = as.character(ipums_id)
      ) %>% 
    group_by(ipums_id) %>%
    summarise(
      el_pop = sum(wgt, na.rm = TRUE),
      el_rural_pop = sum(wgt * (1 - urban), na.rm = TRUE),
      el_urban_pop = sum(wgt * urban, na.rm = TRUE),
      .groups = 'drop'
      )
  
  pop <- full_join(pop0, pop1, by = "ipums_id") %>%
    mutate(
      country = ctry_str,
      pop_start_yr = yr_start, 
      pop_end_yr = yr_end,
      pop_prd_len = period_len,
      # pop change
      delta_pop = el_pop - bl_pop,
      delta_pop_ann = safe_divide(delta_pop,period_len),
      delta_log_pop = log(el_pop) - log(bl_pop),
      delta_log_pop_ann = safe_divide(delta_log_pop,period_len),
      #rural pop
      delta_rural_pop = el_rural_pop - bl_rural_pop,
      delta_rural_pop_ann = safe_divide(delta_rural_pop,period_len),
      delta_log_rural_pop = log(el_rural_pop) - log(bl_rural_pop),
      delta_log_rural_pop_ann = safe_divide(delta_log_rural_pop,period_len),
      #urban pop
      delta_urban_pop = el_urban_pop - bl_urban_pop,
      delta_urban_pop_ann = safe_divide(delta_urban_pop,period_len),
      delta_log_urban_pop = log(el_urban_pop) - log(bl_urban_pop),
      delta_log_urban_pop_ann = safe_divide(delta_log_urban_pop,period_len),
      delta_urban_pop_share = 
        safe_divide(el_urban_pop,el_pop) - safe_divide(bl_urban_pop,bl_pop),
      # clean inf
      across(starts_with("delta_log"), 
             ~ifelse(.x %in% c(-Inf, NaN, Inf), NA, .x))
    )
}

# function for one census
f_pop1 <- function(ctry_str) {
  print("Processing:")
  print(ctry_str)
  #* create the following (+ naming convention)
  #* 1. baseline and endline population, primeage if set so (bl, el)

  census_data <- census_info %>% 
    filter(country == ctry_str) %>%
    janitor::remove_empty(which = "cols") %>%  # remove the empty column
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
  
  # filter to prime age
  if(prime_age == TRUE) {
    df <- df %>% filter(age >= min_pa & age <= max_pa)
  }
  # 
  yr_census <- mean(df$year, na.rm = TRUE)
  
  geo <- g_sym(lvl)
  ipums <- sym(paste0("geo", lvl))
  
  prefix <- if(ctry_str %in% single_first_cens_ctries) "bl" else "el"
  yr_var <- if(ctry_str %in% single_first_cens_ctries) "pop_start_yr" else "pop_end_yr"
  
  # population in first period
  pop0 <- df %>% 
    rename_with(~"ipums_id", starts_with(paste0("geo", lvl)) & 
                  !ends_with(c("0","1","2","3","4","5","6","7","8","9")) ) %>%
    mutate( 
      ipums_id = as.character(ipums_id)
      ) %>% 
    group_by(ipums_id) %>%
    summarise(
      # in case someone has missing weight
      "{prefix}_pop" := round(sum(wgt, na.rm = TRUE)),
      # if people have no age reported, they are removed if prime_age = TRUE
      "{prefix}_rural_pop" := sum(wgt * (1 - urban), na.rm = TRUE),
      "{prefix}_urban_pop" := sum(wgt * urban, na.rm = TRUE),
      across(starts_with("age_"), ~round(sum(.x * wgt, na.rm = TRUE))),
      .groups = 'drop'
    ) %>%
    mutate(
      country = ctry_str,
      "{yr_var}" := yr_census
    )
}

# countries' data may be obtained from aggregates
add_aggregate <- function(
  ctry_str, 
  census_position = c("first", "second", "none"), # if there's no census - "none"
  agg_s, # if has aggregates of first census
  agg_e # if has aggregates of second census
  
  # theres a lot of if conditions here to cover various possibilities -
  # both years (start+end) or just one may be obtained from aggregates
  # and there's also a possibility of countries mixing census+aggregates (MDG)
  # in practice most of them are irrelevant so perhaps should be simplified...
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
      build.dir, "Countries", ctry_str, paste0(yr_s_suffix, "_pop_aggregates.rds")
    )) %>% 
      mutate(ipums_id = as.character(ipums_id))
  }
  
  if (agg_e) {
    agg_e <- readRDS(here(
      build.dir, "Countries", ctry_str, paste0(yr_e_suffix, "_pop_aggregates.rds")
    )) %>% 
      mutate(ipums_id = as.character(ipums_id))
  }
  
  has_census <- census_position %in% c("first", "second")
  
  if (has_census){
    pop_census <- pop_df %>% 
      filter(country == ctry_str) 
    
    # remove the delta columns and the bl/el
    if (census_position == "first") {
      base_data <- pop_census %>% 
        select(-starts_with("el_"), -starts_with("delta"))
    } else if (census_position == "second") {
      base_data <- pop_census %>% 
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
  
  pop_final <- base_data %>% 
    mutate(
      country = ctry_str,
      pop_start_yr = yr_start,
      pop_end_yr = yr_end,
      pop_prd_len = period_len,
      delta_pop = el_pop - bl_pop,
      delta_pop_ann = safe_divide(delta_pop,period_len),
      delta_log_pop = log(el_pop) - log(bl_pop),
      delta_log_pop_ann = safe_divide(delta_log_pop,period_len),
      #rural pop
      delta_rural_pop = el_rural_pop - bl_rural_pop,
      delta_rural_pop_ann = safe_divide(delta_rural_pop,period_len),
      delta_log_rural_pop = log(el_rural_pop) - log(bl_rural_pop),
      delta_log_rural_pop_ann = safe_divide(delta_log_rural_pop,period_len),
      #urban pop
      delta_urban_pop = el_urban_pop - bl_urban_pop,
      delta_urban_pop_ann = safe_divide(delta_urban_pop,period_len),
      delta_log_urban_pop = log(el_urban_pop) - log(bl_urban_pop),
      delta_log_urban_pop_ann = safe_divide(delta_log_urban_pop,period_len),
      delta_urban_pop_share = 
        safe_divide(el_urban_pop,el_pop) - safe_divide(bl_urban_pop,bl_pop),
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
  "AGO", "CIV", "MWI", "RWA", "SDN", "TGO", "ZWE", "MDG", "ETH", "NGA"
  )
# call variables bl_ rather than el_
##single_first_cens_ctries <- c("CIV") ## for now actually define all apart for MDG
single_first_cens_ctries <- c(
  "AGO", "CIV", "MWI", "RWA", "SDN", "TGO", "ZWE", "ETH", "NGA"
  )

get_country_data <- function(ctry_str) {
  if (ctry_str %in% single_cens_ctries) f_pop1(ctry_str) 
  else f_pop(ctry_str)
}

pop_df <- purrr::map_dfr(countries, get_country_data)

### add countries with info from aggregates
agg_ctries <- c("GAB") # madagascar was changed to a single census country

agg_ctries_updated <- list(
  # syntax - country,
  # census_positon is if there's already a census (like MDG) - first/second
  # if there's no census, 'none'
  # agg_s = if there are aggregates of the first census, agg_e for the second 
  add_aggregate("GAB", census_position = "none", agg_s = T, agg_e = T)
)

pop_df <- pop_df %>% 
  filter(!country %in% agg_ctries) %>% 
  bind_rows(agg_ctries_updated)

# save ----
id_df <- readRDS(here(out.dir, "R", "id.rds"))

pop_df %<>%
  left_join(id_df, by = c("ipums_id", "country")) %>%
  mutate(country_name = sapply(country, n_ctry)) %>% 
  relocate(ipums_id, admin_name, country_name, country, geo_lvl,
           pop_start_yr, pop_end_yr, pop_prd_len) %>%
  var_labels(
    bl_pop = "population, baseline",
    bl_rural_pop = "population rural, baseline",
    bl_urban_pop = "population urban, baseline",
    el_pop = "population, endline",
    el_rural_pop = "population rural, endline",
    el_urban_pop = "population urban, endline",
    pop_start_yr = "pop data baseline year",
    pop_end_yr = "pop data endline year",
    pop_prd_len = "pop data period length",
    delta_pop = "change in population, total",
    delta_pop_ann = "change in population, annual",
    delta_log_pop = "change in log population, total",
    delta_log_pop_ann = "change in log population, annual",
    delta_rural_pop = "change in pop, rural total",
    delta_rural_pop_ann = "change in pop, rural annual",
    delta_log_rural_pop = "change in log pop, rural total",
    delta_log_rural_pop_ann = "change in log pop, rural annual",
    delta_urban_pop = "change in pop, urban total",
    delta_urban_pop_ann = "change in pop, urban annual",
    delta_log_urban_pop = "change in log pop, urban total",
    delta_log_urban_pop_ann = "change in log pop, urban annual",
    delta_urban_pop_share = "change in urban population, share",
    )

saveRDS(pop_df, here(out.dir, "R", "population.rds"))
write_dta(pop_df, here(out.dir, "population.dta"))
