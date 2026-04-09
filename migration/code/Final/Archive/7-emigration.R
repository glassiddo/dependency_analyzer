#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    April 1, 2023
#* Title:   Emigration
#* Desc:    Build emigration Dataset for each admin unit that we use
#* TODO:    do we want to use prime-aged for emigration?
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

emig_flow <- function(ctry_str, lvl = 2, cens_yr, yr_start, yr_end, 
                      prime_age = TRUE) {
  
  dat_df <- readRDS(paste0(build.dir, ctry_str, "/Census/census", cens_yr, ".rds"))
  
  geo <- g_sym(lvl)
  prev_geo <- g_sym(lvl, r = "p")
  
  # 1. create set of districts where surveying happened
  distset_df <- dat_df %>% 
    rename_with(~"ipums_id", starts_with(paste0("geo", lvl)) & 
                  !ends_with(c("0","1","2","3","4","5","6","7","8","9")) ) %>%
    mutate(ipums_id = as.character(ipums_id)) %>%
    select({{geo}}, ipums_id) %>%
    distinct() %>%
    mutate(distset = 1)
  
  # 2. keep only prime-aged people for migration flows
  if (prime_age == TRUE) { dat_df %<>% filter(age >= 15 & age <= 65) }
  
  period_len <- yr_end - yr_start + 1
  
  # 3. migration flows
  sample_df <- dat_df %>%
    mutate(
      migrant = ifelse(migration_year >= yr_start & migration_year <= yr_end, wgt, 0),
      migrant = ifelse(is.na(migration_year) == TRUE, 0, migrant),
      migrant = ifelse({{geo}} == {{prev_geo}}, 0, migrant)) %>%
    group_by({{prev_geo}}) %>%
    summarise(emig = sum(migrant, na.rm = TRUE), .groups = 'drop') %>%
    rename_with(~sub("prev_", "", .x), starts_with("prev")) %>%
    full_join(distset_df) %>%
    mutate(emig = ifelse(is.na(emig) == TRUE, 0, emig),
           #make average annual
           emig_ann = emig / period_len,
           log_emig = ifelse(emig > 0, log(emig), 0),
           log_emig_ann = log_emig / period_len,
           emig_measure = paste0("window: ", period_len, " year"),
           country_name = ctry_str,
           emig_start_yr = yr_start, 
           emig_end_yr = yr_end,
           emig_prd_len = period_len) %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset) 
}

#*******************************************************************************
##### 2. emigration flows for fixed period
#*******************************************************************************

emig_period_flow <- function(ctry_str, lvl = 2, cens_yr, period_len, 
                             prime_age = TRUE) {
  
  dat_df <- readRDS(paste0(build.dir, ctry_str, "/Census/census", cens_yr, ".rds"))
  
  yr_end <- mean(dat_df$year, na.rm = TRUE)
  
  # define fixed period or since birth and set-up prev loc names
  if (is.na(period_len) == TRUE) {
    lag <- "birth"
    emig_meas <- "since birth"
  }
  else {
    lag <- "prev"
    emig_meas <- paste0("fixed: ", period_len, " year")
  }
  
  geo <- g_sym(lvl)
  prev_geo <- g_sym(lvl, r = "p", l_stub = lag)
  
  # 1. create set of districts where surveying happened
  distset_df <- dat_df %>% 
    rename_with(~"ipums_id", starts_with(paste0("geo", lvl)) & 
                  !ends_with(c("0","1","2","3","4","5","6","7","8","9")) ) %>%
    mutate(ipums_id = as.character(ipums_id)) %>%
    select({{geo}}, ipums_id) %>%
    distinct() %>%
    mutate(distset = 1)
  
  # o_df <- d_df %>% 
  #   rename({{prev_geo}} := {{geo}}, ipums_id_o = ipums_id_d)
  # 
  # distset_df <- merge(d_df, o_df) %>%
  #   mutate(distset = 1)
  
  # keep only prime-aged people for migration flows
  if (prime_age == TRUE) { dat_df %<>% filter(age >= 15 & age <= 65) }
  
  # 3. migration flows
  emig_df <- dat_df %>%
    filter({{geo}} != {{prev_geo}}) %>%
    group_by({{prev_geo}}) %>%
    summarise(emig = sum(wgt, na.rm = TRUE), .groups = 'drop') %>%
    rename_with(~sub(paste0(lag,"_"), "", .x), starts_with(lag)) %>%
    full_join(distset_df) %>%
    mutate(emig = ifelse(is.na(emig) == TRUE, 0, emig),
           #make average annual
           emig_ann = emig / period_len,
           log_emig = ifelse(emig > 0, log(emig), 0),
           log_emig_ann = log_emig / period_len,
           emig_measure = emig_meas,
           country_name = ctry_str,
           emig_start_yr = yr_end - period_len + 1, 
           emig_end_yr = yr_end,
           emig_prd_len = period_len) %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset) 
  
  # if (lag == "birth") {
  #   emig_df %<>% rename_with(~gsub(lag, "prev", .x), starts_with(lag)) 
  # }
  # 
  # return(emig_df)
}

#*******************************************************************************
##### 3. combine baseline and endline
#*******************************************************************************

emig_wide <- function(bl_df, el_df) {
  bl_df %<>% rename_with(~paste0("bl_", .x), contains("emig"))
  out_df <- full_join(el_df, bl_df)
}

#*******************************************************************************
# Benin 2 ----
ctry_df <- emig_wide(
  emig_flow("Benin", 2, "02", 1992, 2001),
  emig_flow("Benin", 2, "13", 2003, 2012) )
emig_df <- ctry_df

# Botswana 1 ----
ctry_df <- emig_wide(
  emig_period_flow("Botswana", 1, "01", 5),
  emig_period_flow("Botswana", 1, "11", 5) )
emig_df <- bind_rows(emig_df, ctry_df)

# Burkina Faso ----
ctry_df <- emig_wide(
  emig_period_flow("Burkina Faso", 2, "96", 1),
  emig_period_flow("Burkina Faso", 2, "06", 1) )
emig_df <- bind_rows(emig_df, ctry_df)

# Cameroon ----
ctry_df <- emig_wide(
  emig_flow("Cameroon", 2, "87", 1977, 1986),
  emig_flow("Cameroon", 2, "05", 1995, 2004)
   )
emig_df <- bind_rows(emig_df, ctry_df)

# Ghana ----
ctry_df <- emig_wide(
  emig_period_flow("Ghana", 2, "00", 5),
  emig_period_flow("Ghana", 2, "10", 5)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Guinea ----
ctry_df <- emig_wide(
  emig_flow("Guinea", 1, "96", 1986, 1995),
  emig_flow("Guinea", 1, "14", 2004, 2013)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Kenya ----
ctry_df <- emig_wide(
  emig_flow("Kenya", 2, "99", 1999, 1999),
  emig_period_flow("Kenya", 2, "09", 1)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Lesotho ----
ctry_df <- emig_wide(
  emig_period_flow("Lesotho", 1, "96", 10),
  emig_period_flow("Lesotho", 1, "06", 10)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Malawi ----
ctry_df <- emig_flow("Malawi", 1, "08", 1998, 2007) 
emig_df <- bind_rows(emig_df, ctry_df)

# Mali ----
ctry_df <- emig_wide(
  emig_flow("Mali", 2, "98", 1988, 1997),
  emig_flow("Mali", 2, "09", 1999, 2008)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Mauritius ----
ctry_df <- emig_wide(
  emig_period_flow("Mauritius", 1, "00", 5),
  emig_period_flow("Mauritius", 1, "11", 5)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Mozambique ----
ctry_df <- emig_wide(
  emig_period_flow("Mozambique", 2, "97", 5),
  emig_period_flow("Mozambique", 2, "07", 5)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Rwanda ----
ctry_df <- emig_flow("Rwanda", 2, "12", 2002, 2011) 
emig_df <- bind_rows(emig_df, ctry_df)

# Senegal ----
ctry_df <- emig_wide(
  emig_period_flow("Senegal", 2, "02", 5),
  emig_period_flow("Senegal", 2, "13", 5)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Sierra Leone ----
ctry_df <- emig_wide(
  emig_period_flow("Sierra Leone", 2, "04", 14),
  emig_period_flow("Sierra Leone", 2, "15", 5)
)
emig_df <- bind_rows(emig_df, ctry_df)

# South Africa ----
ctry_df <- emig_wide(
  emig_flow("South Africa", 2, "01", 1996, 2000),
  emig_flow("South Africa", 2, "11", 2002, 2010) )
emig_df <- bind_rows(emig_df, ctry_df)

# Sudan & S. Sudan ----
ctry_df <- emig_flow("Sudan", 1, "08", 1998, 2007) 
emig_df <- bind_rows(emig_df, ctry_df)

# Tanzania ----
ctry_df <- emig_wide(
  emig_period_flow("Tanzania", 1, "02", 1),
  emig_period_flow("Tanzania", 1, "12", 1)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Togo ----
ctry_df <- emig_flow("Togo", 2, "10", 2000, 2009) 
emig_df <- bind_rows(emig_df, ctry_df)

# Uganda ----
ctry_df <- emig_wide(
  emig_flow("Uganda", 1, "02", 1992, 2001),
  emig_flow("Uganda", 1, "14", 2004, 2013)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Zambia ----
ctry_df <- emig_wide(
  emig_period_flow("Zambia", 2, "00", 1),
  emig_period_flow("Zambia", 2, "10", 1)
)
emig_df <- bind_rows(emig_df, ctry_df)

# Zimbabwe ----
ctry_df <- emig_period_flow("Zimbabwe", 2, "12", 10) 
emig_df <- bind_rows(emig_df, ctry_df)

# save ----
id_df <- readRDS(file.path(out.dir, "R/id.rds"))

emig_df %<>%
  left_join(id_df) %>%
  relocate(country_name, ipums_id, admin_name, admin_id, geo_lvl, 
           emig_start_yr, emig_end_yr, emig_prd_len) %>%
  relocate(district, region, .after = last_col()) %>%
  var_labels(
    emig = "emigration, prime-aged",
    emig_ann = "annual emigration, prime-aged",
    log_emig = "log emigration, prime-aged",
    log_emig_ann = "annual log emigration, prime-aged",
    emig_measure = "emigration measure",
    emig_start_yr = "emigration period start year",
    emig_end_yr = "emigration period end year",
    emig_prd_len = "emigration period lenfth, years",
    bl_emig = "baseline emigration, prime-aged",
    bl_emig_ann = "baseline annual emigration, prime-aged",
    bl_log_emig = "baseline log emigration, prime-aged",
    bl_log_emig_ann = "baseline annual log emigration, prime-aged",
    bl_emig_measure = "baseline emigration measure",
    bl_emig_start_yr = "baseline emigration period start year",
    bl_emig_end_yr = "baseline emigration period end year",
    bl_emig_prd_len = "baseline emigration period lenfth, years"
  )

saveRDS(emig_df, file.path(out.dir, "R/emigration.rds"))
write_dta(emig_df, file.path(out.dir, "emigration.dta"))

