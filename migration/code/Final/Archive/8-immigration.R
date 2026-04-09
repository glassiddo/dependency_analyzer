#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    April 4, 2023
#* Title:   Immigration
#* Desc:    Build immigration Dataset for each admin unit that we use
#* TODO:    do we want to use prime-aged for immigration?
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

immig_flow <- function(ctry_str, lvl = 2, cens_yr, yr_start, yr_end, 
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
    group_by({{geo}}) %>%
    summarise(immig = sum(migrant, na.rm = TRUE), .groups = 'drop') %>%
    full_join(distset_df) %>%
    mutate(immig = ifelse(is.na(immig) == TRUE, 0, immig),
           #make average annual
           immig_ann = immig / period_len,
           log_immig = ifelse(immig > 0, log(immig), 0),
           log_immig_ann = log_immig / period_len,
           immig_measure = paste0("window: ", period_len, " year"),
           country_name = ctry_str,
           immig_start_yr = yr_start, 
           immig_end_yr = yr_end,
           immig_prd_len = period_len) %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset) 
}

#*******************************************************************************
##### 2. immigration flows for fixed period
#*******************************************************************************

immig_period_flow <- function(ctry_str, lvl = 2, cens_yr, period_len, 
                             prime_age = TRUE) {
  
  dat_df <- readRDS(paste0(build.dir, ctry_str, "/Census/census", cens_yr, ".rds"))
  
  yr_end <- mean(dat_df$year, na.rm = TRUE)
  
  # define fixed period or since birth and set-up prev loc names
  if (is.na(period_len) == TRUE) {
    lag <- "birth"
    immig_meas <- "since birth"
  }
  else {
    lag <- "prev"
    immig_meas <- paste0("fixed: ", period_len, " year")
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
  
  # keep only prime-aged people for migration flows
  if (prime_age == TRUE) { dat_df %<>% filter(age >= 15 & age <= 65) }
  
  # 3. migration flows
  immig_df <- dat_df %>%
    filter({{geo}} != {{prev_geo}}) %>%
    group_by({{geo}}) %>%
    summarise(immig = sum(wgt, na.rm = TRUE), .groups = 'drop') %>%
    full_join(distset_df) %>%
    mutate(immig = ifelse(is.na(immig) == TRUE, 0, immig),
           #make average annual
           immig_ann = immig / period_len,
           log_immig = ifelse(immig > 0, log(immig), 0),
           log_immig_ann = log_immig / period_len,
           immig_measure = immig_meas,
           country_name = ctry_str,
           immig_start_yr = yr_end - period_len + 1, 
           immig_end_yr = yr_end,
           immig_prd_len = period_len) %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset) 

}

#*******************************************************************************
##### 3. combine baseline and endline
#*******************************************************************************

immig_wide <- function(bl_df, el_df) {
  bl_df %<>% rename_with(~paste0("bl_", .x), contains("immig"))
  out_df <- full_join(el_df, bl_df)
}

#*******************************************************************************
# Benin 2 ----
ctry_df <- immig_wide(
  immig_flow("Benin", 2, "02", 1992, 2001),
  immig_flow("Benin", 2, "13", 2003, 2012) )
immig_df <- ctry_df

# Botswana 1 ----
ctry_df <- immig_wide(
  immig_period_flow("Botswana", 1, "01", 5),
  immig_period_flow("Botswana", 1, "11", 5) )
immig_df <- bind_rows(immig_df, ctry_df)

# Burkina Faso ----
ctry_df <- immig_wide(
  immig_period_flow("Burkina Faso", 2, "96", 1),
  immig_period_flow("Burkina Faso", 2, "06", 1) )
immig_df <- bind_rows(immig_df, ctry_df)

# Cameroon ----
ctry_df <- immig_wide(
  immig_flow("Cameroon", 2, "87", 1977, 1986),
  immig_flow("Cameroon", 2, "05", 1995, 2004)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Ghana ----
ctry_df <- immig_wide(
  immig_period_flow("Ghana", 2, "00", 5),
  immig_period_flow("Ghana", 2, "10", 5)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Guinea ----
ctry_df <- immig_wide(
  immig_flow("Guinea", 1, "96", 1986, 1995),
  immig_flow("Guinea", 1, "14", 2004, 2013)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Kenya ----
ctry_df <- immig_wide(
  immig_flow("Kenya", 2, "99", 1999, 1999),
  immig_period_flow("Kenya", 2, "09", 1)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Lesotho ----
ctry_df <- immig_wide(
  immig_period_flow("Lesotho", 1, "96", 10),
  immig_period_flow("Lesotho", 1, "06", 10)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Malawi ----
ctry_df <- immig_flow("Malawi", 1, "08", 1998, 2007) 
immig_df <- bind_rows(immig_df, ctry_df)

# Mali ----
ctry_df <- immig_wide(
  immig_flow("Mali", 2, "98", 1988, 1997),
  immig_flow("Mali", 2, "09", 1999, 2008)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Mauritius ----
ctry_df <- immig_wide(
  immig_period_flow("Mauritius", 1, "00", 5),
  immig_period_flow("Mauritius", 1, "11", 5)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Mozambique ----
ctry_df <- immig_wide(
  immig_period_flow("Mozambique", 2, "97", 5),
  immig_period_flow("Mozambique", 2, "07", 5)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Rwanda ----
ctry_df <- immig_flow("Rwanda", 2, "12", 2002, 2011) 
immig_df <- bind_rows(immig_df, ctry_df)

# Senegal ----
ctry_df <- immig_wide(
  immig_period_flow("Senegal", 2, "02", 5),
  immig_period_flow("Senegal", 2, "13", 5)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Sierra Leone ----
ctry_df <- immig_wide(
  immig_period_flow("Sierra Leone", 2, "04", 14),
  immig_period_flow("Sierra Leone", 2, "15", 5)
)
immig_df <- bind_rows(immig_df, ctry_df)

# South Africa ----
ctry_df <- immig_wide(
  immig_flow("South Africa", 2, "01", 1996, 2000),
  immig_flow("South Africa", 2, "11", 2002, 2010) )
immig_df <- bind_rows(immig_df, ctry_df)

# Sudan & S. Sudan ----
ctry_df <- immig_flow("Sudan", 1, "08", 1998, 2007) 
immig_df <- bind_rows(immig_df, ctry_df)

# Tanzania ----
ctry_df <- immig_wide(
  immig_period_flow("Tanzania", 1, "02", 1),
  immig_period_flow("Tanzania", 1, "12", 1)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Togo ----
ctry_df <- immig_flow("Togo", 2, "10", 2000, 2009) 
immig_df <- bind_rows(immig_df, ctry_df)

# Uganda ----
ctry_df <- immig_wide(
  immig_flow("Uganda", 1, "02", 1992, 2001),
  immig_flow("Uganda", 1, "14", 2004, 2013)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Zambia ----
ctry_df <- immig_wide(
  immig_period_flow("Zambia", 2, "00", 1),
  immig_period_flow("Zambia", 2, "10", 1)
)
immig_df <- bind_rows(immig_df, ctry_df)

# Zimbabwe ----
ctry_df <- immig_period_flow("Zimbabwe", 2, "12", 10) 
immig_df <- bind_rows(immig_df, ctry_df)

# save ----
id_df <- readRDS(file.path(out.dir, "R/id.rds"))

immig_df %<>%
  left_join(id_df) %>%
  relocate(country_name, ipums_id, admin_name, admin_id, geo_lvl, 
           immig_start_yr, immig_end_yr, immig_prd_len) %>%
  relocate(district, region, .after = last_col()) %>%
  var_labels(
    immig = "immigration, prime-aged",
    immig_ann = "annual immigration, prime-aged",
    log_immig = "log immigration, prime-aged",
    log_immig_ann = "annual log immigration, prime-aged",
    immig_measure = "immigration measure",
    immig_start_yr = "immigration period start year",
    immig_end_yr = "immigration period end year",
    immig_prd_len = "immigration period lenfth, years",
    bl_immig = "baseline immigration, prime-aged",
    bl_immig_ann = "baseline annual immigration, prime-aged",
    bl_log_immig = "baseline log immigration, prime-aged",
    bl_log_immig_ann = "baseline annual log immigration, prime-aged",
    bl_immig_measure = "baseline immigration measure",
    bl_immig_start_yr = "baseline immigration period start year",
    bl_immig_end_yr = "baseline immigration period end year",
    bl_immig_prd_len = "baseline immigration period lenfth, years"
  )

saveRDS(immig_df, file.path(out.dir, "R/immigration.rds"))
write_dta(immig_df, file.path(out.dir, "immigration.dta"))

