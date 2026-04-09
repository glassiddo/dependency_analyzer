#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    April 2, 2023
#* Title:   Migration
#* Desc:    Build migration Dataset for each admin unit that we use
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

od_flow <- function(ctry_str, lvl = 2, cens_yr, yr_start, yr_end, 
                    prime_age = TRUE) {
  
  dat_df <- readRDS(paste0(build.dir,ctry_str,"/Census/census",cens_yr,".rds")) %>% 
    rename_with(~"ipums_id_d", starts_with(paste0("geo", lvl)) & 
                  !ends_with(c("0","1","2","3","4","5","6","7","8","9", 
                               "geomig1_p")) ) %>%
    mutate(ipums_id_d = as.character(ipums_id_d))
  
  geo <- g_sym(lvl)
  prev_geo <- g_sym(lvl, r = "p")
  
  # 1. create set of districts where surveying happened
  d_df <- dat_df  %>%
    #select({{geo}}, ipums_id_d) %>%
    select(ipums_id_d) %>%
    distinct() 
  
  o_df <- d_df %>% 
    #rename({{prev_geo}} := {{geo}}, ipums_id_o = ipums_id_d) %>%
    rename(ipums_id_o = ipums_id_d)
  
  distset_df <- merge(d_df, o_df) %>%
    mutate(distset = 1)
  
  # keep only prime-aged people for migration flows
  if (prime_age == TRUE) { dat_df %<>% filter(age >= 15 & age <= 65) }
  
  # 3. migration flows
  sample_df <- dat_df %>%
    full_join(distset_df) %>%
    mutate(
      migrant = ifelse(migration_year >= yr_start & migration_year <= yr_end, wgt, 0),
      migrant = ifelse(is.na(migration_year) == TRUE, 0, migrant),
      #migrant = ifelse({{geo}} == {{prev_geo}}, 0, migrant),
      migrant = ifelse(ipums_id_o == ipums_id_d, 0, migrant),
      m_oo = ifelse(migrant > 0, 0, wgt)
      ) %>%
    #group_by({{geo}}, {{prev_geo}}) %>%
    group_by(ipums_id_o, ipums_id_d) %>%
    # NB: this includes international/unknown migrants
    summarise(m_od = sum(migrant, na.rm = TRUE),
              m_oo = sum(m_oo, na.rm = TRUE),
              N_o = sum(wgt),
              .groups = 'drop') %>%
    full_join(distset_df) %>%
    mutate(
      m_od = ifelse(is.na(m_od) == TRUE, 0, m_od)) %>%
    #group_by({{geo}}) %>%
    group_by(ipums_id_d) %>%
    # destination total immigration
    mutate(m_d = sum(m_od)) %>%
    ungroup() %>%
    #group_by({{prev_geo}}) %>%
    group_by(ipums_id_o) %>%
    # origin total emigration
    mutate(m_o = sum(m_od),
           m_oo = sum(m_oo),
           N_o = sum(N_o)) %>%
    ungroup() %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset) %>%
    mutate(d_shares  = (m_od / m_d),
           o_shares  = (m_od / m_o),
           country_name = ctry_str,
           mig_start_yr = yr_start, 
           mig_end_yr = yr_end,
           mig_prd_len = yr_end - yr_start + 1,
           mig_measure = paste0("window: ", mig_prd_len, " year"),)
  
}


#*******************************************************************************
##### 2. Origin-destination migration flows for fixed period
#*******************************************************************************

od_period_flow <- function(ctry_str, lvl = 2, cens_yr, period_len,  
                           prime_age = TRUE) {
  
  dat_df <- readRDS(paste0(build.dir,ctry_str,"/Census/census",cens_yr,".rds"))
  
  yr_end <- mean(dat_df$year, na.rm = TRUE)
  
  if (is.na(period_len) == TRUE) {
    lag <- "birth"
    mig_meas <- "since birth"
  }
  else {
    lag <- "prev"
    mig_meas <- paste0("fixed: ", period_len, " year")
  }
  
  geo <- g_sym(lvl)
  prev_geo <- g_sym(lvl, r = "p", l_stub = lag)
  
  # 1. create set of districts where surveying happened
  d_df <- dat_df %>% 
    rename_with(~"ipums_id_d", starts_with(paste0("geo", lvl)) & 
                  !ends_with(c("0","1","2","3","4","5","6","7","8","9", 
                               "geomig1_p")) ) %>%
    mutate(ipums_id_d = as.character(ipums_id_d)) %>%
    #select({{geo}}, ipums_id_d) %>%
    select(ipums_id_d) %>%
    distinct() 
  
  o_df <- d_df %>% 
  #rename({{prev_geo}} := {{geo}}, ipums_id_o = ipums_id_d) %>%
  rename(ipums_id_o = ipums_id_d)
  
  distset_df <- merge(d_df, o_df) %>%
    mutate(distset = 1)
  
  # keep only prime-aged people for migration flows
  if (prime_age == TRUE) { dat_df %<>% filter(age >= 15 & age <= 65) }
  
  # 3. migration flows
  sample_df <- dat_df %>%
    #drop_na({{prev_geo}}) %>%
    drop_na(ipums_o) %>%
    #group_by({{geo}}, {{prev_geo}}) %>%
    group_by(ipums_id_o, ipums_id_d) %>%
    summarise(m_od = sum(wgt, na.rm = TRUE), .groups = 'drop') %>%
    full_join(distset_df) %>%
    mutate(m_od = ifelse(is.na(m_od) == TRUE, 0, m_od),
           #m_od = ifelse({{geo}} == {{prev_geo}}, 0, m_od),
           m_oo = ifelse(ipums_id_o == ipums_id_d, m_od, 0),
           m_od = ifelse(ipums_id_o == ipums_id_d, 0, m_od),
           ) %>%
    #group_by({{geo}}) %>%
    group_by(ipums_id_d) %>%
    # destination total immigration
    mutate(m_d = sum(m_od)) %>%
    ungroup() %>%
    #group_by({{prev_geo}}) %>%
    group_by(ipums_id_o) %>%
    # origin total emigration
    mutate(m_o = sum(m_od),
           m_oo = sum(m_oo)) %>%
    ungroup() %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset) %>%
    mutate(d_shares  = (m_od / m_d),
           o_shares  = (m_od / m_o),
           mig_measure = mig_meas,
           country_name = ctry_str,
           mig_start_yr = yr_end - period_len + 1, 
           mig_end_yr = yr_end,
           mig_prd_len = period_len)
  
  # if (lag == "birth") {
  #   sample_df %<>% rename_with(~gsub(lag, "prev", .x), starts_with(lag)) 
  # }
  
  return(sample_df)
}




#*******************************************************************************
# Benin 2 ----
ctry_df <- od_flow("Benin", 2, "02", 1992, 2001)
mig_df <- ctry_df

dat_df <- readRDS(paste0(build.dir,"Benin","/Census/census","02",".rds"))

# Botswana 1 ----
ctry_df <- od_period_flow("Botswana", 1, "01", 5)
mig_df <- bind_rows(mig_df, ctry_df)

# Burkina Faso 2----
ctry_df <- od_period_flow("Burkina Faso", 2, "96", NA)
mig_df <- bind_rows(mig_df, ctry_df)

# Cameroon ----
ctry_df <- od_flow("Cameroon", 2, "87", 1977, 1986)
mig_df <- bind_rows(mig_df, ctry_df)

# Ghana ----
ctry_df <- od_period_flow("Ghana", 2, "00", 5)
mig_df <- bind_rows(mig_df, ctry_df)

# Guinea ----
ctry_df <- od_flow("Guinea", 1, "96", 1986, 1995)
mig_df <- bind_rows(mig_df, ctry_df)

# Kenya ----
ctry_df <- od_flow("Kenya", 2, "99", 1989, 1998)
mig_df <- bind_rows(mig_df, ctry_df)

# Lesotho ----
ctry_df <- od_period_flow("Lesotho", 1, "96", 10)
mig_df <- bind_rows(mig_df, ctry_df)

# Malawi ----
ctry_df <- od_flow("Malawi", 1, "08", 1998, 2007)
mig_df <- bind_rows(mig_df, ctry_df)

# Mali ----
ctry_df <- od_flow("Mali", 2, "98", 1988, 1997)
mig_df <- bind_rows(mig_df, ctry_df)

# Mauritius ----
ctry_df <- od_period_flow("Mauritius", 1, "00", 5)
mig_df <- bind_rows(mig_df, ctry_df)

# Mozambique ----
ctry_df <- od_period_flow("Mozambique", 2, "97", NA)
mig_df <- bind_rows(mig_df, ctry_df)

# Rwanda ----
ctry_df <- od_flow("Rwanda", 2, "12", 2002, 2011)
mig_df <- bind_rows(mig_df, ctry_df)

# Senegal ----
ctry_df <- od_period_flow("Senegal", 2, "02", 5)
mig_df <- bind_rows(mig_df, ctry_df)

# Sierra Leone ----
ctry_df <- od_period_flow("Sierra Leone", 2, "04", NA)
mig_df <- bind_rows(mig_df, ctry_df)

# South Africa ----
ctry_df <- od_flow("South Africa", 2, "01", 1996, 2000)
mig_df <- bind_rows(mig_df, ctry_df)

# Sudan & S. Sudan ----
ctry_df <- od_flow("Sudan", 1, "08", 1998, 2007)
mig_df <- bind_rows(mig_df, ctry_df)

# Tanzania ----
ctry_df <- od_period_flow("Tanzania", 1, "02", NA) 
mig_df <- bind_rows(mig_df, ctry_df)

# Togo ----
ctry_df <- od_flow("Togo", 2, "10", 2000, 2009)
mig_df <- bind_rows(mig_df, ctry_df)

# Uganda ----
ctry_df <- od_flow("Uganda", 1, "02", 1992, 2001)
mig_df <- bind_rows(mig_df, ctry_df)

# Zambia ----
ctry_df <- od_period_flow("Zambia", 2, "00", NA)
mig_df <- bind_rows(mig_df, ctry_df)

# Zimbabwe ----
ctry_df <- od_period_flow("Zimbabwe", 2, "12", 10)
mig_df <- bind_rows(mig_df, ctry_df)

# save ----
mig_df %<>%
  relocate(country_name, ipums_id_d, ipums_id_o, district, prev_district, 
           region, prev_region, mig_measure, mig_start_yr, mig_end_yr, 
           mig_prd_len) %>%
  var_labels(
    m_od = "migration from o to d",
    m_d = "total immigration at d",
    m_o = "total emigration from o",
    d_shares = "share of immig at d from o, m_od / m_d",
    o_shares = "share of emig from o at d , m_od / m_o",
    mig_measure = "migration survey measure",
  )

saveRDS(mig_df, file.path(out.dir, "R/migration.rds"))
write_dta(mig_df, file.path(out.dir, "migration.dta"))

