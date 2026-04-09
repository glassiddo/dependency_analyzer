# Project: Migration Africa
# Author:  Sam Marshall
# Date:    June 8, 2022
# Title:   Bartik Function
# Desc:    
#*******************************************************************************

#*******************************************************************************
# read fns ----
# function to make it easier to read files (less repetitive typing)

read_census <- function(ctry_str, yr) {
  out_df <- readRDS(paste0(build.dir, ctry_str, "/Census/census", yr, ".rds"))
}

read_fl <- function(ctry_str, lvl = 2) {
  out_df <- readRDS(
    paste0(build.dir, ctry_str, "/Forest Cover/l",lvl,"_loss30c.rds")
    )
}

read_dist <- function(ctry_str, lvl = 2) {
  
  geo_name <- sym(paste0(g_sym(lvl), "_name"))
  prev_geo_name <- sym(paste0(g_sym(lvl, r = "p"), "_name"))
  
  dist_df <- readRDS(
    paste0(build.dir, ctry_str, "/Support/cons_l", lvl, "_dist.rds")
    ) %>%
    mutate(distance = as.numeric(distance),
           distance = distance / 1000) 
  
  avg_dist_df <- dist_df %>%
    filter({{geo_name}} != {{prev_geo_name}})  %>%
    group_by({{prev_geo_name}}) %>%
    summarise(mean_dist = mean(distance),
              avg_dist_inv = mean(1/distance),
              total_dist_inv = sum(1/distance),
              .groups = 'drop') 
  
  out_df <- full_join(dist_df, avg_dist_df)
}

read_urb <- function(ctry_str, lvl = 2) {
  out_df <- readRDS(
    paste0(build.dir, ctry_str, "/Support/cons_l", lvl, "_urban.rds")
  ) 
}

read_nl <- function(ctry_str, lvl = 2) {
  out_df <- readRDS(
    paste0(build.dir, ctry_str, "/Support/l", lvl, "_nightlight.rds")
  ) 
}

read_crop <- function(ctry_str, lvl = 2, fl = "croparea") {
  out_df <- readRDS(
    paste0(build.dir, ctry_str, "/Support/l", lvl, "_", fl, ".rds")
  ) 
}

save_ctry <- function(dat_df, ctry_str, lvl = 2) {
  fl_str <- paste0(build.dir, ctry_str, "/Migration/l", lvl,"_Bartik_FL")
  write_dta(dat_df, paste0(fl_str, ".dta"))
  saveRDS(dat_df, paste0(fl_str, ".rds"))
}

read_ctry <- function( ctry_str, lvl = 2) {
  out_df <- readRDS(
    paste0(build.dir, ctry_str, "/Migration/l", lvl, "_Bartik_FL.rds")
    )
}

# common calls ----
g_sym <- function(lvl, r = "c", l_stub = "prev") {
  if (lvl == 2) {
    geo <- sym("district")
    prev_geo <- sym(paste(l_stub, "district", sep = "_"))
  }
  else {
    geo <- sym("region")
    prev_geo <- sym(paste(l_stub, "region", sep = "_"))
  }
  if (r == "c") {return(geo)}
  else {return(prev_geo)}
}


#*******************************************************************************
# svy_districts( dat_df)
# return the set of districts that were surveyed in that round
#*******************************************************************************

svy_districts <- function( dat_df, lag = "prev", l = 2) {
  if ("district" %in% names(dat_df)) {
    # get region codes and names if they exist
    if ("region" %in% names(dat_df)) {
      geo_df <- dat_df %>% 
        select(starts_with("district"), starts_with("region")) %>% unique()
    }
    else {
      geo_df <- dat_df %>% select(starts_with("district")) %>% unique()
    }
  }
  # if level one only, AKA only region
  else { 
    geo_df <- dat_df %>% select(region, region_name) %>% unique() 
    }
  # cross for previous district unless specified no
  if (lag != "none") {
    prev_geo_df <- geo_df %>% rename_with(~paste(lag, .x, sep = "_"))
    geo_df %<>% merge(prev_geo_df)
  }
  geo_df %<>% mutate(distset = 1)
  
  # get only regions if data is at district
  if (l == 1 & "district" %in% names(geo_df)) {
    geo_df %<>% select(contains("region"), distset) %>% unique()
  }
  return(geo_df)
}

# flows ----
#*******************************************************************************
##### Origin-destination migration flows
#*******************************************************************************

od_flow <- function(dat_df, yr_start, yr_end, lvl = 2, prime_age = TRUE) {
  
  # 1. create set of districts where surveying happened
  distset_df <- svy_districts(dat_df, l = lvl)
  geo <- g_sym(lvl)
  prev_geo <- g_sym(lvl, r = "p")
  
  # keep only prime-aged people for migration flows
  if (prime_age == TRUE) { dat_df %<>% filter(age >= 15 & age <= 65) }
  
  # 3. migration flows
  sample_df <- dat_df %>%
    mutate(
      migrant = ifelse(migration_year >= yr_start & migration_year <= yr_end, wgt, 0),
      migrant = ifelse(is.na(migration_year) == TRUE, 0, migrant),
      migrant = ifelse({{geo}} == {{prev_geo}}, 0, migrant)) %>%
    group_by({{geo}}, {{prev_geo}}) %>%
    summarise(m_od = sum(migrant, na.rm = TRUE),
              m_od_ag = sum(migrant * empl_ag, na.rm = TRUE),
              m_od_nonag = sum(migrant * (1 - empl_ag) * employed, na.rm = TRUE),
              m_od_mining = sum(migrant * empl_mining, na.rm = TRUE),
              m_od_nam = sum(migrant * empl_nam, na.rm = TRUE),
              m_od_u = sum(migrant * (1 - employed), na.rm = TRUE),
              .groups = 'drop') %>%
    full_join(distset_df) %>%
    mutate(
      across(starts_with("m_od"), ~ifelse(is.na(.x) == TRUE, 0, .x)),
      # all flows
      m_od_all = m_od,
      # domestic only flows
      m_od_dom = ifelse(is.na(distset) == TRUE, 0, m_od)) %>%
    group_by({{geo}}) %>%
    mutate(across(starts_with("m_od"), ~sum(.x), .names = "d_{.col}") ) %>%
    rename_with(~str_replace(.x, "d_m_od", "m_d"), starts_with("d_m_od")) %>%
    ungroup() %>%
    group_by({{prev_geo}}) %>%
    mutate(bl_emig = sum(m_od_all),
           # make origin total flow, same as baseline emig
           m_o_all = sum(m_od_all)) %>%
    ungroup() %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset)

}


#*******************************************************************************
##### 2. Origin-destination migration flows for fixed period
#*******************************************************************************

od_period_flow <- function(dat_df, lvl = 2, lag = "prev", 
                           prime_age = TRUE) {
  
  # 1. create set of districts where surveying happened
  distset_df <- svy_districts(dat_df, lag, l = lvl)
  geo <- g_sym(lvl)
  prev_geo <- g_sym(lvl, r = "p", l_stub = lag)
  
  # get only regions if data is at district
  # if (lvl == 1 & "district" %in% names(distset_df)) {
  #   distset_df %<>% select(contains("region"), distset) %>% unique()
  # }
  
  # keep only prime-aged people for migration flows
  if (prime_age == TRUE) { dat_df %<>% filter(age >= 15 & age <= 65) }
  
  # 3. migration flows
  sample_df <- dat_df %>%
    drop_na({{prev_geo}}) %>%
    group_by({{geo}}, {{prev_geo}}) %>%
    summarise(m_od = sum(wgt, na.rm = TRUE),
              m_od_ag = sum(wgt * empl_ag, na.rm = TRUE),
              m_od_nonag = sum(wgt * (1 - empl_ag) * employed, na.rm = TRUE),
              m_od_mining = sum(wgt * empl_mining, na.rm = TRUE),
              m_od_nam = sum(wgt * empl_nam, na.rm = TRUE),
              m_od_u = sum(wgt * (1 - employed), na.rm = TRUE),
              .groups = 'drop') %>%
    full_join(distset_df) %>%
    mutate(
      across(starts_with("m_od"), ~ifelse(is.na(.x) == TRUE, 0, .x)),
      across(starts_with("m_od"), ~ifelse({{geo}} == {{prev_geo}}, 0, .x)),
      # all flows
      m_od_all = m_od,
      # domestic only flows
      m_od_dom = ifelse(is.na(distset) == TRUE, 0, m_od)) %>%
    group_by({{geo}}) %>%
    # sum total flows by type
    mutate(across(starts_with("m_od"), ~sum(.x), .names = "d_{.col}") ) %>%
    # properly name the total flow columns as m_d
    rename_with(~str_replace(.x, "d_m_od", "m_d"), starts_with("d_m_od")) %>%
    ungroup() %>%
    group_by({{prev_geo}}) %>%
    mutate(bl_emig = sum(m_od_all),
           # make origin total flow, same as baseline emig
           m_o_all = sum(m_od_all)) %>%
    ungroup() %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset) 

  if (lag == "birth") {
    sample_df %<>% rename_with(~gsub(lag, "prev", .x), starts_with(lag)) %>%
      # rename_with(~sub("m_", "b_", .x), starts_with("m_")) %>%
      {.}
  }

  return(sample_df)
}


#*******************************************************************************
##### Origin emigration
#*******************************************************************************

emig_flow <- function(dat_df, yr_start, yr_end, lvl = 2, prime_age = TRUE) {
  
  # 1. create set of districts where surveying happened
  distset_df <- svy_districts(dat_df, lag = "none", l = lvl)
  geo <- g_sym(lvl)
  prev_geo <- g_sym(lvl, r = "p")

  # 2. keep only prime-aged people for migration flows
  if (prime_age == TRUE) { dat_df %<>% filter(age >= 15 & age <= 65) }
  
  # 3. migration flows
  sample_df <- dat_df %>%
    mutate(
      migrant = ifelse(migration_year >= yr_start & migration_year <= yr_end, wgt, 0),
      migrant = ifelse(is.na(migration_year) == TRUE, 0, migrant),
      migrant = ifelse({{geo}} == {{prev_geo}}, 0, migrant)) %>%
    group_by({{prev_geo}}) %>%
    summarise(e_o = sum(migrant, na.rm = TRUE), .groups = 'drop') %>%
    rename_with(~sub("prev_", "", .x), starts_with("prev")) %>%
    full_join(distset_df) %>%
    mutate(e_o = ifelse(is.na(e_o) == TRUE, 0, e_o),
           #make average annual
           e_o = e_o / (yr_end- yr_start),
           log_emig = log(1+e_o)) %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset)
}

emig_period_flow <- function(dat_df, period_len, lvl = 2, 
                             lag = "prev", prime_age = TRUE) {
  
  # 1. create set of districts where surveying happened
  distset_df <- svy_districts(dat_df, lag = "none", l = lvl)
  geo <- g_sym(lvl)
  prev_geo <- g_sym(lvl, r = "p", l_stub = lag)
  
  # keep only prime-aged people for migration flows
  if (prime_age == TRUE) { dat_df %<>% filter(age >= 15 & age <= 65) }
  
  # 3. migration flows
  emig_df <- dat_df %>%
    filter({{geo}} != {{prev_geo}}) %>%
    group_by({{prev_geo}}) %>%
    summarise(e_o = sum(wgt, na.rm = TRUE), .groups = 'drop') %>%
    rename_with(~sub("prev_", "", .x), starts_with("prev")) %>%
    full_join(distset_df) %>%
    mutate(e_o = ifelse(is.na(e_o) == TRUE, 0, e_o),
           #make average annual
           e_o = e_o / period_len,
           log_emig = log(1+e_o)) %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset) 
  
  immig_df <- dat_df %>%
    filter({{geo}} != {{prev_geo}}) %>%
    group_by({{geo}}) %>%
    summarise(i_o = sum(wgt, na.rm = TRUE), .groups = 'drop') %>%
    rename_with(~sub("prev_", "", .x), starts_with("prev")) %>%
    full_join(distset_df) %>%
    mutate(i_o = ifelse(is.na(i_o) == TRUE, 0, i_o),
           #make average annual
           i_o = i_o / period_len,
           log_immig = log(1+i_o)) %>%
    # drop flows in from unsurveyed districts
    drop_na(distset) %>%
    select(-distset) 
  
  out_df <- full_join(emig_df, immig_df) %>%
    mutate(net_log_mig = log_immig - log_emig,
           ihsine_mig = asinh(i_o - e_o))
}


#*******************************************************************************
##### 3. f_pop(start_df, end_df, ctry3)
#*******************************************************************************
f_pop <- function(start_df, end_df, lvl = 2) {
  # create the following (+ naming convention)
  # 1. delta log pop at o using full population (log(N_o1_all/N_o0_all))
  # 2. delta N_dt using prime aged (delta_Nd = N_d1_pa - N_d0_pa)
  # 3. Origin time 0 prime aged pop for Bartik
  # 4. Origin time 0 total pop for weight (pop_wgt)
  
  # time between censuses
  period_len <- mean(end_df$year, na.rm = TRUE) - mean(start_df$year, na.rm = TRUE)
  
  geo <- g_sym(lvl)
  prev_geo <- g_sym(lvl, r = "p")
  
  # population in o and d in first period
  pop0 <- start_df %>% 
    mutate(prime_age = ifelse(age >= 15 & age <= 65, wgt, 0),
           # young people period length
           age_ypl = ifelse(age >= 14 - period_len & age < 15, wgt, 0),
           # old people period length
           age_opl = ifelse(age >= 65 - period_len & age < 65, wgt, 0),
           #10 year window
           age_5_15 = ifelse(age >= 5 & age < 15, wgt, 0),
           age_55_65 = ifelse(age > 55  & age <= 65, wgt, 0),
           age_0_15 = ifelse(age >= 0 & age < 15, wgt, 0)) %>%
    group_by({{geo}}) %>% 
    summarise(
      # in case someone has missing weight
      N0_all = round(sum(wgt, na.rm = TRUE)),
      # keep people with no age reported, but not as prime aged
      N0_pa = round(sum(prime_age, na.rm = TRUE)),
      # agricultural employment weight
      N0_ag = sum(wgt * empl_ag, na.rm = TRUE),
      N0_nonag = sum(wgt * employed * (1 - empl_ag), na.rm = TRUE),
      N0_mining = sum(wgt * empl_mining, na.rm = TRUE),
      N0_nonmining = sum(wgt * employed * (1 - empl_mining), na.rm = TRUE),
      N0_empl = sum(wgt * employed, na.rm = TRUE),
      N0_u = sum(wgt * (1 - employed), na.rm = TRUE),
      N0_nam = sum(wgt * empl_nam, na.rm = TRUE),
      N0_rural = sum(wgt * (1 - urban), na.rm = TRUE),
      N0_urban = sum(wgt * urban, na.rm = TRUE),
      across(starts_with("age_"), ~round(sum(.x, na.rm = TRUE))),
      .groups = 'drop') %>%
    mutate(across(starts_with("age_"), ~log(.x), .names = "log_{.col}"),
           )
  
  # population in o and d in second period
  pop1 <- end_df %>% 
    mutate(prime_age = ifelse(age >= 15 & age <= 65, wgt, 0)) %>%
    group_by({{geo}}) %>% 
    summarise(
      N1_all = sum(wgt, na.rm = TRUE),
      N1_pa = sum(prime_age, na.rm = TRUE),
      N1_ag = sum(wgt * empl_ag, na.rm = TRUE),
      N1_nonag = sum(wgt * employed * (1 - empl_ag), na.rm = TRUE),
      N1_mining = sum(wgt * empl_mining, na.rm = TRUE),
      N1_nonmining = sum(wgt * employed * (1 - empl_mining), na.rm = TRUE),
      N1_empl = sum(wgt * employed, na.rm = TRUE),
      N1_u = sum(wgt * (1 - employed), na.rm = TRUE),
      N1_nam = sum(wgt * empl_nam, na.rm = TRUE),
      N1_rural = sum(wgt * (1 - urban), na.rm = TRUE),
      N1_urban = sum(wgt * urban, na.rm = TRUE),
      .groups = 'drop')
  
  # combine for destination
  pop <- full_join(pop0, pop1) 
  
  # destination
  pop_d <- pop %>%
    mutate(delta_Nd = N1_pa - N0_pa,
           delta_Nd_avg = delta_Nd / period_len,
           delta_log_pop_d = log(N1_pa) - log(N0_pa),
           delta_log_pop_d_avg = delta_log_pop_d / period_len,
           delta_Nd_ag = (N1_ag - N0_ag) / period_len,
           delta_Nd_nonag = (N1_nonag - N0_nonag) / period_len,
           delta_Nd_mining = (N1_mining - N0_mining) / period_len,
           delta_Nd_nonmining = (N1_nonmining - N0_nonmining) / period_len,
           delta_Nd_empl = (N1_empl - N0_empl) / period_len,
           delta_Nd_u = (N1_u - N0_u) / period_len,
           delta_Nd_nam = (N1_nam - N0_nam) / period_len,
           delta_Nd_rural = (N1_rural - N0_rural) / period_len,
           delta_Nd_urban = (N1_urban - N0_urban) / period_len,
           bl_pop = N0_all,
           bl_pa  = N0_pa) %>%
    select({{geo}}, starts_with("delta"), bl_pop, bl_pa, N1_nonag)
  
  # origin
  pop_o <- pop %>%
    mutate(log_gr = log(N1_all / N0_all),
           log_gr_ann = log((N1_all / N0_all)^(1/period_len)),
           log_gr_ag = log((1 + N1_ag) / (1 + N0_ag)),
           log_gr_mining = log((1 + N1_mining) / (1 + N0_mining)),
           log_gr_nam = log((1 + N1_nam) / (1 + N0_nam)),
           log_gr_empl = log((1 + N1_empl) / (1 + N0_empl)),
           log_gr_rural = log((1 + N1_rural) / (1 + N0_rural)),
           pop_wgt = N0_all,
           ag_wgt = N0_ag,
           rural_wgt = N0_rural,
           N_o0 = N0_pa,
           N_o0_ag = N0_ag,
           N_o0_mining = N0_mining,
           N_o0_nam = N0_nam,
           N_o0_u = N0_u,
           N_o0_rural = N0_rural) %>%
    select({{geo}}, ends_with("wgt"), starts_with("log_gr"),
           starts_with("N_o0"), starts_with("log_age_")) %>%
    rename({{prev_geo}} := {{geo}})
#TODO something wrong in here... with delta log age creation
  # join datasets and create variables
  distset_df <- svy_districts(end_df, l = lvl)
  # get only regions if data is at district
  if (lvl == 1 & "district" %in% names(distset_df)) {
    distset_df %<>% select(contains("region"), distset) %>% unique()
  }
  
  pop_df <- full_join(distset_df, pop_d) %>%
    full_join(pop_o) %>%
    drop_na(distset) %>%
    select(-distset) %>%
    mutate(period_len = period_len)
}

#****************************************************************************
#* make urban status for each geographical unit using the second wave
#****************************************************************************

f_urb <- function(dat_df = cens2_df, lvl = 2) {
  if (lvl == 2) {
    out_df <- dat_df %>% group_by(district, district_name) %>% 
      summarise(urban = weighted.mean(urban, w = wgt, na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(urban = round(urban))
  }
  else {
    out_df <- dat_df %>% group_by(region, region_name) %>% 
      summarise(urban = weighted.mean(urban, w = wgt, na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(urban = round(urban))
  }
  return(out_df)
}

#****************************************************************************
#* make urban status for each geographical unit using the second wave
#****************************************************************************

f_nitelite <- function(dat_df = nl_df, yr_start, yr_end, lvl = 2) {
  # make min and max years be in the data range
  yr_start <- max(yr_start, 1992)
  yr_end <- min(yr_end, 2013)
  
  geo_name <- sym(paste0(g_sym(lvl), "_name"))
  
  # night light in first observed year (1992)
  nl_t0 <- dat_df %>% filter(year == 1992) %>%
    select({{geo_name}}, ntla, ntlacc, ntlv, ntlvcc) %>%
    rename_with(~paste0("t0_", .x), starts_with("ntl")) %>%
    mutate(t0_log_ntlv = log(1 + t0_ntlv))

  nl_start <- dat_df %>% filter(year == yr_start) %>%
    select({{geo_name}}, ntla, ntlv) %>%
    rename_with(~paste0("bl_", .x), starts_with("ntl")) %>%
    mutate(bl_log_ntlv = log(1 + bl_ntlv))
  
  nl_end <- dat_df %>% filter(year == yr_end) %>%
    select({{geo_name}}, ntla, ntlv) %>%
    rename_with(~paste0("el_", .x), starts_with("ntl")) %>%
    mutate(el_log_ntlv = log(1 + el_ntlv))
  
  delta_nl <- full_join(nl_start, nl_end) %>%
    full_join(nl_t0) %>%
    # everything is annualized here
    mutate(
      delta_ntla = (el_ntla - bl_ntla) / (yr_end - yr_start),
      delta_ntlv = (el_ntlv - bl_ntlv) / (yr_end - yr_start),
      delta_log_ntlv = (el_log_ntlv - bl_log_ntlv) / (yr_end - yr_start),
      # lagged changes
      delta_lag_ntla = (bl_ntla - t0_ntla) / (yr_start - 1992),
      delta_log_lag_ntlv = (bl_log_ntlv - t0_log_ntlv) / (yr_start - 1992)) %>%
    # select(-starts_with("delta")) %>%
    {.}
}

#****************************************************************************
#* make change in log area and share cropped
#****************************************************************************

f_croparea <- function(dat_df, yr_start, yr_end, lvl = 2) {
  
  geo_name <- sym(paste0(g_sym(lvl), "_name"))
  
  delta_crop_df <- dat_df %>%
    filter(year %in% c(yr_start,yr_end)) %>%
    mutate(
      # make in terms of hectares (1000/900)
      total_area = npix * 0.9,
      croparea = sharecrop * total_area,
      log_share = ifelse(sharecrop != 0, log(sharecrop), log(0.01/total_area)),
      log_croparea = ifelse(croparea != 0, log(croparea), log(0.01))) %>%
    arrange({{geo_name}}, year) %>%
    mutate(
      # this is the same because the denominator drops out
      delta_log_croparea = log_croparea - lag(log_croparea),
      delta_log_share = log_share - lag(log_share)) %>%
    filter(year == yr_end) %>%
    select({{geo_name}}, delta_log_share)
  
}

#*******************************************************************************
# f_custom_loss(dat_df, geo, yr_start, yr_end)
# calculate the average and total tree loss for a geographical unit
#*******************************************************************************
geo_forestloss <- function( dat_df, lvl, yr_start, yr_end) {
  
  if (lvl == 2) { geo <- sym("district_name") }
  else { geo <- sym("region_name") }
  
  # get baseline forest cover
  if (yr_start > 2001) {
    bl_df <- dat_df %>%
      filter(year < yr_start) %>%
      group_by({{geo}}) %>%
      summarise(treecover = mean(treecover),
                loss = sum(loss), .groups = 'drop') %>%
      mutate(bl_treecover = treecover - loss) %>%
      select({{geo}}, bl_treecover)
  }
  # if first year is 2001, no forest loss before period
  else {
    bl_df <- dat_df %>% select({{geo}}, treecover) %>% unique() %>%
      rename(bl_treecover = treecover)
  }
  
  
  out_df <- dat_df %>%
    filter(year >= yr_start & year <= yr_end) %>%
    mutate(log_loss = ifelse(loss != 0, log(loss), 0)) %>%
    group_by({{geo}}) %>%
    summarise(
      # 1. average log loss
      avg_log_loss = mean(log_loss),
      # 2. log average loss
      avg_loss = mean(loss),
      # 3. log total loss
      total_loss = sum(loss),
      treecover = mean(treecover), .groups = 'drop') %>%
    full_join(bl_df) %>%
    mutate(range = paste0(yr_start, "-", yr_end),
           pct_loss = ifelse(bl_treecover != 0, total_loss / bl_treecover, NA),
           ann_pct_loss = ifelse(is.na(pct_loss) == FALSE, 
                                 pct_loss / (yr_end - yr_start + 1), NA),
           delta_log_loss = log(bl_treecover) - log(bl_treecover - total_loss),
           # give 1 to places with no loss and take logs
           # delta log loss is equiv to log total loss
           across(c(avg_loss, total_loss), 
                  ~ifelse(.x == 0, 0, log(.x)), .names = "log_{.col}")) 
}

#*******************************************************************************
##### f_bartik(mig_df, pop_df, loss_df, dist_df, ctry3)
#*******************************************************************************
#* birth_df = birth_od, birth_dlta_pop = pop_df, birth_lvl = "district", 
f_bartik <- function(ctry3, mig_df = mig_od, dlta_pop = pop_df, 
                     fl_df = loss_df, dist_df = distance_df, 
                     emigration_df = emig_o, urban_df = urb_df, 
                     nitelite_df = delta_nl, crop_df = delta_crop, lvl = 2) {
  # vars I need to create
  # 1. migration from o-d in base period
  # 2. total migration into d in base period
  # 3. the population in d in both periods
  # 4. the population in o in both periods
  # 5. the log forest loss between the two periods 
  
  geo <- g_sym(lvl)
  prev_geo <- g_sym(lvl, r = "p")
  geo_name <- sym(paste0(g_sym(lvl), "_name"))
  prev_geo_name <- sym(paste0(g_sym(lvl, r = "p"), "_name"))
  
  # step 1 create the bartik instruments
  Btk_df <- full_join(dlta_pop, mig_df) %>%
    full_join(dist_df) %>%
    #full_join(avg_dist_df) %>%
    filter({{geo}} != {{prev_geo}}) %>%
    mutate(
      # create holders for the shares
      inv_dist_shares = (1 / distance),
      avg_dist_shares = (1 / distance) / (total_dist_inv),
      d_shares  = (m_od_all / m_d_all),
      o_shares  = (m_od_all / m_o_all),
      # Bartiks for change in dest pop / orig baseline pop
      Bm_d    = d_shares * (delta_Nd_avg / N_o0),
      Bm_o    = o_shares * (delta_Nd_avg / N_o0),
      Bd      = inv_dist_shares * (delta_Nd_avg / N_o0),
      Bd_a    = avg_dist_shares * (delta_Nd_avg / N_o0),
      # Bartiks for delta log pop at destination
      Bm_d_dlp = d_shares * (delta_log_pop_d_avg),
      Bm_o_dlp = o_shares * (delta_log_pop_d_avg),
      Bd_dlp   = inv_dist_shares * (delta_log_pop_d_avg),
      Bd_a_dlp = avg_dist_shares * (delta_log_pop_d_avg),
      # other miscellaneous variants
      Bm_d_dom    = (m_od_dom / m_d_dom)* (delta_Nd_avg / N_o0),
      Bm_d_nonag  = d_shares * (delta_Nd_nonag / N_o0),
      Bd_nonag    = inv_dist_shares * (delta_Nd_nonag / N_o0),
      Bm_d_rural  = d_shares * (delta_Nd_urban / N_o0),
      Bm_d_ag     = d_shares * (delta_Nd_ag / N_o0),
      Bd_rural    = inv_dist_shares * (delta_Nd_urban / N_o0),
      Bd_ag       = inv_dist_shares * (delta_Nd_ag / N_o0),
      Bm_d_mining = d_shares * (delta_Nd_mining / N_o0),
      # adding 1 to avoid divide by zero errors
      # Bm_ag2     = (m_od_ag / (1 + m_d_ag))* (delta_Nd_ag / N_o0),
      # Bm_nonag2  = (m_od_nonag / m_d_all)* ((N1_nonag - m_od_nonag) / N_o0),
      # Bm_mining2 = (m_od_mining / (1 + m_d_mining)) * (delta_Nd_mining / N_o0),
      # Bm_other   = (m_od_nam / m_d_nam) * (delta_Nd_nam / N_o0),
      # Bm_u       = (m_od_u / m_d_u) * (delta_Nd_u / N_o0),
    ) %>%
    group_by({{prev_geo}}) %>%
    summarise(across(c(starts_with("Bm"), starts_with("Bd")), 
                     ~sum(.x, na.rm = TRUE)),
              d_shares_sum = sum(d_shares),
              avg_inv_dist = mean(avg_dist_inv),
              total_inv_dist = mean(total_dist_inv),
              .groups = 'drop') %>%
    rename({{geo}} := {{prev_geo}})
  
  # bartik for immigration
  B_i_df <- mig_df %>%
    rename_with(~sub("prev", "next", .x), starts_with("prev")) %>%
    rename_with(~paste0("prev_", .x), 
                !c(starts_with("next"), starts_with("m"))) %>%
    rename_with(~sub("next_", "", .x), starts_with("next")) %>%
    rename_with(~sub("m_od", "m_do", .x), starts_with("m_od")) %>%
    full_join(dlta_pop) %>%
    mutate(
      Bi_all = (m_do_all / m_d_all)* (delta_Nd_avg / N_o0),
      Bi_dom = (m_do_dom / m_d_dom)* (delta_Nd_avg / N_o0),
      Bi_ag = (m_do_all / m_d_all)* (delta_Nd_ag / N_o0),
      Bi_nonag = (m_do_all / m_d_all)* (delta_Nd_nonag / N_o0),
      Bi_rural = (m_do_all / m_d_all)* (delta_Nd_urban / N_o0)) %>%
    filter({{geo}} != {{prev_geo}}) %>%
    group_by({{prev_geo}}) %>%
    summarise(across(starts_with("Bi"), ~sum(.x, na.rm = TRUE)),
              .groups = 'drop') %>%
    rename({{geo}} := {{prev_geo}})
  
  # bartik for nightlight
  geo_name <- sym(paste0(geo, "_name"))
  prev_geo_name <- sym(paste0(prev_geo, "_name"))
  
  B_nl_df <- full_join(mig_df, nitelite_df) %>%
    rename_with(~paste0(.x, "_d"), contains("ntl")) %>%
    full_join(
      nitelite_df %>% 
        rename({{prev_geo_name}} := {{geo_name}})) %>%
    full_join(dist_df) %>%
    #full_join(avg_dist_df) %>%
    full_join(dlta_pop) %>%
  mutate(
    # shares to multiply the shifts
    d_shares  = (m_od_all / m_d_all),
    o_shares  = (m_od_all / m_o_all),
    inv_dist_shares = (1 / distance),
    avg_dist_shares = (1 / distance) / (total_dist_inv),
    # Bartik with dest share, origin nitelight denominator shift
    Bd_ntla1 = inv_dist_shares * (delta_ntla_d / (1 + bl_ntla)),
    Bd_ntlv1 = inv_dist_shares * (delta_log_ntlv_d / (1 + bl_log_ntlv)),
    Bm_ntla1 = d_shares * (delta_ntla_d / (1 + bl_ntla)),
    Bm_ntlv1 = d_shares * (delta_log_ntlv_d / (1 + bl_log_ntlv)),
    
    Bd_ntlaNo = inv_dist_shares * (delta_ntla_d / N_o0),
    Bd_ntlvNo = inv_dist_shares * (delta_log_ntlv_d / N_o0),
    Bd_ntla   = inv_dist_shares * (delta_ntla_d),
    Bd_ntlv   = inv_dist_shares * (delta_log_ntlv_d),
    Bm_ntlaNo = d_shares * (delta_ntla_d / N_o0),
    Bm_ntlvNo = d_shares * (delta_log_ntlv_d / N_o0),
    Bm_ntlaoNo = o_shares * (delta_ntla_d / N_o0),
    Bm_ntlvoNo = o_shares * (delta_log_ntlv_d / N_o0),
    Bm_ntlv = d_shares * (delta_log_ntlv_d),
    Bm_ntla = d_shares * (delta_ntla_d),
    Bm_ntlvo = o_shares * (delta_log_ntlv_d),
    Bm_ntlao = o_shares * (delta_ntla_d),
    Bd_avg_ntlv = avg_dist_shares * (delta_log_ntlv_d),
    Bd_avg_ntla = avg_dist_shares * (delta_ntla_d),
    # lagged ntl changes
    Bm_ntlv_lag = d_shares * (delta_log_lag_ntlv_d),
    Bm_ntla_lag = d_shares * (delta_lag_ntla_d),
    Bm_ntlvo_lag = o_shares * (delta_log_lag_ntlv_d),
    Bm_ntlao_lag = o_shares * (delta_lag_ntla_d),
    Bd_avg_ntlv_lag = avg_dist_shares * (delta_log_lag_ntlv_d),
    Bd_avg_ntla_lag = avg_dist_shares * (delta_lag_ntla_d),
    Bd_ntla_lag   = inv_dist_shares * (delta_lag_ntla_d),
    Bd_ntlv_lag   = inv_dist_shares * (delta_log_lag_ntlv_d),
  ) %>%
    filter({{geo}} != {{prev_geo}}) %>%
    group_by({{prev_geo}}) %>%
    summarise(across(c(starts_with("Bm_"), starts_with("Bd_")), 
                     ~sum(.x, na.rm = TRUE)),
              .groups = 'drop') %>%
    rename({{geo}} := {{prev_geo}})
  
  # change in population at origin
  orig_pop <- dlta_pop %>%
    filter({{geo}} == {{prev_geo}}) %>%
    # mutate(log_gr = log((growth_rate)^(period_len)),
    #        log_gr_ann = log(growth_rate),
    #        log_gr_ag = delta_ag_pop^(1/period_len),
    #        log_gr_rural = delta_rural_pop^(1/period_len)) %>%
    rename(lag_pa = N_o0) %>%
    select({{geo}}, starts_with("delta"), starts_with("log_gr"), 
           ends_with("wgt"), starts_with("log_age_")) %>%
    # change in cohort pop
    mutate(delta_log_age = log_age_ypl - log_age_opl)
  
  # baseline characteristics
  bl_df <- full_join(dlta_pop, mig_df) %>%
    filter({{geo}} == {{prev_geo}}) %>%
    mutate(bl_emig_rate = 100 * bl_emig / bl_pa ) %>%
    select({{geo}}, bl_emig_rate, bl_emig, bl_pop, bl_pa)
  
  distset <- svy_districts(dlta_pop, l = lvl) %>%
    select(-starts_with("prev")) %>%
    # select(-distset) %>%
    unique()
  
  # put everything together
  out_df <- full_join(distset, orig_pop) %>%
    full_join(emigration_df) %>%
    full_join(bl_df) %>%
    full_join(fl_df) %>%
    full_join(crop_df) %>%
    full_join(urban_df) %>%
    full_join(nitelite_df) %>%
    full_join(Btk_df) %>%
    full_join(B_i_df) %>%
    full_join(B_nl_df) %>%
    mutate(
      Country = ctry3,
      country_name = n_ctry(ctry3),
      el_emig_rate = 100 * e_o / bl_pa, 
      across(c(starts_with("Bi"), starts_with("Bm"), starts_with("Bd")), 
             ~log(1 + .x), .names = "log_{.col}"),
      log_area = log(total_area),
      log_urban = log(1+ urban_area),
      rural_pop = rural_wgt / pop_wgt,
      ag_pop = ag_wgt / pop_wgt,
      # make missing for countries that don't have ag in both wages
      across(ends_with("ag"), ~ifelse(log_gr_ag %in% c(-Inf, Inf), NA, .x)),
      across(ends_with("rural"), ~ifelse(log_gr_rural %in% c(-Inf, Inf), NA, .x)),
      #across(starts_with("B"), ~log(.x), .names = "log_{.col}_unadj"),
           across(ends_with("loss"), ~ifelse(is.na(.x) == TRUE, 0, .x))) %>%
    drop_na(distset) %>%
    select(-distset) 

}

#*******************************************************************************
##### add_labs(dat_df)
#*******************************************************************************

add_labs <- function(dat_df) {
  out_df <- dat_df %>%
    var_labels(ag_wgt = "agricultural employment weight",
               rural_wgt = "rural population weight",
               pop_wgt = "population weight",
               el_emig_rate = "endline emigration rate",
               treecover = "hectares of tree cover in 2000",
               bl_treecover = "hectares of tree cover at start of period",
               delta_Nd = "change in prime age pop",
               delta_Nd_avg = "change in prime age pop per year",
               delta_Nd_ag = "change in ag empl pop per year",
               delta_Nd_nonag = "change in nonag empl pop per year",
               delta_Nd_mining = "change in mining employment per year",
               delta_Nd_nonmining = "change in nonmining employment per year",
               delta_Nd_nam = "change in non-ag, non-mining employment per year",
               delta_Nd_empl = "change in employment per year",
               delta_Nd_rural = "change in rural pop per year",
               delta_Nd_urban = "change in urban pop per year",
               log_gr = "log period 2 total pop / period 1 total pop",
               log_gr_ann = "log gr annualized rate",
               log_gr_ag = "annualized growth rate of ag employment",
               log_gr_mining = "annualized growth rate of mining employment",
               log_gr_nam = "annualized growth rate of non-ag, non-mining employment",
               log_gr_empl = "annualized growth rate of employment",
               log_gr_rural = "annualized growth rate of rural pop",
               e_o = "average annual emigration",
               e_o = "average annual emigration",
               log_emig = "log 1 + average annual emigration",
               i_o = "average annual immigration",
               log_immig = "log 1 + average annual immigration",
               net_log_mig = "log immigration - log emigration",
               ihsine_mig = "inverse hyperbolic sine net migration",
               # log_Bm_all = "$\\log\\text{B}_{m}^{\\text{all}}$",
               log_Bi_all = "$\\log\\text{B}_{i}^{\\text{all}}$",
               # log_Bd_all = "$\\log\\text{B}_{d}^{\\text{all}}$",
               # log_Bd_avg = "$\\log\\text{B}_{d}^{\\text{avg}}$",
               log_Bm_d_dom = "$\\log\\text{B}_{m}^{\\text{dom}}$",
               log_Bm_o = "$\\log\\text{B}_{m}^{\\text{o}}$",
               # Bm_all = "$\\text{B}_{m}^{\\text{all}}$",
               Bm_o = "$\\text{B}_{m}^{\\text{o}}$",
               # Bd_avg = "$\\text{B}_{d}^{\\text{avg}}$",
               # Bd_avgo = "$\\text{B}_{d}^{\\text{avg,o}}$",
               log_Bi_dom = "$\\log\\text{B}_{i}^{\\text{dom}}$",
               log_Bm_d_rural = "$\\log\\text{B}_{m}^{\\text{rur}}$",
               log_Bi_rural = "$\\log\\text{B}_{i}^{\\text{rur}}$",
               log_Bd_rural = "$\\log\\text{B}_{d}^{\\text{rur}}$",
               log_Bm_d_ag = "$\\log\\text{B}_{m}^{\\text{ag}}$",
               log_Bi_ag = "$\\log\\text{B}_{i}^{\\text{ag}}$",
               log_Bd_ag = "$\\log\\text{B}_{d}^{\\text{ag}}$",
               log_Bm_d_nonag = "$\\log\\text{B}_{m}^{\\text{nonag}}$",
               log_Bi_nonag = "$\\log\\text{B}_{i}^{\\text{nonag}}$",
               log_Bd_nonag = "$\\log\\text{B}_{d}^{\\text{nonag}}$",
               log_Bm_d_mining = "$\\log\\text{B}_{m}^{\\text{mining}}$",
               #log_Bm_d_other = "$\\log\\text{B}_{m}^{\\text{other}}$",
               #log_Bm_u = "$\\log\\text{B}_{m}^{\\text{u}}$",
               log_Bm_ntla = "$\\log\\text{B}_{m}^{\\text{ntla}}$",
               log_Bm_ntlv = "$\\text{B}_{m}^{\\text{ntlv}}$",
               log_Bd_ntla = "$\\log\\text{B}_{d}^{\\text{ntla}}$",
               log_Bd_ntlv = "$\\log\\text{B}_{d}^{\\text{ntlv}}$",
               log_Bm_ntla1 = "$\\log\\text{B}_{m}^{\\text{ntla}}$",
               log_Bm_ntlv1 = "$\\log\\text{B}_{m}^{\\text{ntlv}}$",
               log_Bd_ntla1 = "$\\log\\text{B}_{d}^{\\text{ntla}}$",
               log_Bd_ntlv1 = "$\\log\\text{B}_{d}^{\\text{ntlv}}$",
               log_Bm_ntlaNo = "$\\log\\text{B}_{m}^{\\text{ntla}}$",
               log_Bm_ntlvNo = "$\\log\\text{B}_{m}^{\\text{ntlv}}$",
               log_Bd_ntlaNo = "$\\log\\text{B}_{d}^{\\text{ntla}}$",
               log_Bd_ntlvNo = "$\\log\\text{B}_{d}^{\\text{ntlv}}$",
               Bm_ntlaNo = "$\\text{B}_{m}^{\\text{ntla}}$",
               Bm_ntlvNo = "$\\text{B}_{m}^{\\text{ntlv}}$",
               Bd_ntlaNo = "$\\text{B}_{d}^{\\text{ntla}}$",
               Bd_ntlvNo = "$\\text{B}_{d}^{\\text{ntlv}}$",
               Bm_ntlao = "$\\text{B}_{m}^{\\text{ntla,o}}$",
               Bm_ntlvo = "$\\text{B}_{m}^{\\text{ntlv,o}}$",
               Bd_avg_ntla = "$\\text{B}_{d}^{\\text{avg,ntla}}$",
               Bd_avg_ntlv = "$\\text{B}_{d}^{\\text{avg,ntlv}}$",
               log_age_ypl = "log baseline youth pop that will be prime age",
               log_age_opl = "log baseline aging pop that will not be prime age",
               log_age_5_15 = "log baseline pop age 5-14",
               log_age_55_65 = "log baseline pop age 55-64",
               log_age_0_15 = "log baseline pop age 0-14",
               log_gr = "$\\Delta$ log pop",
               avg_log_loss = "log loss",
               log_emig = "log emig",
               Country = "country code 3",
               range = "tree loss sample period",
               total_area = "total area of geographic unit",
               urban_area = "built up urban area in geographic unit",
               urban_share = "share of area built up urban",
               delta_log_age = "$\\Delta$ log cohort pop",
               log_area = "log area",
               log_urban = "log urban area",
               rural_pop = "rural pop share",
               ag_pop = "agricultural pop share",
               bl_ntla = "baseline nighttime light area",
               bl_ntlv = "baseline nighttime light value",
               el_ntla = "endline nighttime light area",
               el_ntlv = "endline nighttime light value",
               delta_ntla = "change in nighttime light area",
               delta_ntlv = "change in nighttime light value",
               total_inv_dist = "$\\sum\\text{dist}_{od}^{-1}$",
               avg_inv_dist = "avg dist$^{-1}$",
               # Bd_odlp = "$\\text{B}_{d}^{\\text{dlp}}$",
               # Bd_odlpav = "$\\text{B}_{d}^{\\text{dlp}}$",
               d_shares_sum = "$\\sum (m_{od}/m_d)$",
               # Bd_all = "$\\text{B}_{d}^{\text{all}}$",
               # Bm_d_dlp = "$\\text{B}_{m}^{\\text{dlp}}$",
               Bm_ntla = "$\\text{B}_{m}^{\\text{ntla}}$",
               Bd_ntla = "$\\text{B}_{d}^{\\text{ntla}}$",
               Bm_ntlv = "$\\text{B}_{m}^{\\text{ntlv}}$",
               Bd_ntlv = "$\\text{B}_{d}^{\\text{ntlv}}$",
               # Bartiks for change in dest pop / orig baseline pop
               Bm_d = "$\\sum (m_{od}/m_d)\\times\\Delta N_d/N{o0}$",
               Bm_o = "$\\sum (m_{od}/m_o)\\times\\Delta N_d/N{o0}$",
               Bd = "$\\sum (1/d_{od})\\times\\Delta N_d/N{o0}$",
               Bd_a = "$\\sum (d^{-1}_{od}/\bar{d}^{-1}_{od})\\times\\Delta N_d/N{o0}$",
               # Bartiks for delta log pop at destination
               Bm_d_dlp = "$\\sum (m_{od}/m_d)\\times\\Delta\\log\\text{pop}$",
               Bm_o_dlp = "$\\sum (m_{od}/m_o)\\times\\Delta\\log\\text{pop}$",
               Bd_dlp   = "$\\sum (1/d_{od})\\times\\Delta\\log\\text{pop}",
               Bd_a_dlp = "$\\sum (d^{-1}_{od}/\bar{d}^{-1}_{od})\\Delta\\log\\text{pop}",)
}

#*******************************************************************************
##### #. To match benin 
#*******************************************************************************
f_bartik_ben <- function(mig_df, pop_dat, loss_df, dist_df, lvl = "district", ctry3) {
  # vars I need to create
  # 1. migration from o-d in base period
  # 2. total migration into d in base period
  # 3. the population in d in both periods
  # 4. the population in o in both periods
  # 5. the log forest loss between the two periods 
  
  # step 1: country specific adjustments
  if (ctry3 %in% c("BEN", "SEN")) {
    period_len <- 11
  }
  
  if (ctry3 %in% c("MOZ", "TZA")) {
    period_len <- 10
  }
  
  if (ctry3 == "SAF") {
    period_len <- 15
  }
  
  geo <- sym(lvl)
  prev_geo <- sym(paste0("prev_", lvl))
  
  # step 1 create the bartik instruments
  # dest_mig <- mig_df %>%
  #   group_by({{geo}}) %>%
  #   summarise(m_dt = sum(m_odt, na.rm = TRUE), .groups = 'drop')
  
  B_m_df <- full_join(pop_dat, mig_df) %>%
    # full_join(dest_mig, mig_df) %>%
    # full_join(pop_df) %>%
    mutate(
           # TODO delete this, I think it is wrong
           m_dt = ifelse(is.na(m_dt) | m_dt == 0, 1, m_dt),
           Bartik_sum = (m_odt / m_dt)* (Delta_N_dt / N_ot1_mig)) %>%
    filter({{geo}} != {{prev_geo}}) %>%
    group_by({{prev_geo}}) %>%
    summarise(B_m = sum(Bartik_sum, na.rm = TRUE), .groups = 'drop') %>%
    rename({{geo}} := {{prev_geo}})
  
  B_d_df <- full_join(pop_dat, dist_df) %>%
    filter({{geo}} != {{prev_geo}}) %>%
    mutate(
           distance = as.numeric(distance),
           distance = distance / 1000,
           Bartik_sum = (1 / distance)* (Delta_N_dt / N_ot1_mig)) %>%
    group_by({{prev_geo}}) %>%
    summarise(B_d = sum(Bartik_sum), .groups = 'drop') %>%
    rename({{geo}} := {{prev_geo}})
  
  # change in population at origin
  orig_pop <- pop_dat %>%
    filter({{geo}} == {{prev_geo}}) %>%
    mutate(delta_log_pop = log(N_ot / N_ot1),
           growth_rate = (N_ot / N_ot1 )^(1/period_len) - 1,
           #log_gr = log(1 + growth_rate),
           # adjustment to match benin data
           log_gr = log((1 + growth_rate)^(period_len-2)),
           pop_st = N_ot1,
    ) %>%
    select({{geo}}, delta_log_pop, log_gr, pop_st)
  
  distset <- svy_districts(pop_dat, l = lvl) %>%
    select(-starts_with("prev")) %>%
    # select(-distset) %>%
    unique()
  
  # put everything together
  out_df <- full_join(distset, orig_pop) %>%
    full_join(loss_df) %>%
    full_join(B_m_df) %>%
    full_join(B_d_df) %>%
    mutate(Country = ctry3,
           across(c(log_loss, avg_log_loss), ~ifelse(is.na(.x) == TRUE, 0, .x))) %>%
    drop_na(distset) %>%
    select(-distset)
  
}