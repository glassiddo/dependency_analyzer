#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    March 13, 2023
#* Title:   Night Light
#* Desc:    Build Night Light Dataset for each admin unit that we use
#*          1996-2013
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

f_nitelite <- function(ctry_str, lvl = 2) {
  # make min and max years be in the data range
  # year start becomes 1996 because of the rolling mean
  ctry3 <- g_ctry3(ctry_str)
  census_yrs <- g_cens_yr(ctry3)
  yr_start <- max(census_yrs[1] + 1, 1996)
  yr_end <- min(census_yrs[2], 2013, na.rm = TRUE)
  
  geo_name <- sym(paste0(g_sym(lvl), "_name"))
  
  if (lvl == 2) { geo <- sym("district_name") }
  else { geo <- sym("region_name") }
  
  ipums <- sym(paste0("geolevel", lvl))
  
  dat_df <- readRDS(
    paste0(build.dir, ctry_str, "/Support/l", lvl, "_nightlight.rds")
  ) %>%
    group_by({{ipums}}) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(across(ntla:ntlv, ~rollmean(.x, 5, align="right", fill=NA)),
           across(c(nla,nlv), ~rollmean(.x, 5, align="center", fill=NA),
                  .names = "{.col}_ra")) %>%
    ungroup() %>%
    mutate(ipums_id = as.character({{ipums}}))
  
  #**********************************************
  # night light in first observed year (1992)
  nl_t0 <- dat_df %>% filter(year == 1996) %>%
    select({{geo}}, ipums_id, ntla, ntlacc, ntlv, ntlvcc) %>%
    rename_with(~paste0("t0_", .x), starts_with("ntl")) %>%
    mutate(t0_log_ntlv = log(1 + t0_ntlv))
  
  nl_start <- dat_df %>% filter(year == yr_start) %>%
    select({{geo}}, ipums_id, ntla, ntlv) %>%
    rename_with(~paste0("bl_", .x), starts_with("ntl")) %>%
    mutate(bl_log_ntlv = log(1 + bl_ntlv))
  
  nl_end <- dat_df %>% filter(year == yr_end) %>%
    select({{geo}}, ipums_id, ntla, ntlv) %>%
    rename_with(~paste0("el_", .x), starts_with("ntl")) %>%
    mutate(el_log_ntlv = log(1 + el_ntlv))
  #**********************************************
  #harmonized part
  # full period is extended from 1994-2019
  yr_start_fp <- max(census_yrs[1] + 1, 1994)
  yr_end_fp <- min(census_yrs[2], 2019, na.rm = TRUE)
  
  nl_harm_st <- dat_df %>% filter(year == yr_start_fp) %>%
    select({{geo}}, ipums_id, starts_with("nl")) %>%
    rename_with(~paste0("bl_", .x), starts_with("nl")) %>%
    mutate(bl_log_nlv = log(1 + bl_nlv),
           bl_log_nlv_ra = log(1 + bl_nlv_ra))
  
  nl_harm_end <- dat_df %>% filter(year == yr_end_fp) %>%
    select({{geo}}, ipums_id, starts_with("nl")) %>%
    rename_with(~paste0("el_", .x), starts_with("nl")) %>%
    mutate(el_log_nlv = log(1 + el_nlv),
           el_log_nlv_ra = log(1 + el_nlv_ra))
  
  nl_harm <- full_join(nl_harm_st, nl_harm_end) %>%
    mutate(
      nl_harm_start_yr = yr_start_fp,
      nl_harm_end_yr = yr_end_fp,
      nl_harm_prd_len = yr_end_fp - yr_start_fp,
      delta_nla = (el_nla - bl_nla) / nl_harm_prd_len,
      delta_nlv = (el_nlv - bl_nlv) / nl_harm_prd_len,
      delta_log_nlv = (el_log_nlv - bl_log_nlv) / nl_harm_prd_len,
      #rolling average versions
      delta_nla_ra = (el_nla_ra - bl_nla_ra) / nl_harm_prd_len,
      delta_nlv_ra = (el_nlv_ra - bl_nlv_ra) / nl_harm_prd_len,
      delta_log_nlv_ra = (el_log_nlv_ra - bl_log_nlv_ra) / nl_harm_prd_len) %>%
    #select(-starts_with(c("bl", "el"))) %>%
    select(-starts_with(c("el")))
    
  #**********************************************
  delta_nl <- full_join(nl_start, nl_end) %>%
    full_join(nl_t0) %>%
    full_join(nl_harm) %>%
    # everything is annualized here
    mutate(
      country_name = ctry_str,
      nl_start_yr = yr_start, 
      nl_end_yr = yr_end, 
      nl_prd_len = yr_end - yr_start,
      delta_ntla = (el_ntla - bl_ntla) / nl_prd_len,
      delta_ntlv = (el_ntlv - bl_ntlv) / nl_prd_len,
      delta_log_ntlv = (el_log_ntlv - bl_log_ntlv) / nl_prd_len,
      # lagged changes
      delta_lag_ntla = (bl_ntla - t0_ntla) / (yr_start - 1996),
      delta_log_lag_ntlv = (bl_log_ntlv - t0_log_ntlv) / (yr_start - 1996)) %>%
    # select(-starts_with("delta")) %>%
    {.}
  
  # adjust ipums id for Botswana
  if (ctry_str == "Botswana") {
    delta_nl %<>%
      mutate(ipums_id = paste0("0", ipums_id))
  }
  
  return(delta_nl)
}

#*******************************************************************************
# Benin 2 ----
# ctry_df <- read_nl("Benin", 2) %>%
#   group_by(district_name) %>%
#   mutate(across(ntla:ntlv,~rollmean(.x,5,align="right",fill=NA))) %>%
#   ungroup()
ctry_df <- f_nitelite("Benin", 2)
nl_df <- ctry_df

# Botswana 1 ----
ctry_df <- f_nitelite("Botswana", 1) 
nl_df <- bind_rows(nl_df, ctry_df)

# Burkina Faso ----
ctry_df <- f_nitelite("Burkina Faso", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# Cameroon ----
ctry_df <- f_nitelite("Cameroon", 2)   
nl_df <- bind_rows(nl_df, ctry_df)

# Ghana ----
ctry_df <- f_nitelite("Ghana", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# Guinea ----
ctry_df <- f_nitelite("Guinea", 1) 
nl_df <- bind_rows(nl_df, ctry_df)

# Kenya ----
ctry_df <- f_nitelite("Kenya", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# Lesotho ----
ctry_df <- f_nitelite("Lesotho", 1) 
nl_df <- bind_rows(nl_df, ctry_df)

# Malawi ----
ctry_df <- f_nitelite("Malawi", 1) 
nl_df <- bind_rows(nl_df, ctry_df)

# Mali ----
ctry_df <- f_nitelite("Mali", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# Mauritius ----
ctry_df <- f_nitelite("Mauritius", 1) 
nl_df <- bind_rows(nl_df, ctry_df)

# Mozambique ----
ctry_df <- f_nitelite("Mozambique", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# Rwanda ----
ctry_df <- f_nitelite("Rwanda", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# Senegal ----
ctry_df <- f_nitelite("Senegal", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# Sierra Leone ----
ctry_df <- f_nitelite("Sierra Leone", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# South Africa ----
ctry_df <- f_nitelite("South Africa", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# Sudan & S. Sudan ----
ctry_df <- f_nitelite("Sudan", 1) 
nl_df <- bind_rows(nl_df, ctry_df)

# Tanzania ----
ctry_df <- f_nitelite("Tanzania", 1) 
nl_df <- bind_rows(nl_df, ctry_df)

# Togo ----
ctry_df <- f_nitelite("Togo", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# Uganda ----
ctry_df <- f_nitelite("Uganda", 1) 
nl_df <- bind_rows(nl_df, ctry_df)

# Zambia ----
ctry_df <- f_nitelite("Zambia", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# Zimbabwe ----
ctry_df <- f_nitelite("Zimbabwe", 2) 
nl_df <- bind_rows(nl_df, ctry_df)

# save ----
id_df <- readRDS(file.path(out.dir, "R/id.rds"))

nl_df %<>%
  inner_join(id_df) %>%
  relocate(country_name, ipums_id, admin_name, admin_id, geo_lvl, 
           nl_start_yr, nl_end_yr) %>%
  relocate(district_name, region_name, .after = last_col()) %>%
  var_labels(
    bl_ntla = "night time light area, baseline",
    bl_ntlv = "night time light value, baseline",
    bl_log_ntlv = "log night time light value, baseline",
    el_ntla = "night time light area, endline",
    el_ntlv = "night time light value, endline",
    el_log_ntlv = "log night time light value, endline",
    t0_ntla = "night time light area, 1996",
    t0_ntlacc = "night time light area, cc measure 1996",
    t0_ntlv = "night time light value, 1996",
    t0_ntlvcc = "night time light value, cc measure 1996",
    delta_ntla = "change in night time light area, annual",
    delta_ntlv = "change in night time light value, annual",
    delta_log_ntlv = "change in log-night time light value, annual",
    delta_lag_ntla = "change in night time light area pre-period, annual",
    delta_log_lag_ntlv = "change in log-night time light value pre-period, annual",
    nl_start_yr = "night light period start year",
    nl_end_yr = "night light period end year",
    nl_prd_len = "night light period length",
    nl_harm_end_yr = "harmonized night light period end year",
    nl_harm_prd_len = "harmonized night light period length",
    delta_nla = "change in harmonized night light area, annual",
    delta_nlv = "change in harmonized night light value, annual",
    delta_log_nlv = "change in harmonized log-night light value, annual",)


saveRDS(nl_df, file.path(out.dir, "R/nightlight.rds"))
write_dta(nl_df, file.path(out.dir, "nightlight.dta"))

