#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    March 13, 2023
#* Title:   Night Light
#* Desc:    Build Night Light Dataset for each admin unit that we use
#*          1996-2013
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

nl_df <- readRDS(paste0(build.dir, 'Africa/Sample Aggregates/nightlight.rds')) %>%
  select(-geolevel1, -geolevel2, -country)

id_df <- readRDS(here(out.dir, "R", "id.rds")) 

#*******************************************************************************
# Main File ----
samp_df <- inner_join(nl_df, id_df, by = join_by(ipums_id)) %>%
  mutate(nl_start_yr = ifelse(census_yr1 >= 1995, census_yr1 + 1, 1996),
         nl_end_yr = ifelse(is.na(census_yr2) == FALSE & census_yr2 < 2013, 
                            census_yr2, 2013),
         nl_prd_len = nl_end_yr - nl_start_yr + 1) %>% 
  group_by(country, ipums_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(across(ntla:ntlv, ~rollmean(.x, 3, align="right", fill=NA)),
         across(c(nla,nlv), ~rollmean(.x, 3, align="right", fill=NA),
                .names = "{.col}_ra")) %>%
  ungroup()


#**********************************************
## t0 ----
# night light in first observed year (1992)
nl_t0 <- samp_df %>% filter(year == 1996) %>%
  select(country, ipums_id, nl_start_yr, nl_end_yr, nl_prd_len, ntla, ntlacc, ntlv, 
         ntlvcc) %>%
  rename_with(~paste0("t0_", .x), starts_with("ntl")) %>%
  mutate(t0_log_ntlv = log(1 + t0_ntlv))

nl_start <- samp_df %>% filter(year == nl_start_yr) %>%
  select(country, ipums_id, ntla, ntlv) %>%
  rename_with(~paste0("bl_", .x), starts_with("ntl")) %>%
  mutate(bl_log_ntlv = log(1 + bl_ntlv))

nl_end <- samp_df %>% filter(year == nl_end_yr) %>%
  select(country, ipums_id, ntla, ntlv) %>%
  rename_with(~paste0("el_", .x), starts_with("ntl")) %>%
  mutate(el_log_ntlv = log(1 + el_ntlv))
#**********************************************
## harmonized part ----
# full period is extended from 1994-2019

harmony_df <- samp_df %>%
  mutate(nl_harm_start_yr = ifelse(census_yr1 >= 1994, census_yr1 + 1, 1994),
         nl_harm_end_yr = ifelse(is.na(census_yr2) == FALSE & census_yr2 < 2019, 
                                 census_yr2, 2019),
         nl_harm_prd_len = nl_harm_end_yr - nl_harm_start_yr)

nl_harm_st <- harmony_df %>% filter(year == nl_harm_start_yr) %>%
  select(country, ipums_id, starts_with("nl"), #-starts_with('nl_harm')
         ) %>%
  rename_with(~paste0("bl_", .x), c(starts_with("nl"), -starts_with('nl_harm'))) %>%
  mutate(bl_log_nlv = log(1 + bl_nlv),
         bl_log_nlv_ra = log(1 + bl_nlv_ra))

nl_harm_end <- harmony_df %>% filter(year == nl_harm_end_yr) %>%
  select(country, ipums_id, starts_with("nl"), -starts_with('nl_harm')) %>%
  rename_with(~paste0("el_", .x), starts_with("nl")) %>%
  mutate(el_log_nlv = log(1 + el_nlv),
         el_log_nlv_ra = log(1 + el_nlv_ra))

nl_harm <- full_join(nl_harm_st, nl_harm_end,
                     by = join_by(country, ipums_id)) %>%
  mutate(
    delta_nla = (el_nla - bl_nla) / nl_harm_prd_len,
    delta_nlv = (el_nlv - bl_nlv) / nl_harm_prd_len,
    delta_log_nlv = (el_log_nlv - bl_log_nlv) / nl_harm_prd_len,
    #rolling average versions
    delta_nla_ra = (el_nla_ra - bl_nla_ra) / nl_harm_prd_len,
    delta_nlv_ra = (el_nlv_ra - bl_nlv_ra) / nl_harm_prd_len,
    delta_log_nlv_ra = (el_log_nlv_ra - bl_log_nlv_ra) / nl_harm_prd_len) %>%
  select(-starts_with(c("el")))

#**********************************************
## delta ----
delta_nl <- full_join(nl_start, nl_end, by = join_by(country, ipums_id)) %>%
  full_join(nl_t0, by = join_by(country, ipums_id)) %>%
  full_join(nl_harm, by = join_by(country, ipums_id)) %>%
  # everything is annualized here
  mutate(
    delta_ntla = (el_ntla - bl_ntla) / nl_prd_len,
    delta_ntlv = (el_ntlv - bl_ntlv) / nl_prd_len,
    delta_log_ntlv = (el_log_ntlv - bl_log_ntlv) / nl_prd_len,
    # lagged changes
    delta_lag_ntla = (bl_ntla - t0_ntla) / (nl_start_yr - 1996),
    delta_log_lag_ntlv = (bl_log_ntlv - t0_log_ntlv) / (nl_start_yr - 1996)) %>%
  inner_join(id_df, by = join_by(ipums_id)) %>%
  relocate(country_name, ipums_id, admin_name, admin_id, geo_lvl, 
           nl_start_yr, nl_end_yr) %>%
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


saveRDS(delta_nl, file.path(out.dir, "R/nightlight.rds"))
write_dta(delta_nl, file.path(out.dir, "nightlight.dta"))

rm(harmony_df, delta_nl, nl_end, nl_harm, nl_harm_end, nl_harm_st, nl_start,
   nl_t0)
#*******************************************************************************
# Second Period ----

post_df <- samp_df %>% 
  mutate(
    nl_start_yr = ifelse(is.na(census_yr2) == TRUE, census_yr1 + 1, 
                         census_yr2 + 1),
    nl_end_yr = 2020,
    nl_prd_len = nl_end_yr - nl_start_yr + 1)


#**********************************************
## harmonized part ----
# full period is extended from 1994-2019

nl_harm_st <- post_df %>% filter(year == nl_start_yr) %>%
  select(country, ipums_id, starts_with("nl"), 
         -c(nl_start_yr, nl_end_yr, nl_prd_len)
  ) %>%
  rename_with(~paste0("bl_", .x), c(starts_with("nl"))) %>%
  mutate(bl_log_nlv = log(1 + bl_nlv),
         bl_log_nlv_ra = log(1 + bl_nlv_ra))

nl_harm_end <- post_df %>% filter(year == nl_end_yr) %>%
  select(country, ipums_id, starts_with("nl"), 
         -c(nl_start_yr, nl_end_yr, nl_prd_len)) %>%
  rename_with(~paste0("el_", .x), starts_with("nl")) %>%
  mutate(el_log_nlv = log(1 + el_nlv),
         el_log_nlv_ra = log(1 + el_nlv_ra))

nl_post <- full_join(nl_harm_st, nl_harm_end,
                     by = join_by(country, ipums_id)) %>%
  full_join(post_df %>% 
              select(ipums_id, nl_start_yr, nl_end_yr, nl_prd_len) %>%
              distinct()) %>%
  mutate(
    delta_nla = (el_nla - bl_nla) / nl_prd_len,
    delta_nlv = (el_nlv - bl_nlv) / nl_prd_len,
    delta_log_nlv = (el_log_nlv - bl_log_nlv) / nl_prd_len,
    #rolling average versions
    delta_nla_ra = (el_nla_ra - bl_nla_ra) / nl_prd_len,
    delta_nlv_ra = (el_nlv_ra - bl_nlv_ra) / nl_prd_len,
    delta_log_nlv_ra = (el_log_nlv_ra - bl_log_nlv_ra) / nl_prd_len) %>%
  select(-starts_with(c("el"))) %>%
  inner_join(id_df, by = join_by(ipums_id)) %>%
  relocate(country_name, ipums_id, admin_name, admin_id, geo_lvl, 
           nl_start_yr, nl_end_yr) %>%
  var_labels(
    nl_start_yr = "night light period start year",
    nl_end_yr = "night light period end year",
    nl_prd_len = "night light period length",
    delta_nla = "change in harmonized night light area, annual",
    delta_nlv = "change in harmonized night light value, annual",
    delta_log_nlv = "change in harmonized log-night light value, annual",)

saveRDS(nl_post, file.path(out.dir, "R/nightlight_period2.rds"))
write_dta(nl_post, file.path(out.dir, "nightlight_period2.dta"))

