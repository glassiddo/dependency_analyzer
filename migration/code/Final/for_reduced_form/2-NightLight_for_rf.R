#* Project: Migration Africa
#* Author:  Iddo Glass (edited from Sam Marshall's version from 2023)
#* Date:    October 22, 2025
#* Title:   Night Light
#* Desc:    Build Night Light Dataset for each admin unit that we use
#* Note:    Harmonized values are based on old dataset with old sample units
#*******************************************************************************
#* Informal explanation by Kenneth regarding the old file
### The main ntl data is from DMSP satellites since 2013.
### The DMSP satellites that were discontinued because their orbit shifted. 
### However, they kept recording data and could measure ntl at dawn
### Given that we can observe ntlv in 2013 at dawn from a discontinued satellite
### and at dusk from the last DMSP satellite in operation, 
### we can check how much ntl shifts between dawn and dusk across both satellites
### Using this factor, one can try to back-calculate all dawn measurements
### to an estimated value of what they would have been if taken at dusk
### However, this approach relies on too many shaky assumptions. 

### Hence - we have the harmonized version (using the old dataset)
### But the main data that will be used doesn't account for it

source("code/SSA_env_SetUp.R")

first_year <- 1996 # data starts in 1993, so we could use 3 years rolling avgs

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country, census_yr1, census_yr2) 
# country is used just to filter out values that exist in nl_df but not in id_df

nl_df <- read_dta(here(build.dir, "Africa", "Night Light", "SampleNTLGDP.dta")) %>% 
  mutate(
    ipums_id = ifelse(str_length(ipums_id) == 5, paste0('0', ipums_id), ipums_id)
  ) %>%
  select(ipums_id, year, ntlv, ntlla, starts_with("gdp")) 

w <- 1
nl_yrs <- readRDS(here(
  build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", w, ".rds")
)) 

nightlight_yrs <- id_df %>% 
  left_join(nl_yrs, by = "country") %>% 
  select(-wave)

samp_df <- nl_df %>% 
  left_join(nightlight_yrs, by = "ipums_id") %>% 
  group_by(ipums_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    across(
      c(ntlv, ntlla), 
      ~rollmean(.x, 3, align="right", fill=NA),
      .names = "{.col}_ra")
  ) %>%
  ungroup()

## t0 ----
# night light in first observed year (1993) 
# the old code wrote that and had 1996... perhaps due to rolling mean of 3 years
# but 1993 is the first year with data
nl_t0 <- samp_df %>% 
  filter(year == first_year) %>%
  select(
    ipums_id, ntlv, ntlla, starts_with("gdp")
    ) %>%
  rename_with(~paste0("t0_", .x), c(starts_with("ntl"), starts_with("gdp"))) %>%
  mutate(t0_log_ntlv = log(1 + t0_ntlv))

nl_start <- samp_df %>% 
  filter(year == nl_start_yr) %>%
  select(ipums_id, starts_with("ntl"), starts_with("gdp")) %>%
  rename_with(~paste0("bl_", .x), c(starts_with("ntl"), starts_with("gdp"))) %>%
  mutate(
    bl_log_ntlv = log(1 + bl_ntlv),
    bl_log_ntlv_ra = log(1 + bl_ntlv_ra)
    )

nl_end <- samp_df %>% 
  filter(year == nl_end_yr) %>%
  select(ipums_id, starts_with("ntl"), starts_with("gdp")) %>%
  rename_with(~paste0("el_", .x), c(starts_with("ntl"), starts_with("gdp"))) %>%
  mutate(
    el_log_ntlv = log(1 + el_ntlv),
    el_log_ntlv_ra = log(1 + el_ntlv_ra)
    )

nl_mix <- full_join(nl_start, nl_end, by = "ipums_id") %>%
  left_join(nightlight_yrs, by = "ipums_id") %>% 
  mutate(
    across(
      starts_with("el_"),
      ~ (. - get(str_replace(cur_column(), "^el_", "bl_"))) / nl_prd_len,
      .names = "delta_{str_remove(.col, '^el_')}"
    )
  ) # for all variables starting with el_, calculate per period deltas

#### harmonized part ----

nl_harm <- readRDS(paste0(build.dir, 'Africa/Sample Aggregates/nightlight.rds')) %>%
  select(ipums_id, year, nlv_harm = nlv, nla_harm = nla) %>% 
  left_join(id_df, by = "ipums_id") %>% # get census yrs
  filter(
    !is.na(country),
    !is.na(nla_harm) # for some reason ipums_id == "072080" has two values
    ) %>% 
  select(-country)

harm_samp_df <- nl_harm %>% 
  mutate(
    nl_harm_start_yr = ifelse(census_yr1 >= 1994, census_yr1 + 1, 1994),
    nl_harm_end_yr = ifelse(is.na(census_yr2) == FALSE & census_yr2 < 2019, 
                            census_yr2, 2019),
    nl_harm_prd_len = nl_harm_end_yr - nl_harm_start_yr
  ) %>% 
  group_by(ipums_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    across(c(nlv_harm, nla_harm), ~rollmean(.x, 3, align="right", fill=NA),
           .names = "{.col}_ra")) %>%
  ungroup()

nl_harm_years <- harm_samp_df %>% 
  select(ipums_id, nl_harm_start_yr, nl_harm_end_yr, nl_harm_prd_len) %>% 
  unique()

nl_harm_st <- harm_samp_df %>% 
  filter(year == nl_harm_start_yr) %>%
  select(ipums_id, starts_with("nl"), -contains("yr"), -contains("len")) %>% 
  rename_with(~paste0("bl_", .x), -ipums_id) %>% 
  mutate(
    bl_log_nlv_harm = log(1 + bl_nlv_harm),
    bl_log_nlv_harm_ra = log(1 + bl_nlv_harm_ra)
  )

nl_harm_end <- harm_samp_df %>% 
  filter(year == nl_harm_end_yr) %>%
  select(ipums_id, starts_with("nl"), -contains("yr"), -contains("len")) %>% 
  rename_with(~paste0("el_", .x), -ipums_id) %>% 
  mutate(
    el_log_nlv_harm = log(1 + el_nlv_harm),
    el_log_nlv_harm_ra = log(1 + el_nlv_harm_ra)
  )

harm_mix <- full_join(nl_harm_st, nl_harm_end, by = "ipums_id") %>%
  left_join(nl_harm_years, by = "ipums_id") %>% 
  mutate(
    delta_nla_harm = (el_nla_harm - bl_nla_harm) / nl_harm_prd_len,
    delta_nlv_harm = (el_nlv_harm - bl_nlv_harm) / nl_harm_prd_len,
    delta_log_nlv_harm = (el_log_nlv_harm - bl_log_nlv_harm) / nl_harm_prd_len,
    #rolling average versions
    delta_nla_harm_ra = (el_nla_harm_ra - bl_nla_harm_ra) / nl_harm_prd_len,
    delta_nlv_harm_ra = (el_nlv_harm_ra - bl_nlv_harm_ra) / nl_harm_prd_len,
    delta_log_nlv_harm_ra = (el_log_nlv_harm_ra - bl_log_nlv_harm_ra) / nl_harm_prd_len
  ) %>% 
  select(-starts_with(c("el")))

#**********************************************
## delta ----
delta_nl <- nl_mix %>% 
  full_join(nl_t0, by = "ipums_id") %>%
  full_join(harm_mix, by = "ipums_id") %>% 
  # everything is annualized here
  mutate(
    # lagged changes
    delta_lag_ntlla = (bl_ntlla - t0_ntlla) / (nl_start_yr - first_year),
    delta_log_lag_ntlv = (bl_log_ntlv - t0_log_ntlv) / (nl_start_yr - first_year)
  ) %>% 
  var_labels(
    nl_start_yr = "night light period start year",
    nl_end_yr = "night light period end year",
    nl_prd_len = "night light period length",
    bl_ntlla = "night time light area, baseline",
    bl_ntlv = "night time light value, baseline",
    bl_ntlla_ra = "night time light area, baseline, 3 years rolling average",
    bl_ntlv_ra = "night time light value, baseline, 3 years rolling average",
    bl_log_ntlv = "log night time light value, baseline",
    el_ntlla = "night time light area, endline",
    el_ntlv = "night time light value, endline",
    el_log_ntlv = "log night time light value, endline",
    t0_ntlla = "night time light area, 1996",
    t0_ntlv = "night time light value, 1996",
    delta_ntlla = "change in night light area, annual",
    delta_ntlv = "change in night light value, annual",
    delta_log_ntlv = "change in log-night light value, annual",
    delta_lag_ntlla = "change in night time light area pre-period, annual",
    delta_log_lag_ntlv = "change in log-night time light value pre-period, annual",
    delta_ntlla_ra = "change in night light area, 3 years rolling average",
    delta_ntlv_ra = "change in night light value, 3 years rolling average",
    delta_log_ntlv_ra = "change in log-night light value, 3 years rolling average",
    delta_gdp_nag_cd_hata = "change in predicted GDP in non agricultural sector, short model",
    delta_gdp_nag_cd_hatb = "change in predicted GDP in non agricultural sector, long model",
    delta_gdp_ind_cd_hata = "change in predicted GDP in industry sector, short model",
    delta_gdp_ind_cd_hatb = "change in predicted GDP in industry sector, long model",
    delta_gdp_serv_cd_hata = "change in predicted GDP in non agricultural sector, short model",
    delta_gdp_serv_cd_hatb = "change in predicted GDP in non agricultural sector, long model",
    nl_harm_start_yr = "harmonized night light period start year",
    nl_harm_end_yr = "harmonized night light period end year",
    nl_harm_prd_len = "harmonized night light period length",
    delta_nlv_harm = "change in harmonized night light value, annual",
    delta_nla_harm = "change in harmonized night light are, annual",
    delta_log_nlv_harm = "change in harmonized log-night light value, annual",
    delta_nla_harm_ra = "change in harmonized night light area, 3 years rolling average",
    delta_nlv_harm_ra = "change in harmonized night light value, 3 years rolling average",
    delta_log_nlv_harm_ra = "change in harmonized log-night light value, 3 years rolling average"
    )

saveRDS(delta_nl, here(out.dir, "R", "nightlight.rds"))
write_dta(delta_nl, here(out.dir, "nightlight.dta"))

rm(delta_nl, nl_end, nl_harm, nl_harm_end, nl_harm_st, 
   nl_harm_years, nl_start, nl_t0, nl_df, nightlight_yrs, harm_mix)

# Second Period ----

w <- 2
nl_yrs <- readRDS(here(
  build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", 2, ".rds")
)) 

nightlight_yrs <- id_df %>% 
  left_join(nl_yrs, by = "country") %>% 
  select(ipums_id, starts_with("nl_"))

post_df <- samp_df %>% 
  select(ipums_id, year, ntlv, ntlla, ntlv_ra, ntlla_ra, starts_with("gdp")) %>% 
  left_join(nightlight_yrs, by = "ipums_id")

#**********************************************
## harmonized part ----
# full period is extended from 1994-2019

nl_st <- post_df %>% 
  left_join(
    harm_samp_df %>% 
      select(-contains("yr"), -contains("len")),
    by = c("year", "ipums_id")
    ) %>% 
  filter(year == nl_start_yr) %>% 
  select(-contains("year"), -contains("yr"), -contains("len")) %>% 
  rename_with(~paste0("bl_", .x), -"ipums_id") %>% 
  mutate(
    bl_log_ntlv = log(1 + bl_ntlv),
    bl_log_ntlv_ra = log(1 + bl_ntlv_ra),
    bl_log_nlv_harm = log(1 + bl_nlv_harm),
    bl_log_nlv_harm_ra = log(1 + bl_nlv_harm_ra)
  )
  

nl_end <- post_df %>% 
  left_join(
    harm_samp_df %>% 
      select(-contains("yr"), -contains("len")),
    by = c("year", "ipums_id")
  ) %>% 
  filter(year == nl_end_yr) %>%
  select(-contains("year"), -contains("yr"), -contains("len")) %>% 
  rename_with(~paste0("el_", .x), -"ipums_id") %>% 
  mutate(
    el_log_ntlv = log(1 + el_ntlv),
    el_log_ntlv_ra = log(1 + el_ntlv_ra),
    el_log_nlv_harm = log(1 + el_nlv_harm),
    el_log_nlv_harm_ra = log(1 + el_nlv_harm_ra)
  )

nl_post <- full_join(nl_st, nl_end, by = "ipums_id") %>% 
  left_join(nightlight_yrs, by = "ipums_id") %>% 
  mutate(
    across(
      starts_with("el_"),
      ~ (. - get(str_replace(cur_column(), "^el_", "bl_"))) / nl_prd_len,
      .names = "delta_{str_remove(.col, '^el_')}"
    )
  ) %>%  # for all variables starting with el_, calculate per period deltas
  select(-starts_with(c("el"))) %>% 
  var_labels(
    nl_start_yr = "night light period start year",
    nl_end_yr = "night light period end year",
    nl_prd_len = "night light period length",
    delta_ntlla = "change in night light area, annual",
    delta_ntlv = "change in night light value, annual",
    delta_log_ntlv = "change in log-night light value, annual",
    delta_gdp_nag_cd_hata = "change in predicted GDP in non agricultural sector, short model",
    delta_gdp_nag_cd_hatb = "change in predicted GDP in non agricultural sector, long model",
    delta_gdp_ind_cd_hata = "change in predicted GDP in industry sector, short model",
    delta_gdp_ind_cd_hatb = "change in predicted GDP in industry sector, long model",
    delta_gdp_serv_cd_hata = "change in predicted GDP in non agricultural sector, short model",
    delta_gdp_serv_cd_hatb = "change in predicted GDP in non agricultural sector, long model",
    delta_nla_harm = "change in harmonized night light area, annual",
    delta_nlv_harm = "change in harmonized night light value, annual",
    delta_log_nlv_harm = "change in harmonized log-night light value, annual"
  )

saveRDS(nl_post, here(out.dir, "R", "nightlight_period2.rds"))
write_dta(nl_post, here(out.dir, "nightlight_period2.dta"))

