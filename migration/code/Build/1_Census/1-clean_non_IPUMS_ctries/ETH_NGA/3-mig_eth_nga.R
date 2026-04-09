#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    December, 2025
#* Title:   Create migration variables for ETH and NGA from LSMS for 'census'
#* Desc:    Get locations in next wave and current one and time between 
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

#### this is the construction of the migration matrices 
#### based on the lsms work with various changes to be consistent with Final/
#### notes:
#### - age0/1 issue with ethiopia isnt handled - ages 0 are excluded
#### - not filtered to econ migration 
#### - filters to migration since the previous wave - hence, we need to take wave t+1

# Create final dataset ----------------------------------------------------
df_eth <- read_dta(here(build.dir, "Africa", "LSMS", "INPUT_CLEAN", "Ethiopia.dta"))
df_nga <- read_dta(here(build.dir, "Africa", "LSMS", "INPUT_CLEAN", "Nigeria.dta"))

df_list <- list(NGA = df_nga, ETH = df_eth)
param_lookup <- data.table(
  ctry =   c("NGA", "ETH"),
  ctry_code =   c(566, 231),
  origin =      c("o_state_cd", "o_region_cd"),
  destination = c("d_state_cd", "d_region_cd"),
  wave =        c(2, 3) # both countries take the next wave
)

results_list <- lapply(param_lookup$ctry, function(target_ctry) {
  ### parameters
  p <- param_lookup[ctry == target_ctry]
  dd <- df_list[[target_ctry]]  
  
  time_diff <- dd %>% 
    group_by(wave) %>%
    reframe(med_int_date = median(int_date, na.rm = TRUE)) %>%
    mutate(
      months_diff = as.numeric(med_int_date - lag(med_int_date)) / 30.42,
      months_diff = round(months_diff / 12) * 12
    )
  
  mig <- dd %>% 
    left_join(time_diff, by = "wave") %>% 
    rename(orig = !!p$origin, dest = !!p$destination) %>% 
    mutate(
      dest = ifelse(dest == "", orig, dest)
    ) %>% 
    filter(
      wave == p$wave,
      age >= min_pa, age <= max_pa,
      ) %>% 
    transmute(
      recent_mig = ifelse(
        #is_econ_mig == 1 &
        #left_country!=1 & 
        left_since_months <= months_diff &
          orig != dest # interested in inter-region migration
        , 1, 0
      ),
      ipums_id_o = paste0(p$ctry_code, 0, orig),
      ipums_id_d = paste0(p$ctry_code, 0, dest),
      mig_wgt    = ifelse(recent_mig == 1, weight, 0),
      nonmig_wgt = ifelse(recent_mig == 1, 0, weight)
    ) %>%
    group_by(ipums_id_o, ipums_id_d) %>%
    summarise(
      m_od = round(sum(mig_wgt, na.rm = T), 0),
      nonmig_od = round(sum(nonmig_wgt, na.rm = T), 0),
      .groups = "drop"
    ) %>%
    group_by(ipums_id_d) %>%
    mutate(
      m_oo = sum(nonmig_od)
    ) %>%
    ungroup() %>%
    select(-nonmig_od)
  
  m_oo_lookup <- mig %>%
    filter(ipums_id_o == ipums_id_d) %>%
    select(ipums_id_d, m_oo)
  
  all_regions <- unique(c(mig$ipums_id_o, mig$ipums_id_d))
  
  if (target_ctry == "NGA") {
    s_yr <- 2011
    e_yr <- 2012
  } else {
    s_yr <- 2013
    e_yr <- 2014
  }
  
  mig_complete <- expand.grid(
    ipums_id_o = all_regions,
    ipums_id_d = all_regions,
    stringsAsFactors = FALSE
  ) %>%
    left_join(mig %>% select(-m_oo), by = c("ipums_id_o", "ipums_id_d")) %>%
    left_join(m_oo_lookup, by = "ipums_id_d") %>% 
    mutate(
      m_od = replace_na(m_od, 0),
      m_oo = replace_na(m_oo, 0)
    ) %>%
    group_by(ipums_id_d) %>%
    mutate(
      m_d = sum(m_od)
    ) %>%
    ungroup() %>%
    group_by(ipums_id_o) %>%
    mutate(
      m_o = sum(m_od)
    ) %>%
    ungroup() %>%
    mutate(
      d_shares  = (m_od / m_d),
      o_shares  = (m_od / m_o)
    ) %>% 
    mutate(
      country_name = n_ctry(target_ctry),
      prev_country_name = n_ctry(target_ctry),
      ipums_id_d = as.character(ipums_id_d),
      ipums_id_o = as.character(ipums_id_o),
      mig_start_yr = s_yr,
      mig_end_yr = e_yr,
      mig_prd_len = 2,
      mig_measure = "fixed: 2 year"
    )
  
  yr_suff <- str_sub(e_yr, 3, 4)
  saveRDS(mig_complete, here(
    build.dir, "Countries", target_ctry, paste0(yr_suff, "_mig_aggregates.rds")
  ))
  
})
