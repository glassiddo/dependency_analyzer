source("code/SSA_env_SetUp.R")

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country, everything()) 

# taking the second visit, totcons is nominal

# For Ethiopia and Nigeria - 
# I copied here the relevant bits from Build/LSMS/Shift_share_function.R
# In that code, the consumption levels are filtered to urban only
# This code includes both rural and urban
# all by household size? not by adult equivalent

## Ethiopia - 
# the variable used seems to be nominal consumption (nom_totcons_aeq)
# there is regional spatial price index (price_index_hce)
# which isn't used in the cleaning...

## Nigeria - 
# from the email of Sam, mentioned the following:
# "spatial prices and real and nominal consumption in 2018
# food conversion factors by item in the previous waves
# but this hasn't been made into a spatial price aggregate"
# however, there is totcons (Total consumption per capita) in the waves
# and it is cleaned here (copied and adjusted from presumably Eduardo?)

nga_hh <- read_dta(here(raw.dir, "Africa", "LSMS", "Nigeria", "GHS 12", "cons_agg_wave2_visit2.dta")) %>% 
  select(hhid, hhweight, totcons, state)

nga_ind <-  read_dta(here(raw.dir, "Africa", "LSMS", "Nigeria", "GHS 12", "sect1_harvestw2.dta")) %>% 
  select(state, hhid, indiv, sex = s1q2, age = s1q4) %>% 
  mutate(
    ae = get_adulteq(age, sex)
  ) %>% 
  group_by(hhid) %>% 
  reframe(
    ae = sum(ae, na.rm = T)
  ) 

nga_hh <- nga_hh %>% 
  left_join(nga_ind, by = "hhid") %>% 
  mutate(
    consumption = totcons,
    consumption_ae = totcons / ae
  ) %>% 
  group_by(state) %>% 
  reframe(
    across(
      c(consumption, consumption_ae),
      ~ weighted.mean(.x, w = hhweight, na.rm = TRUE)
    )
  ) %>% 
  mutate(across(c(starts_with("cons")), ~log(.x)))

# note - the price diffs are quite large, hence diffs between nominal and real is large
# need to check how is price_index_hce defined
eth <- read_dta(here(raw.dir, "Africa", "LSMS", "Ethiopia", "ESS 13", "cons_agg_w2.dta")) %>% 
  mutate(
    region = case_when(
      str_length(saq01) == 1 ~ paste0("25000", saq01),
      str_length(saq01) == 2 ~ paste0("2500", saq01),
      TRUE ~ NA
    ),
    consumption = total_cons_ann,
    consumption_real = total_cons_ann / price_index_hce,
    consumption_ae = nom_totcons_aeq,
    consumption_real_ae = nom_totcons_aeq / price_index_hce,
    weight = pw2
  ) %>% 
  group_by(region) %>% 
  reframe(
    across(
      c(consumption, consumption_real, consumption_ae, consumption_real_ae),
      ~ weighted.mean(.x, w = weight, na.rm = TRUE)
    )
  ) %>% 
  mutate(across(c(starts_with("cons")), ~log(.x)))

## --- Notes (in addition to email) ---

#### Nigeria and Ethiopia -----
# copied and modified from Eduardo's and Robin's code

df_nigeria <- read_dta(here(build.dir, "Africa/LSMS/INPUT_CLEAN/Nigeria.dta")) 
df_ethiopia <- read_dta(here(build.dir, "Africa/LSMS/INPUT_CLEAN/Ethiopia.dta"))

country_data <- list(
  Nigeria = df_nigeria,
  Ethiopia = df_ethiopia
)

# based on destination in UGA, at origin in NGA+ETH
param_lookup <- data.table(
  countries =   c("Nigeria", "Ethiopia"),
  wave_num = c(2, 2),
  shift_loc = c("o_state_cd",  "o_region_cd"),
  id_var = c("hh_id", "hh_id")
)

shift_cons_list <- list()
data_dir <- file.path(build.dir, "Africa", "LSMS", "APPEND_ALL")
shift_cons_all <- read_dta(list.files(data_dir, pattern = "LSMS_Household_dataset", full.names = TRUE)) %>% 
  as.data.table()

for (ctry in c("Nigeria", "Ethiopia")) {
  ### parameters
  params <- param_lookup[countries == ctry]
  shift_loc <- params$shift_loc
  wave_num <- params$wave_num
  id_var <- params$id_var
  
  df_ctry <- country_data[[ctry]]
  
  cons <- df_ctry %>% 
    filter(wave == wave_num) %>%
    distinct(!!sym(id_var), weight, !!sym(shift_loc)) %>% 
    mutate(!!id_var := as.character(!!sym(id_var))) %>% 
    rename(orig = !!sym(shift_loc)) %>%
    as.data.table()  # Convert to data.table after dplyr operations
  
  shift_cons <- shift_cons_all %>% 
    filter(country == ctry) %>%                   # Subset by country
    rename(hh_id = hh_id_merge) %>%               # Rename column
    select(hh_id, totcons_LCU) %>%               # Keep only needed columns
    inner_join(cons, by = "hh_id") %>%           # Merge with cons
    mutate(
      # Winsorisation at the upper 99th percentile
      totcons_LCU = pmin(totcons_LCU, 
                         quantile(totcons_LCU, 0.99, na.rm = TRUE))  # Winsorise
    ) %>% 
    group_by(orig) %>%
    reframe(cons = weighted.mean(totcons_LCU, weight, na.rm = TRUE)) %>%
    transmute(
      ipums_id = case_when(
        ctry == "Ethiopia" ~ paste0(231, 0, orig),
        ctry == "Nigeria" ~ paste0(566, 0, orig)
      ),
      consumption = log(cons)
    )
  shift_cons_list[[ctry]] <- shift_cons
}

shift_cons_nga <- shift_cons_list[["Nigeria"]]
shift_cons_eth <- shift_cons_list[["Ethiopia"]]
