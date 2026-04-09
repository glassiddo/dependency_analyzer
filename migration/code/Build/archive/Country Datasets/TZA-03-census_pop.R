# Project: Migration Africa
# Author:  Sam Marshall
# Created: March 18, 2022
# Title:   Tanzania Census Populations
# Output:  
###############################################################################

source("code/SSA_env_SetUp.R")

###############################################################################
##### Read data and identify which districts are in each sample
###############################################################################

combo_df <- readRDS(paste0(tza.dir, "Support/district_combinations.rds"))

xwalk_df <- read_dta(paste0(tza_raw.dir, "Census/Census_dist_names.dta") ) %>%
  mutate(
    d_o = str_to_title(d_o),
    d_cons = str_to_title(d_cons),
    d_o = sub("Mjini", "Urban", d_o),
    d_o = sub("Vijijini", "Rural", d_o),
    d_o = ifelse(d_o == "Urban", "Mjini", d_o),
    d_o = ifelse(d_o == "Arusha Rural", "Arusha", d_o),
    d_o = ifelse(d_o == "Babati Rural", "Babati", d_o),
    d_o = ifelse(d_o == "Chake Chake", "Chakechake", d_o),
    d_o = ifelse(d_o == "Handeni Urban", "Handeni Mji", d_o),
    d_o = ifelse(d_o == "Ikungu", "Ikungi", d_o),
    d_o = ifelse(d_o == "Kaskazini 'A'", "Kaskazini ‘A’", d_o),
    d_o = ifelse(d_o == "Kaskazini 'B'", "Kaskazini ‘B’", d_o),
    d_o = ifelse(d_o == "Kasulu Urban", "Kasulu Township Authority", d_o),
    d_o = ifelse(d_o == "Korogwe Urban", "Korogwe Township Authority", d_o),
    d_o = ifelse(d_o == "Masasi Urban", "Masasi  Township Authority", d_o),
    d_o = ifelse(d_o == "Mbarali", "Mbalali", d_o),
    d_o = ifelse(d_o == "Micheweni", "Michweweni", d_o),
    d_o = ifelse(d_o == "Misenyi", "Missenyi", d_o),
    d_o = ifelse(d_o == "Moshi Kijijini", "Moshi Rural", d_o),
    d_o = ifelse(d_o == "Mpanda", "Mpanda Rural", d_o),
    d_o = ifelse(d_o == "Mtwara Urban", "Mtwara Mikindani", d_o),
    d_o = ifelse(d_o == "Singida Mijini", "Singida Urban", d_o),
    d_o = ifelse(d_o == "Tanga", "Tanga Urban", d_o),
    d_o = ifelse(d_o == "Urambo", "Uramba", d_o),
  ) %>%
  full_join(combo_df) %>%
  mutate(district_name = ifelse(d_o == "Ushetu", "KAHAMA", district_name)) 

###############################################################################
##### Create Census district growth rates, pop in 2000
###############################################################################

# kishapu not in census for some reason, extract pop data from TZA census website
kishapu_df <- read_excel(paste0(tza_raw.dir, "Census/Kishapu_pop.xlsx"),
                         range = "A1:D18") %>%
  rename("pop_02" = "2002", "pop_12" = "2012", "samp" = "prime age") %>%
  filter(samp == 1) %>%
  mutate(pop_02 = pop_02 * -1) %>%
  summarise(across(c(pop_02, pop_12), ~sum(.x))) %>%
  mutate(district_name = "Kishapu",
         growth_rate = (pop_12 / pop_02)^(1/10) - 1,
         pop_00 = pop_12 / (1 + growth_rate)^12) %>%
  select(-pop_02, -pop_12)

# calculate district growth rate based on consistent district
pop_gr <- read_dta(paste0(tza_raw.dir, "Census/Census.dta") ) %>%
  filter(age >= 15 & age <= 65) %>%
  group_by(geo2_tz, year) %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = "year",
              names_glue = "pop_{year}",
              values_from = "pop") %>%
  mutate(growth_rate = (pop_2012 / pop_2002)^(1/10) - 1,
         pop_00 = pop_2012 / (1 + growth_rate)^12) %>%
  left_join( xwalk_df ) %>%
  select(d_o, district_name, growth_rate, pop_00) %>%
  group_by(district_name) %>%
  summarise(growth_rate = weighted.mean(growth_rate, w = pop_00),
            pop_00 = sum(pop_00),
            .groups = 'drop') %>%
  bind_rows(kishapu_df)

saveRDS( pop_gr, paste0(tza.dir, "Support/census_population.rds"))

