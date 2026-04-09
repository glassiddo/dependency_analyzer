# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Feb 7, 2022
# Title:   Tanzania Panel
# Output:  Support/population.rds  LSMS/panel.rds
# Desc:    Creates panel dataset and population dataset         
###############################################################################

source("code/SSA_env_SetUp.R")

###############################################################################
##### Create panel of location and migration dates
###############################################################################

# dataset of all migration years and beginning and ending location
sample_df <- bind_rows(
  read_dta( paste0(tza.dir, "LSMS/Tanzania_2008.dta" )),
  read_dta( paste0(tza.dir, "LSMS/Tanzania_2010.dta" )),
  read_dta( paste0(tza.dir, "LSMS/Tanzania_2012.dta" )),
  read_dta( paste0(tza.dir, "LSMS/Tanzania_2014.dta" )),
  # don't use wave 5, it is too small
  #read_dta( paste0(tza_lsms.dir, "Tanzania_2019.dta" )) 
) %>%
  select(region:prev_district_name, irw_year, age, duration, female, 
         birth_district, urban, year, move_reason, panel_wgt, xwalk_id) %>%
  rename(pid = xwalk_id, wgt = panel_wgt, surv_year = year) 

panel_df <- sample_df %>%
  drop_na(duration) %>%
  mutate(
    # if missing interview year, use survey year
    irw_year = ifelse(is.na(irw_year) == TRUE, surv_year, irw_year),
    migration_year = ifelse(duration != 99, irw_year - duration, NA),
    # surveys happened over the course of the year and following year
    # make migration year survey year if it is recorded as after
    migration_year = ifelse(migration_year > surv_year, surv_year, migration_year),
    ever_migrate = ifelse(duration != 99, 1, 0),
    ever_migrate_dist = ifelse(district != birth_district, 1, 0),
    district_name = str_to_title(district_name),
    prev_district_name = str_to_title(prev_district_name),
    move_reason = str_to_title(move_reason),
    move_reason = ifelse(substring(move_reason, 1, 4) == "Work", "Work", move_reason),
    move_reason = ifelse(move_reason == "Other,Specify", "Other", move_reason),
    move_reason = ifelse(substring(move_reason, 1, 3) %in% c("Oth", "Mar"),
                         "Family Reasons", move_reason) ) %>%
  filter(age >= 15 & age <= 65) %>%
  filter(duration != 0)

saveRDS( panel_df, paste0(tza.dir, "LSMS/panel.rds"))

# create a list of the districts that were sampled in each wave, so that we only use
# those that were sampled for creating migration rates
# dist_set_df <- panel_df %>% select(district, surv_year) %>% unique() 

###############################################################################
##### Census populations
###############################################################################

tza_pop_df <- read_excel(paste0(tza_raw.dir, "Census_populations_2012.xlsx"),
                         range = "A1:E202")

names(tza_pop_df) <- c("district_name", "type", "pop_88", "pop_02", "pop_12")

dist_pop_df <- tza_pop_df %>%
  filter(type != "Region") %>%
  mutate(
    district_name = sub("Municipal", "Urban", district_name),
    district_name = sub("City", "Urban", district_name),
    district_name = ifelse(district_name == "Babati Rural", "Babati", district_name),
    district_name = ifelse(district_name == "Chake Chake", "Chakechake", district_name),
    district_name = ifelse(district_name == "Handeni Rural", "Handeni", district_name),
    district_name = ifelse(district_name == "Ilala Urban", "Ilala", district_name),
    district_name = ifelse(district_name == "Ilemela Urban", "Ilemela", district_name),
    district_name = ifelse(district_name == "Kahama Rural", "Kahama", district_name),
    district_name = ifelse(substr(district_name, 1,11)  == "Kaskazini A", "Kaskazini ‘A’", district_name),
    district_name = ifelse(substr(district_name, 1,11)  == "Kaskazini B", "Kaskazini ‘B’", district_name),
    district_name = ifelse(district_name == "Kasulu Rural", "Kasulu", district_name),
    district_name = ifelse(substr(district_name, 1, 4) == "Kati", "Kati", district_name),
    district_name = ifelse(district_name == "Kigoma-Ujiji Urban", "Kigoma Urban", district_name),
    district_name = ifelse(district_name == "Kinondoni Urban", "Kinondoni", district_name),
    district_name = ifelse(district_name == "Korogwe Rural", "Korogwe", district_name),
    district_name = ifelse(substr(district_name, 1, 6) == "Kusini", "Kusini", district_name),
    district_name = ifelse(district_name == "Mafinga Town", "Mafinga", district_name),
    district_name = ifelse(substr(district_name, 1, 9) == "Magharibi", "Magharibi", district_name),
    district_name = ifelse(district_name == "Makambako Town", "Makambako", district_name),
    district_name = ifelse(district_name == "Masasi Rural", "Masasi", district_name),
    district_name = ifelse(district_name == "Mbarali", "Mbalali", district_name),
    district_name = ifelse(district_name == "Micheweni", "Michweweni", district_name),
    district_name = ifelse(substr(district_name, 1, 5) == "Mjini", "Mjini", district_name),
    district_name = ifelse(district_name == "Mpanda Town", "Mpanda Urban", district_name),
    district_name = ifelse(district_name == "Mtwara Urban", "Mtwara Mikindani", district_name),
    district_name = ifelse(district_name == "Njombe Town", "Njombe Urban", district_name),
    district_name = ifelse(district_name == "Nyamagana Urban", "Nyamagana", district_name),
    district_name = ifelse(district_name == "Temeke Urban", "Temeke", district_name),
    district_name = ifelse(district_name == "Urambo", "Uramba", district_name),
    district_name = ifelse(district_name == "Tunduma Town", "Tunduma", district_name),
    district_name = ifelse(substr(district_name, 1, 6) == "Songwe", "Songwe", district_name)
    )

region_pop_df <- tza_pop_df %>%
  filter(type == "Region") %>%
  select(-type, -pop_88) %>%
  rename(region_name = district_name, r_pop_02 = pop_02, r_pop_12 = pop_12) %>%
  mutate(
    region_name = ifelse(region_name == "Dar es Salaam", "Dar Es Salaam", region_name),
    region_name = ifelse(substring(region_name, 1, 15) == "Kaskazini Pemba", 
                         "Kaskazini Pemba", region_name),
    region_name = ifelse(substring(region_name, 1, 16) == "Kaskazini Unguja", 
                         "Kaskazini Unguja", region_name),
    region_name = ifelse(substring(region_name, 1, 12) == "Kusini Pemba", 
                         "Kusini Pemba", region_name),
    region_name = ifelse(substring(region_name, 1, 13) == "Kusini Unguja", 
                         "Kusini Unguja", region_name),
    region_name = ifelse(substring(region_name, 1, 5) == "Mjini", 
                         "Mjini/Magharibi Unguja", region_name),
    r_pop_02 = as.numeric(r_pop_02),
    r_pop_12 = as.numeric(r_pop_12))


combo_df <- full_join(
  dist_pop_df, 
  # get district names from panel
  panel_df %>% 
    select(district, district_name, region_name) %>% 
    unique() ) %>%
  arrange(district_name) %>%
  select(-pop_88) %>%
  rename(d_o = district_name) %>%
  mutate(
    pop_02 = ifelse(pop_02 == "...", NA, pop_02),
    pop_02 = as.numeric(pop_02),
    d_o = str_to_upper(d_o),
    district_name = d_o,
    # combine districts that are combined in the data
    district_name = ifelse(d_o == "CHEMBA", "KONDOA", district_name),
    district_name = ifelse(d_o == "LONGIDO", "MONDULI", district_name),
    district_name = ifelse(d_o == "ARUSHA RURAL", "MERU", district_name),
    district_name = ifelse(d_o == "KOROGWE TOWN", "KOROGWE", district_name),
    district_name = ifelse(d_o == "HANDENI TOWN", "HANDENI", district_name),
    district_name = ifelse(d_o == "GAIRO", "KILOSA", district_name),
    district_name = ifelse(d_o == "KIBAHA TOWN", "KIBAHA", district_name),
    district_name = ifelse(d_o == "MASASI TOWN", "MASASI", district_name),
    district_name = ifelse(d_o == "MOMBA", "MBOZI", district_name),
    district_name = ifelse(d_o == "MKALAMA", "IRAMBA", district_name),
    district_name = ifelse(d_o == "IKUNGI", "SINGIDA RURAL", district_name),
    district_name = ifelse(d_o == "KALAMBO", "SUMBAWANGA RURAL", district_name),
    district_name = ifelse(d_o == "KAKONKO", "KIBONDO", district_name),
    district_name = ifelse(d_o == "BUHIGWE", "KASULU", district_name),
    district_name = ifelse(d_o == "KASULU TOWN", "KASULU", district_name),
    district_name = ifelse(d_o == "KAHAMA TOWN", "KAHAMA", district_name),
    district_name = ifelse(d_o == "KYERWA", "KARAGWE", district_name),
    district_name = ifelse(d_o == "RORYA", "TARIME", district_name),
    district_name = ifelse(d_o == "BABATI TOWN", "BABATI", district_name),
    district_name = ifelse(d_o == "NYANG'HWALE", "GEITA", district_name),
    d_o = str_to_title(d_o),
    district_name = str_to_title(district_name)) 

pop_df <- combo_df %>%
  group_by(district_name) %>%
  summarise(across(c(pop_02, pop_12), ~sum(.x, na.rm = TRUE)),
            district = mean(district, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(pop_02 = ifelse(pop_02 == 0, NA, pop_02)) %>%
  drop_na(district) %>%
  full_join(panel_df %>% select(district, district_name, region_name) %>% 
              unique() ) %>%
  drop_na(district) %>%
  full_join(region_pop_df) %>%
  drop_na(district) %>%
  mutate(
    pop_02 = ifelse(pop_02 == "...", NA, pop_02),
    pop_02 = as.numeric(pop_02),
    # step 1. get the implied growth rate - use region growth rate if new district
    growth_rate = ifelse(is.na(pop_02) == FALSE, 
                         (pop_12 / pop_02)^(1/10) - 1,
                         (r_pop_12 / r_pop_02)^(1/10) - 1 ),
    # step 2. population at time start
    pop_00 = pop_12 / (1 + growth_rate)^12 ) %>%
  select(district, growth_rate, pop_00)

# rural-urban status
urban_df <- sample_df %>%
  group_by(district, surv_year) %>%
  summarise(pop = sum(wgt),
            urban = sum(urban * wgt), .groups = 'drop') %>%
  mutate(urban = round(urban/ pop),
         surv_year = surv_year - 2000) %>%
  select(-pop) %>%
  pivot_wider(names_from = surv_year,
              names_glue = "urban_{surv_year}",
              values_from = urban) %>%
  rename(urban_08 = urban_8) %>%
  mutate(urban_14 = ifelse(is.na(urban_14) == TRUE, urban_12, urban_14),
         urban_12 = ifelse(is.na(urban_12) == TRUE, urban_14, urban_12),
         urban_10 = ifelse(is.na(urban_10) == TRUE, urban_12, urban_10),
         urban_08 = ifelse(is.na(urban_08) == TRUE, urban_10, urban_08)) %>%
  rename(urban = urban_14)

pop_df <- full_join(pop_df, urban_df)

saveRDS( pop_df, paste0(tza.dir, "Support/population.rds"))

# population for Mwanza district and DSM district
city_pop <- region_pop_df %>%
  filter(region_name %in% c("Dar Es Salaam", "Mwanza")) %>%
  rename(district_name = region_name) %>%
  mutate(district = ifelse(district_name == "Dar Es Salaam", 7, 19),
         growth_rate = (r_pop_12 / r_pop_02)^(1/10) - 1,
         pop_00 = r_pop_12 / (1 + growth_rate)^12 ) %>%
  select(-r_pop_02, -r_pop_12)

saveRDS( city_pop, paste0(tza.dir, "Support/city_population.rds"))

# save dataset of districts that were merged for mapping
combo_df %<>% select(d_o, district_name) %>%
  mutate(
    d_o = str_to_title(d_o),
    d_o = ifelse(d_o == "Arusha Rural", "Arusha", d_o),
    d_o = ifelse(d_o == "Babati Town", "Babati Urban", d_o),
    d_o = ifelse(d_o == "Handeni Town", "Handeni Mji", d_o),
    d_o = ifelse(d_o == "Kahama Town", "Kahama Township Authority", d_o),
    d_o = ifelse(d_o == "Kasulu Town", "Kasulu Township Authority", d_o),
    d_o = ifelse(d_o == "Kibaha Town", "Kibaha Urban", d_o),
    d_o = ifelse(d_o == "Korogwe Town", "Korogwe Township Authority", d_o),
    d_o = ifelse(d_o == "Masasi Town", "Masasi  Township Authority", d_o)) %>%
  filter(district_name != "Songwe")

saveRDS( combo_df, paste0(tza.dir, "Support/district_combinations.rds"))

