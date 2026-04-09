source("code/SSA_env_SetUp.R")

### assuming the following:
# 1) urban activity rate is identical to rural activity rate (in bl_urban_emp)
# 2) prime age is 15-64
# 3) industry - agriculture/mining/manufacturing/unknown - else is services
# 4) no international migrants

mdg_units <- read_sf(here(
  raw.dir, "Countries", "MDG", "Shapefiles", "level 1", 
  "MDG_adm1.shp")
) %>% 
  st_drop_geometry() %>% 
  transmute(
    ipums_id = GEOLEVEL1,
    admin_name = ADMIN_NAME
  )

### don't have employment by industry by urban/rural or by age
### need to check numbers as well...

prime_age <- TRUE

## the following were copied form here
# https://ireda.ceped.org/inventaire/format_liste_operation.php?onglet=1&Chp6=mdg-1993-rec
# urban-rural population by province from p31-32 in https://ireda.ceped.org/inventaire/ressources/mdg-1993-rec-o2_v2t1.pdf
# acitivity rates from p46 in https://ireda.ceped.org/inventaire/ressources/mdg-1993-rec-o7_v2t6.pdf
# ages from p55 in https://ireda.ceped.org/inventaire/ressources/mdg-1993-rec-o2_v2t1.pdf

### population -----
df_pop <- data.frame(
  admin_name = c("Antananarivo", "Fianarantsoa", "Toamasina", "Mahajanga", "Toliary", "Antsiranana"),
  Total = c(3601127, 2550190, 1995461, 1364793, 1772610, 954733),
  Urban = c(1085627, 422914, 434757, 283967, 398641, 164325),
  Rural = c(2515500, 2127276, 1560704, 1080826, 1373969, 780408),
  Activity_Rate = c(58.2, 61.6, 59.7, 61.6, 55.5, 65.0),
  Share_0_14 = c(43.4, 46.1, 45.2, 45.0, 44.4, 44.3),
  Share_15_64 = c(53.8, 51.0, 52.2, 51.9, 52.1, 52.3),
  Share_65_plus = c(2.8, 2.9, 2.6, 3.1, 3.6, 3.4)
) %>% 
  full_join(mdg_units, by = "admin_name")

pop <- df_pop %>% 
  transmute(
    ipums_id,
    bl_pop = if(prime_age) Total * (Share_15_64/100) else Total,
    bl_rural_pop = if(prime_age) Rural * (Share_15_64/100) else Rural,
    bl_urban_pop = if(prime_age) Urban * (Share_15_64/100) else Urban
  ) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 0)))

saveRDS(pop, here(build.dir, "Countries", "MDG", "93_pop_aggregates.rds"))

### employment -----

prime_age_groups <- c(
  "15-19","20-24","25-29","30-34","35-39",
  "40-44","45-49","50-54","55-59","60-64"
)

age_distr <- data.frame(
  # from p61 in https://ireda.ceped.org/inventaire/ressources/mdg-1993-rec-o2_v2t1.pdf
  age_group = c(
    "00-04","05-09","10-14","15-19","20-24","25-29",
    "30-34","35-39","40-44","45-49","50-54","55-59",
    "60-64","65-69","70+"
  ),
  pop = c(
    2234605, 1704564, 1526022, 1372659, 1118519, 903176,
    789734, 631357, 488602, 332625, 306080, 248217,
    218913, 147867, 215974
  )
)

prime_age_weights <- age_distr %>%
  filter(age_group %in% prime_age_groups) %>%
  transmute(age_group, weight = pop / sum(pop))

activity_data <- data.frame(
  # from p161-163 in https://ireda.ceped.org/inventaire/ressources/mdg-1993-rec-o7_v2t6.pdf
  admin_name = c(
    rep("Antananarivo", 16),
    rep("Fianarantsoa", 16),
    rep("Toamasina", 16),
    rep("Antsiranana", 16),
    rep("Mahajanga", 16),
    rep("Toliary", 16)
  ),
  age_group = rep(c(
    "ENSEMBLE", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
    "75-79", "80 et +"
  ), 6),
  activity_rate = c(
    # ANTANANARIVO
    58.2, 21.2, 49.9, 62.5, 69.9, 74.8, 78.5, 80.0, 79.4, 78.1,
    74.6, 66.1, 62.3, 56.6, 45.5, 34.2,
    # FIANARANTSOA
    61.6, 33.5, 57.0, 65.5, 70.3, 73.9, 76.3, 77.0, 76.0, 75.4,
    74.6, 70.5, 66.2, 60.1, 49.3, 35.5,
    # TOAMASINA
    59.7, 26.4, 38.5, 53.2, 63.7, 70.0, 73.3, 74.8, 75.9, 75.7,
    76.3, 75.9, 72.1, 68.0, 62.0, 50.8,
    # MAHAJANGA
    61.6, 30.7, 55.3, 65.1, 70.7, 73.3, 75.8, 76.3, 76.2, 75.9,
    75.7, 71.9, 67.7, 61.8, 51.8, 38.1,
    # TOLIARY
    55.5, 30.2, 45.7, 55.4, 61.4, 65.7, 70.0, 70.4, 71.0, 71.2,
    71.5, 69.7, 68.4, 62.0, 54.6, 42.5,
    # ANTSIRANANA
    65.0, 25.1, 54.8, 70.6, 78.1, 81.1, 83.3, 84.8, 84.8, 84.4,
    83.2, 78.8, 73.7, 66.3, 54.3, 38.9
    )
  ) 

workers_prime_age <- activity_data %>%
  filter(age_group %in% prime_age_groups) %>% 
  left_join(prime_age_weights, by = "age_group") %>% 
  group_by(admin_name) %>%
  reframe(
    activity_rate_pa = sum(activity_rate * weight)
  ) %>%
  left_join(
    df_pop %>% select(admin_name, ipums_id, Total, Share_15_64),
    by = "admin_name"
  ) %>%
  transmute(
    ipums_id,
    activity_rate_pa,
    pa_workers =
      round(Total * (Share_15_64/100) * (activity_rate_pa/100), 0)
  )

# those were downloaded from https://madagascar.opendataforafrica.org/
original <- fread(here(raw.dir, "Countries", "MDG", "Census", "1993",
"migration_industries.csv")) %>%
  transmute(
    admin_name = str_to_title(tolower(location)),
    # separate the indicator into type and value
    indicator_type = str_extract(indicator, "^[^,]+"),
    indicator_value = str_extract(indicator, "(?<=, ).+$"),
    value = Value,
    indicator
  ) %>%
  full_join(mdg_units, by = "admin_name")

urban_emp <- pop %>%
  left_join(mdg_units, by = "ipums_id") %>% 
  left_join(
    activity_data %>% 
      filter(age_group == "ENSEMBLE"),
    by = "admin_name"
  ) %>% 
  transmute(
    ipums_id,
    bl_urban_emp = round(bl_urban_pop * (activity_rate/100), 0)
  )

emp <- original %>% 
  filter(
    indicator_type == "Plugged business", 
    indicator_value %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "n.d")
    # only use parent categories, not subdivisions to avoid double counting
  ) %>% 
  mutate(
    value = case_when(
      ipums_id == "450006" & indicator_value == "0" ~ value/100,
      ipums_id == "450003" & indicator_value == "5" ~ value/100,
      # a bug that requires fixing...
      TRUE ~ value/1000),
    indgen = case_when(
      indicator_value == "0" ~ 10, # agriculture etc
      indicator_value == "1" ~ 20, # mining
      indicator_value == "2" ~ 30, # manufacturing
      indicator_value == "n.d" ~ 999, # unknown (not declared)
      TRUE ~ 110 # services (+ construction)
      )
    ) %>% 
  group_by(indgen, ipums_id) %>% 
  reframe(
    share = sum(value)
  ) %>% 
  left_join(workers_prime_age, by = "ipums_id") %>%
  transmute(
    ipums_id, indgen, ind_workers = pa_workers*share
  ) %>%
  pivot_wider(
    names_from = indgen,
    values_from = ind_workers
  ) %>%
  left_join(pop, by = "ipums_id") %>% 
  transmute(
    ## all are already normalized by the prime age, 
    ## except for bl_u which needs to take the total # of PA ppl
    ipums_id,
    bl_ag = `10`, 
    bl_nonag = (`20` + `30` + `110` + `999`),  
    bl_mining = `20`, 
    bl_nonmining = (`10` + `30` + `110` + `999`),  
    bl_mfg = `30`,
    bl_svc = `110`,
    bl_empl = (`10` + `20` + `30` + `110` + `999`),
    bl_u = bl_pop - (`10` + `20` + `30` + `110` + `999`)
  ) %>% 
  left_join(urban_emp, by = "ipums_id") %>% 
  mutate(across(where(is.numeric) & -ipums_id, ~ round(.x, 0))) 

saveRDS(emp, here(build.dir, "Countries", "MDG", "93_emp_aggregates.rds"))

### migration -----

# m_oo is number of nonmigrants in d (constant for d)
# m_od is number of migrants in d from o
# m_d  is number of migrants in d (constant for d, for a given country of o)
# m_o  is number of migrants from o (constant for o, for a given country of d)
# d_shares  = (m_od / m_d) # share of migrants from o, of all migrants to d
# o_shares  = (m_od / m_o) # share of migrants to d, of all migrants from o

## need to figure out the international variables

mig <- original %>% 
  filter(indicator_type == "Faritany birth") %>% 
  transmute(
    ipums_id, birth_location = str_to_title(indicator_value), value
  ) %>% 
  pivot_wider(
    names_from = birth_location,
    values_from = value
  ) %>%
  left_join(df_pop %>% select(ipums_id, Total), by = "ipums_id") %>% 
  rowwise() %>%
  mutate(
    across(
      Etrangers:Antananarivo, 
      ~coalesce(., Total - sum(c_across(Etrangers:Antananarivo),
                               na.rm = TRUE))
      )
  ) %>% 
  ungroup() %>%
  select(-Total, -Etrangers) %>% # ignore international migration for now
  pivot_longer(
    cols = -ipums_id,
    names_to = "admin_name",
    values_to = "m_od"
  ) %>% 
  rename(ipums_id_d = ipums_id) %>% 
  left_join(mdg_units, by = "admin_name") %>% 
  rename(ipums_id_o = ipums_id) %>% 
  group_by(ipums_id_d) %>%
  mutate(
    m_oo = m_od[ipums_id_o == ipums_id_d],
    m_d = sum(m_od[ipums_id_o != ipums_id_d]),
    d_shares = ifelse(ipums_id_o != ipums_id_d, m_od / m_d, 0),
    intm_d = sum(m_od)
  ) %>%
  ungroup() %>%
  group_by(ipums_id_o) %>%
  mutate(
    m_o = sum(m_od[ipums_id_d != ipums_id_o]),
    o_shares = ifelse(ipums_id_d != ipums_id_o, m_od / m_o, 0),
    intm_o = sum(m_od)
  ) %>%
  ungroup() %>% 
  mutate(
    intd_shares  = (m_od / intm_d),
    into_shares  = (m_od / intm_o),
    # for since birth, for now approx with 15 year avg. duration
    m_od_10years = m_od/1.5,
  ) %>%
  group_by(ipums_id_d) %>%
  mutate(
    m_d_10years = sum(m_od_10years, na.rm = TRUE),
    N0 = sum(m_od, na.rm = TRUE) + m_oo,
    m_oo_10years = N0 - m_d_10years
  ) %>%
  ungroup() %>%
  group_by(ipums_id_o) %>%
  mutate(m_o_10years = sum(m_od_10years, na.rm = TRUE)) %>%
  ungroup() %>%
  transmute(
    country_name = "Madagascar",
    prev_country_name = "Madagascar",
    ipums_id_d = as.character(ipums_id_d),
    ipums_id_o = as.character(ipums_id_o),
    mig_measure = "since birth",
    #mig_start_yr = NA,
    mig_end_yr = 1993,
    #mig_prd_len = NA,
    m_od, m_o, m_d, o_shares, d_shares, m_oo,
    intm_d, intm_o, intd_shares, into_shares, m_od_10years, m_d_10years,
    m_oo_10years, m_o_10years
  )

saveRDS(mig, here(build.dir, "Countries", "MDG", "93_mig_aggregates.rds"))
