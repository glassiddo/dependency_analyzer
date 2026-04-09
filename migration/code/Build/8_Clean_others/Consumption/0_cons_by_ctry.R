#* Project: Migration Africa
#* Author:  Sam Marshall and Iddo Glass
#* Date:    October 9, 2025
#* Title:   Get real consumption from LSMS
#* Desc:    Gets spatial variation in avg real consumption of adult eq. from LSMS
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

## load IDs
id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country, everything())

## countries included --- 
# Tanzania and Malawi - based on LSMS, values by industry
# Mali, Uganda, Ethiopia and Nigeria - based on LSMS
# South Africa - based on LSMS, adjusted from the following paper
## https://github.com/mwaugh0328/JME-Migration-Costs-2020

## countries that can be done --
# Niger (but we don't have the units)

## helper function - adult equivalent scale 
# from the migration cost paper
# https://github.com/mwaugh0328/JME-Migration-Costs-2020/blob/main/Code/Build/Scripts/ZAF_01A_chars.do
get_adulteq <- function(age, sex) {
  # sex == 2 is female, sex == 1 is male
  case_when(
    age >= 0  & age <= 2  ~ 0.40,
    age >= 3  & age <= 4  ~ 0.48,
    age >= 5  & age <= 6  ~ 0.56,
    age >= 7  & age <= 8  ~ 0.64,
    age >= 9  & age <= 10 ~ 0.76,
    age >= 11 & age <= 12 & sex == 2   ~ 0.88,
    age >= 11 & age <= 12 & sex == 1   ~ 0.80,
    age >= 11 & age <= 12 & is.na(sex) ~ 0.84,
    age >= 13 & age <= 14              ~ 1.00,
    age >= 15 & age <= 18 & sex == 2   ~ 1.00,
    age >= 15 & age <= 18 & sex == 1   ~ 1.20,
    age >= 15 & age <= 18 & is.na(sex) ~ 1.10,
    age >= 19 & age <= 59 & sex == 2   ~ 0.88,
    age >= 19 & age <= 59 & sex == 1   ~ 1.00,
    age >= 19 & age <= 59 & is.na(sex) ~ 0.94,
    age >= 60 & sex == 2               ~ 0.72,
    age >= 60 & sex == 1               ~ 0.80,
    age >= 60 & is.na(sex)             ~ 0.76,
    TRUE ~ NA_real_
  )
}

#*******************************************************************************
# Tanzania ----

# assume 6 days per week, 4.333 weeks per month
monthly_wage <- function(dat_df, payment, period, hpw, vname = wage) {
  out_df <- dat_df %>%
    mutate(wage = {{payment}},
           # hourly
           wage = if_else({{period}} == 1, wage * {{hpw}} * 52/12, wage, wage),
           # daily
           wage = if_else({{period}} == 2, wage * 6 * 52/12, wage, wage),
           # weekly
           wage = if_else({{period}} == 3, wage * 52/12, wage, wage),
           # fortnightly
           wage = if_else({{period}} == 4, wage * 26/12, wage, wage),
           # quarterly
           wage = if_else({{period}} == 6, wage / 3, wage, wage),
           # half-year
           wage = if_else({{period}} == 6, wage / 6, wage, wage),
           # annually
           wage = if_else({{period}} == 8, wage / 12, wage, wage),
           # if no period, most frequent value is paid monthly...
           # make NA 0 for summing
           wage = ifelse(is.na(wage) == TRUE, 0, wage)) %>%
    rename({{vname}} := wage)
  
}

consumpagg <- function( dat_df ) {
  dat_df %<>%
    rename(consumption = expm, consumption_real = expmR,
           food = foodbev, food_real = foodbevR) %>%
    select(contains('hhid'), consumption, consumption_real, food, food_real,
           adulteq, hhsize) %>%
    mutate(
      food_share = food/consumption,
      across(c(consumption, consumption_real, food, food_real), ~.x/adulteq,
             .names = "{.col}_ae"),
      across(starts_with(c('consumption', 'food')), ~log(.x),
             .names = "log_{.col}"),
      price = consumption / consumption_real
    )
}

tza_dir <- paste0(raw.dir, "Africa/LSMS/Tanzania/NPS 10/")

# age = hh_b04
# read_dta(paste0(tza_dir, "HH_SEC_B.dta") )

tza_df <- read_dta(paste0(tza_dir, "HH_SEC_A.dta") ) %>%
  select(y2_hhid, weight = y2_weight, region) %>%
  full_join( read_dta(paste0(tza_dir, "HH_SEC_E1.dta") )) %>%
  # drop under 5 years old
  filter(hh_e01 != 2) %>%
  mutate(# any work in the past week or a job to return to
    empl7_plus = if_else(hh_e04 == 1, 1, 0),
    empl7_plus = if_else(hh_e05 == 1, 1, empl7_plus, empl7_plus),
    # 0 indicator for any employment in last 7
    lfs7 = ifelse(is.na(empl7_plus) == FALSE, 0, NA),
    empl_wage7 = if_else(hh_e12 == 1, 1, 0, lfs7),
    # including apprenticeships for consistency
    empl_wage12 = if_else(hh_e12 == 1 | hh_e13 == 1 | hh_e44 == 1, 1, 0, lfs7),
    empl_self7 = if_else(hh_e51 == 1, 1, 0, lfs7),
    empl_self12 = if_else(hh_e51 == 1 | hh_e52 == 1, 1, 0, lfs7),
    empl_ag7   = if_else(hh_e77 == 1, 1, 0, lfs7),
    empl_ag12   = if_else(hh_e77 == 1 | hh_e79 == 1, 1, 0, lfs7),
    empl7 = pmax(empl_wage7, empl_self7, empl_ag7, na.rm = TRUE),
    empl12 = pmax(empl_wage12, empl_self12, empl_ag12, na.rm = TRUE),
    empl_nonwage = pmax(empl_self12, empl_ag12, na.rm = TRUE),
    isic = hh_e17_2,
    isic = ifelse(isic > 1000, floor(isic/100), isic),
    isic = ifelse(isic > 100, floor(isic/10), isic),
    industry = case_when(
      isic %in% c(1:3) ~ "Agriculture",
      isic %in% c(5:33,45:47) ~ "Manufacturing",
      isic %in% c(36:43,49:99) ~ "Services"
    )) %>% 
  monthly_wage(payment = hh_e22_1, period = hh_e22_2, hpw = hh_e25, vname = wage_p) %>%
  monthly_wage(payment = hh_e24_1, period = hh_e24_2, hpw = hh_e25, vname = wage_inkind) %>%
  monthly_wage(payment = hh_e37_1, period = hh_e37_2, hpw = hh_e40, vname = wage2) %>%
  monthly_wage(payment = hh_e39_1, period = hh_e39_2, hpw = hh_e40, vname = wage2_inkind) %>%
  mutate(wage = wage_p,
         wage_p_total = wage_p + wage_inkind,
         wage_total = wage_p + wage_inkind + wage2 + wage2_inkind,
         across(starts_with("wage"), ~ifelse(.x == 0, NA, .x)),
         #across(starts_with("wage"), ~winsor(.x, 0.02, 0.98)),
         income = hh_e22_1) %>%
  drop_na(y2_hhid) %>%
  select(-starts_with("hh"), -lfs7) %>%
  remove_all_labels() %>%
  drop_na(region) %>%
  left_join(
    read_dta(paste0(tza_dir, "TZY2.HH.Consumption.dta") ) %>%
      consumpagg(),
    by = join_by(y2_hhid)
  ) %>%
  # make regions consistent
  mutate(
    region = case_when(
      region %in% c(17,18) ~ 19,
      region == 21 ~ 2,
      TRUE ~ region
    )
  )

tza_region <- tza_df %>%
  group_by(region) %>%
  summarise(
    across(c(wage_total, starts_with("consumption"), price),
           ~weighted.mean(.x, w = weight, na.rm = TRUE)),
    .groups = 'drop') %>%
  rename(wage = wage_total) %>%
  mutate(across(c(wage, starts_with("consumption")), ~log(.x)))

tza_ind <- tza_df %>%
  drop_na(industry) %>%
  group_by(region, industry) %>%
  summarise(
    across(c(wage_total, starts_with("consumption"), price),
           ~weighted.mean(.x, w = weight, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  rename(wage = wage_total) %>%
  mutate(across(c(wage, starts_with("consumption")), ~log(.x)),
         industry = case_when(
           industry == "Agriculture" ~ "ag",
           industry == "Manufacturing" ~ "mfg",
           industry == "Services" ~ "svc",
         )) %>%
  pivot_wider(
    id_cols = region,
    names_from = industry,
    names_glue = "{.value}_{industry}",
    values_from = c(wage:price)
  )

tza_price_df <- id_df %>% 
  filter(country == "TZA") %>%
  mutate(region = as.numeric(str_sub(ipums_id, -2))) %>%
  full_join(tza_region) %>%
  full_join(tza_ind)

rm(tza_df, tza_ind, tza_region, tza_dir)

#*******************************************************************************

#*******************************************************************************
# Malawi ----

mwi_dir <- paste0(raw.dir, "Africa/LSMS/Malawi/IHPS 10/")

mwi_bld_dir <- paste0(build.dir, "Africa/LSMS/")

cons_pcap_df <- read_dta(
  paste0(raw.dir, "Africa/LSMS/Malawi/IHS3_2010_2011/",
         "MWI_2010_IHS-III_v01_M_STATA8/Full_Sample/",
         "ihs3fc2M_consumption.dta") ) %>%
  select(case_id, consumption_real = rexpagg, 
         consumption = expagg, price = price_index)

# cons_df <- read_dta(paste0(mwi_dir, "Round 1 (2010) Consumption Aggregate.dta") ) %>%
#   select(HHID, case_id, adulteq, hhsize, hhweight, consumption_real = rexpagg, 
#          consumption_real_ae = pcrexpagg, price = price_indexL)

cons_df <- read_dta(paste0(mwi_dir, "Round 1 (2010) Consumption Aggregate.dta") ) %>%
  select(HHID, case_id, adulteq, hhsize, hhweight)

hhid_df <- read_dta(paste0(mwi_dir, "HH_MOD_B_10.dta") ) %>%
  select(HHID, case_id, PID)

mwi_df <- read_dta(paste0(mwi_bld_dir, "MWI_FINAL_indivw1.dta") ) %>%
  mutate(
    ag = ifelse(ind_ag == 1 | ind_fish == 1, 1, 0),
    mfg = ifelse(ind_mining == 1 | ind_manuf == 1, 1, 0),
    svc = ifelse(ind_const == 1 | ind_serv == 1, 1, 0),
    across(c(ag, mfg, svc), ~ifelse(is.na(.x) == TRUE, 0, .x)),
    industry = case_when(
      ind_ag == 1 | ind_fish == 1 ~ "Agriculture",
      ind_mining == 1 | ind_manuf == 1 ~ "Manufacturing",
      ind_const == 1 | ind_serv == 1 ~ "Services"
    ),
    admin_name_l1 = case_when(
      admin_2_name %in% c("Blantyre City","Blanytyre") ~ "Blantyre, Blantyre City",
      admin_2_name %in% c("Lilongwe City","Lilongwe") ~ "Lilongwe, Lilongwe City",
      admin_2_name %in% c("Mwanza","Neno") ~ "Mwanza, Neno",
      admin_2_name %in% c("Mzimba","Mzuzu City") ~ "Mzimba, Mzuzu City",
      admin_2_name %in% c("Nkhatabay","Likoma") ~ "Nkhata Bay, Likoma",
      admin_2_name %in% c("Zomba","Zomba City") ~ "Zomba, Zomba City",
      admin_2_name == "Nkhota kota" ~ "Nkhotakota",
      TRUE ~ admin_2_name
    )
  ) %>%
  full_join(hhid_df) %>%
  # group_by(HHID, admin_name_l1) %>%
  # summarise(across(c(ag, mfg, svc), ~max(.x)),
  #           .groups = 'drop') %>%
  group_by(case_id, admin_name_l1) %>%
  summarise(across(c(ag, mfg, svc), ~max(.x)),
            .groups = 'drop') %>%
  full_join(cons_df) %>%
  left_join(cons_pcap_df) %>%
  mutate(consumption_ae = consumption/adulteq,
         consumption_real_ae = consumption_real/adulteq)

## by admin region 
mwi_admin <- mwi_df %>%
  group_by(admin_name_l1) %>%
  summarise(
    across(c(starts_with("consumption"), price),
           ~weighted.mean(.x, w = hhweight, na.rm = TRUE)),
    .groups = 'drop') %>%
  mutate(across(c(starts_with("consumption")), ~log(.x)))

mwi_ind <- mwi_df %>%
  mutate(across(c(starts_with("cons"), price), ~.x * ag, .names = "{.col}_ag"),
         across(c(consumption_real:consumption_real_ae),
                ~.x * mfg, .names = "{.col}_mfg"),
         across(c(consumption_real:consumption_real_ae),
                ~.x * svc, .names = "{.col}_svc")) %>%
  select(-c(consumption_real:consumption_real_ae)) %>%
  mutate(across(c(starts_with(c("cons", "price"))), 
                ~ifelse(.x == 0, NA, .x))) %>%
  group_by(admin_name_l1) %>%
  summarise(
    across(c(starts_with(c("cons", "price"))),
           ~weighted.mean(.x, w = hhweight, na.rm = TRUE)),
    .groups = 'drop') %>%
  mutate(across(c(starts_with("cons")), ~log(.x)))

# mwi_ind <- mwi_df %>%
#   mutate(across(c(consumption_real, consumption_real_ae, price),
#                 ~.x * ag, .names = "{.col}_ag"),
#          across(c(consumption_real, consumption_real_ae, price),
#                 ~.x * mfg, .names = "{.col}_mfg"),
#          across(c(consumption_real, consumption_real_ae, price),
#                 ~.x * svc, .names = "{.col}_svc")) %>%
#   select(-c(consumption_real, consumption_real_ae, price)) %>%
#   mutate(across(c(starts_with(c("consumption", "price"))), 
#          ~ifelse(.x == 0, NA, .x))) %>%
#   group_by(admin_name_l1) %>%
#   summarise(
#     across(c(starts_with(c("consumption", "price"))),
#            ~weighted.mean(.x, w = hhweight, na.rm = TRUE)),
#     .groups = 'drop') %>%
#   mutate(across(c(starts_with("consumption")), ~log(.x)))

mwi_price_df <- id_df %>% 
  filter(country == "MWI") %>%
  full_join(mwi_admin) %>%
  full_join(mwi_ind)

rm(mwi_df, mwi_admin, mwi_bld_dir, mwi_dir, mwi_ind, hhid_df, cons_df, cons_pcap_df)

#*******************************************************************************
# Uganda ----

uga_ids <- read_dta(here(raw.dir, "AFRICA/LSMS/Uganda/UNPS 13/GSEC3.dta")) %>% 
  select(district = h3q13D) %>% 
  filter(district > 100, district < 500) %>% 
  distinct() %>% 
  mutate(
    name = to_character(district), 
    dest = paste0(800,district)
  ) %>%
  mutate(
    district = as.character(district),
    admin_name_l1 = case_when(
      name %in% c("Kalangala") ~ "Kalangala",
      name %in% c("Kampala") ~
        "Kampala",
      name %in% c("Kiboga", "Kyankwanzi") ~
        "Kiboga, Kyankwanzi",
      name %in% c("Luwero", "Nakasongola", "Nakaseke") ~
        "Luwero, Nakasongola, Nakaseke",
      name %in% c("Masaka", "Sembabule", "Bukomasimbi", "Kalungu", "Lwengo") ~
        "Masaka, Ssembabule, Bukomansimbi, Kalungu, Lwengo",
      name %in% c("Mpigi", "Wakiso", "Butambala", "Gomba") ~
        "Mpigi, Wakiso, Butambala, Gomba",
      name %in% c("Mubende", "Mityana") ~
        "Mubende, Mityana",
      name %in% c("Mukono", "Kayunga", "Buikwe", "Buvuma") ~
        "Mukono, Kayunga, Buikwe, Buvuma",
      name %in% c("Rakai", "Lyantonde") ~
        "Rakai, Lyantonde",
      name %in% c("Bugiri", "Iganga", "Mayuge", "Namutumba", "Namayingo", "Luuka") ~
        "Bugiri, Iganga, Mayuge, Namutumba, Luuka, Namayingo",
      name %in% c("Busia", "Tororo", "Butaleja") ~
        "Busia, Tororo, Butaleja",
      name %in% c("Jinja") ~
        "Jinja",
      name %in% c("Kamuli", "Kaliro", "Buyende") ~
        "Kamuli, Kaliro, Buyende",
      name %in% c("Kapchorwa", "Bukwo", "Kween") ~
        "Kapchorwa, Bukwo, Kween",
      name %in% c("Katakwi", "Soroti", "Kaberamaido", "Amuria", "Serere") ~
        "Katakwi, Soroti, Kaberamaido, Amuria, Serere",
      name %in% c("Kumi", "Bukedea", "Ngora") ~
        "Kumi, Bukedea, Ngora",  
      name %in% c("Mbale", "Sironko", "Bududa", "Manafwa", "Bulambuli") ~
        "Mbale, Sironko, Bududa, Manafwa, Bulambuli",
      name %in% c("Pallisa", "Budaka", "Kibuku") ~
        "Pallisa, Budaka, Kibuku",
      name %in% c("Adjumani", "Moyo") ~
        "Adjumani, Moyo",
      name %in% c("Apac", "Oyam", "Kole") ~
        "Apac, Oyam, Kole",
      name %in% c("Arua", "Yumbe", "Koboko", "Maracha") ~
        "Arua, Yumbe, Koboko, Maracha",
      name %in% c("Gulu", "Amuru", "Nwoya", "Omoro") ~
        "Gulu, Amuru, Nwoya, Omoro",
      name %in% c("Kitgum", "Pader", "Agago", "Lamwo") ~
        "Kitgum, Pader, Agago, Lamwo",
      name %in% c("Kotido", "Moroto", "Nakapiripirit", "Abim", "Napak", 
                  "Amudat", "Kaabong") ~
        "Kotido, Moroto, Nakapiripirit, Abim, Kaabong, Amudat, Napak",
      name %in% c("Lira", "Amolatar", "Dokolo", "Alebtong", "Otuke") ~
        "Lira, Amolatar, Dokolo, Alebtong, Otuke",
      name %in% c("Nebbi", "Zombo") ~
        "Nebbi, Zombo",
      name %in% c("Bundibugyo", "Ntoroko") ~
        "Bundibugyo, Ntoroko",
      name %in% c(
        "Bushenyi", "Mbarara", "Ntungamo", "Ibanda", "Isingiro", "Kiruhura",
        "Sheema", "Mitooma", "Buhweju", "Rubirizi") ~
        "Bushenyi, Mbarara, Ntungamo, Ibanda, Isingiro, Kiruhura, Buhweju, Mitooma, Rubirizi, Sheema",
      name %in% c("Hoima") ~
        "Hoima",
      name %in% c("Kabale", "Rubanda") ~
        "Kabale, Rubanda",
      name %in% c("Kabarole", "Kamwenge", "Kyenjojo", "Kyegegwa") ~
        "Kabarole, Kamwenge, Kyenjojo, Kyegegwa",
      name %in% c("Kasese") ~
        "Kasese",
      name %in% c("Kibaale", "Kagadi", "Kakumiro") ~
        "Kibaale, Kagadi, Kakumiro",
      name %in% c("Kisoro") ~
        "Kisoro",
      name %in% c("Masindi", "Buliisa", "Kiryandongo") ~
        "Masindi, Buliisa, Kiryandongo",
      name %in% c("Rukungiri", "Kanungu") ~
        "Rukungiri, Kanungu",
      TRUE ~ NA_character_
    )
  ) 

uga_hh <- read_dta(here(raw.dir, "AFRICA/LSMS/UGANDA/UNPS 13/pov2013_14.dta")) %>% 
  transmute(
    HHID, 
    region,
    district = district_code,
    consumption = nrrexp30,
    consumption_real = cpexp30
  ) %>% 
  distinct() # a single household appears twice for some reason

uga_df <- read_dta(here(raw.dir, "AFRICA/LSMS/UGANDA/UNPS 13/GSEC2.dta")) %>%
  rename(
    age = h2q8, 
    sex = h2q3, # female 2, male 1
    weight = wgt_X
  ) %>% 
  mutate(
    ae = get_adulteq(age, sex)
  ) %>% 
  group_by(HHID) %>% 
  mutate(
    ae = sum(ae)
  ) %>% 
  ungroup() %>% 
  left_join(uga_hh, by = "HHID") %>% 
  mutate(
    consumption_ae = consumption / ae,
    consumption_real_ae = consumption_real / ae
  ) %>% 
  left_join(
    uga_ids, by = "district"
    ) %>% 
  mutate(
    across(starts_with("consumption"), ~ .x * 12) # annual, 
    ) %>% 
  group_by(admin_name_l1) %>% 
  reframe(
    across(
      c(consumption, consumption_real, consumption_ae, consumption_real_ae),
      ~ weighted.mean(.x, w = weight, na.rm = TRUE)
    )
  ) %>% 
  mutate(across(c(starts_with("cons")), ~log(.x)))

uga_price_df <- id_df %>% 
  filter(country == "UGA") %>% 
  left_join(uga_df, by = "admin_name_l1")

rm(uga_df, uga_hh, uga_ids)

#*******************************************************************************
# Mali ----

# uses LSMS of 2014
# prices are adjusted per the following: "the country is stratified into region 
# and area (urban/rural) of residence for a total of 15 zones"
# p12 in "raw.dir, AFRICA\LSMS\Mali\EAC-I_2014\technical_documents\bid_eaci14.pdf"
# it is calculated at the household level rather at the individual level 
# as we only have household weights
# missing data for two units (kidal and menaka)

mli_ids <- read_dta(here(
  raw.dir, "AFRICA/LSMS/MALI/EAC-I_2014/EACIIND_p1.dta"
)) %>% 
  mutate(
    hhid = paste(grappe, menage, sep = "-"),
    region = s00q01,
    cercle = s00q02,
    age = s01q04a, # in years
    sex = s01q01, # male == 1, female == 2
    ae = get_adulteq(age, sex),
    # matching is based on labels of regions, and codes of cercles
    # in p89-98 of EACI_MANUEL_P2 (annex 5)
    cercle_name = case_when(
      region == 1 & cercle == 1 ~ "Kayes",
      region == 1 & cercle == 2 ~ "Bafoulabe",
      region == 1 & cercle == 3 ~ "Diema",
      region == 1 & cercle == 4 ~ "Kenieba",
      region == 1 & cercle == 5 ~ "Kita",
      region == 1 & cercle == 6 ~ "Denioro",
      region == 1 & cercle == 7 ~ "Yelimane",
      region == 2 & cercle == 1 ~ "Koulikoro",
      region == 2 & cercle == 2 ~ "Banamba",
      region == 2 & cercle == 3 ~ "Dioila",
      region == 2 & cercle == 4 ~ "Kangaba",
      region == 2 & cercle == 5 ~ "Kati",
      region == 2 & cercle == 6 ~ "Kolokani",
      region == 2 & cercle == 7 ~ "Nara",
      region == 3 & cercle == 1 ~ "Sikasso",
      region == 3 & cercle == 2 ~ "Bougouni",
      region == 3 & cercle == 3 ~ "Kadiolo",
      region == 3 & cercle == 4 ~ "Kolondieba",
      region == 3 & cercle == 5 ~ "Koutiala",
      region == 3 & cercle == 6 ~ "Yanfolila",
      region == 3 & cercle == 7 ~ "Yorosso",
      region == 4 & cercle == 1 ~ "Segou",
      region == 4 & cercle == 2 ~ "Baroueli",
      region == 4 & cercle == 3 ~ "Bla",
      region == 4 & cercle == 4 ~ "Macina",
      region == 4 & cercle == 5 ~ "Niono",
      region == 4 & cercle == 6 ~ "San",
      region == 4 & cercle == 7 ~ "Tominian",
      region == 5 & cercle == 1 ~ "Mopti",
      region == 5 & cercle == 2 ~ "Bandiagara",
      region == 5 & cercle == 3 ~ "Bankass",
      region == 5 & cercle == 4 ~ "Djenne",
      region == 5 & cercle == 5 ~ "Douentza",
      region == 5 & cercle == 6 ~ "Koro",
      region == 5 & cercle == 7 ~ "Tenenkou",
      region == 5 & cercle == 8 ~ "Youwarou",
      region == 6 & cercle == 1 ~ "Tombouctou",
      region == 6 & cercle == 2 ~ "Dire",
      region == 6 & cercle == 3 ~ "Goundam",
      region == 6 & cercle == 4 ~ "Gourma-Rharous",
      region == 6 & cercle == 5 ~ "Niafunke",
      region == 7 & cercle == 1 ~ "Gao",
      region == 7 & cercle == 2 ~ "Ansongo",
      region == 7 & cercle == 3 ~ "Bourem",
      # Kidal and Menaka do not exist in the data
      region == 9 & cercle == 1 ~ "Bamako",
      TRUE ~ NA_character_
    )
  ) %>% 
  group_by(hhid, cercle_name) %>% 
  reframe(
    ae = sum(ae)
  )

mli_df <- read_dta(here(
  raw.dir, "AFRICA/LSMS/MALI/EAC-I_2014/eaci2014_agregatconso.dta"
  )) %>%
  mutate(
    hhweight,
    hhid = paste(grappe, menage, sep = "-"),
    hhsize,
    consumption = dtot,
    consumption_real = dtot / deflator,
    # totcons_pc = pcexp, # already divided by deflator
    region_name = as.character(as_label(region))
  ) %>%
  left_join(mli_ids, by = "hhid") %>% 
  mutate(
    consumption_ae = consumption / ae,
    consumption_real_ae = consumption_real / ae,
    region_name = case_when( # to match with the units
      region_name %in% c("Gao", "Kidal") ~ "Gao, Kidal",
      region_name == "Tomboctou" ~ "Tombouctou", # spelling
      TRUE ~ region_name
    ),
    district_name = case_when(
      cercle_name == "Denioro" ~ "Nioro",
      cercle_name == "Bamako" ~ "District Of Bamako",
      # region_name == "Kidal" ~ "Kidal, Abeibara, Tessalit, Tin-Essako", 
      # # doesnt exist
      TRUE ~ cercle_name
    )
  ) %>% 
  group_by(region_name, district_name) %>% 
  reframe(
    across(
      c(consumption, consumption_real, consumption_ae, consumption_real_ae),
      ~ weighted.mean(.x, w = hhweight, na.rm = TRUE)
    )
  ) %>% 
  mutate(across(c(starts_with("cons")), ~log(.x)))

mli_price_df <- id_df %>% 
  filter(country == "MLI") %>% 
  left_join(mli_df, by = c("region_name", "district_name"))

rm(mli_df, mli_ids)

#*******************************************************************************
# South Africa ----
## the prices are in provinces level, but some l2 units mix diff provinces

zaf_year <- 2012
wave_zaf <- 3

cpi_zaf <- read_data(here( # 
  raw.dir, "AFRICA/LSMS/others/ZAF/cpi.dta"
)) %>%
  mutate(
    # stata anchors the original data to start from 01/1960
    # so each intrv_mo is a month since
    year  = 1960 + intrv_mo %/% 12,
    month = intrv_mo %% 12 + 1,
    date  = as.Date(sprintf("%d-%02d-01", year, month))
  ) %>% 
  filter(
    prov2011 < 100 | # only look at the provinces
      prov2011 == 103 # or the country average
  ) %>%
  group_by(prov2011, location, year) %>% 
  reframe(mean_cpi = mean(CPI)) %>%
  filter(year == zaf_year) %>%
  transmute(
    prov2011, location,
    mean_cpi = mean_cpi / mean_cpi[prov2011 == 103] * 100
  ) # normalise all regions based on country average of 100, for each year

zaf_df <- read_dta(here(
  raw.dir, "AFRICA/LSMS/others/ZAF/saf_panel.dta"
)) %>% 
  select(weight, pweight, pid, hhid, hhsize, nadult, adulteq, wave, year, 
         prov2011, dc2011,
         consumption, consumption_pa) %>%
  mutate(
    admin_name_l2 = case_when(
      dc2011 == 101 ~ "West Coast",
      dc2011 %in% c(102, 103) ~ "Cape Winelands, Overberg",
      dc2011 == 104 ~ "Eden",
      dc2011 == 105 ~ "Central Karoo",
      dc2011 == 199 ~ "City Of Cape Town",
      dc2011 %in% c(
        599, 215, 522, 212, 556, 244, 213, 260, 521, 554, 543, 214
      ) ~
        "Ethekwini, O.r.tambo, Umgungundlovu, Amathole, Zululand, Alfred Nzo, Chris Hani, Buffalo City, Ugu, Umzinyathi, Sisonke, Joe Gqabi",
      dc2011 == 210 ~ "Cacadu",
      dc2011 == 299 ~ "Nelson Mandela Bay",
      dc2011 %in% c(
        742, 638, 748, 640, 639, 309, 308, 345, 307
      ) ~
        "Sedibeng, Ngaka Modiri Molema, West Rand, Dr Kenneth Kaunda, Dr Ruth Segomotsi Mompati, Frances Baard, Siyanda, John Taolo Gaetsewe, Pixley Ka Seme",
      dc2011 == 306 ~ "Namakwa",
      dc2011 %in% c(
        419, 499, 418, 420, 416
      ) ~
        "Thabo Mofutsanyane, Mangaung, Lejweleputswa, Fezile Dabi, Xhariep",
      dc2011 %in% c(523, 555) ~ "Uthukela, Amajuba",
      dc2011 == 528 ~ "Uthungulu",
      dc2011 == 559 ~ "Ilembe",
      dc2011 == 527 ~ "Umkhanyakude",
      dc2011 %in% c(
        797, 799, 832, 637, 831, 934, 935, 947, 933, 936
      ) ~
        "Ekurhuleni, City Of Tshwane, Ehlanzeni, Bojanala, Nkangala, Vhembe, Capricorn, Greater Sekhukhune, Mopani, Waterberg",
      dc2011 == 798 ~ "City Of Johannesburg",
      dc2011 == 830 ~ "Gert Sibande",
      TRUE ~ NA_character_
    ),
    dc2011_label = as_label(dc2011)
  ) # 67 observations with NAs in dc2011

zaf_cons <- zaf_df %>% 
  filter(
    wave == wave_zaf, 
    prov2011 != 10 # drop 'outside of South Africa'
  ) %>% 
  left_join(cpi_zaf, by = "prov2011") %>% 
  mutate(
    consumption_real = consumption * (mean_cpi / 100),
    consumption_ae = consumption / adulteq,
    consumption_real_ae = 
      consumption_real / adulteq,
    across(starts_with("consumption"), ~ .x * 365) # annual from days? 
    # TODO - check whats the reference
  ) %>% 
  group_by(admin_name_l2) %>% # our l2 units
  reframe(
    across(
      c(consumption, consumption_real, consumption_ae, consumption_real_ae),
      ~ weighted.mean(.x, w = weight, na.rm = TRUE)
    )  
  ) %>% 
  mutate(across(c(starts_with("cons")), ~log(.x)))


zaf_price_df <- id_df %>% 
  filter(country == "ZAF") %>% 
  left_join(zaf_cons, by = "admin_name_l2")

rm(zaf_df, zaf_year, zaf_cons, cpi_zaf)
#*******************************************************************************

# Ghana ---- 

## matching of units to id_df could be based on p.63-65 in the code book
## raw.dir, "Countries\GHA\LMMVW data\Questionnaires\Wave 1\CODE_BOOK.pdf"
## however, there's quite a few mismatches so using region prices
## currently based on district cpis which are quite extreme

gha_food_p <- read_dta(here(
  raw.dir, "COUNTRIES/GHA/LMMVW DATA/Wave 1/S11A.dta"
))

gha_district_lookup <- tribble(
  ~id1, ~id2, ~district_original,
  
  # WESTERN (01)
  1,  1, "Ahanta West",
  1,  2, "Aowin Or Suaman",
  1,  3, "Bia",
  1,  4, "Bibiani/Anhwiaso/Bekwai",
  1,  5, "Ellembelle",
  1,  6, "Jomoro",
  1,  7, "Juabeso",
  1,  8, "Mpohor-Wassa East",
  1,  9, "Nzema East",
  1, 10, "Prestea Or Huni Valley",
  1, 11, "Sefwi-Akontobra",
  1, 12, "Sefwi-Wiawso",
  1, 13, "Sekondi-Takoradi",
  1, 14, "Shama",
  1, 15, "Tarkwa Nsuaem",
  1, 16, "Wassa Amenfi East",
  1, 17, "Wassa Amenfi West",
  
  # CENTRAL (02)
  2, 18, "Abura-Asebu-Kwamankese",
  2, 19, "Agona East",
  2, 20, "Agona West",
  2, 21, "Ajumako/Enyan/Esiam",
  #2, 22, "Asikuma/Odoben/Brakwa", # multiple units
  2, 23, "Assin North",
  2, 24, "Assin South",
  2, 25, "Awutu Senya",
  2, 26, "Cape Coast",
  2, 27, "Effutu",
  2, 28, "Gomoa East",
  2, 29, "Gomoa West",
  2, 30, "Komenda-Edina-Eguafo-Abirem",
  2, 31, "Mfantsiman",
  2, 32, "Twifo/Heman/Lower Denkyira",
  2, 33, "Upper Denkyira West",
  2, 34, "Upper Denkyira East",
  
  # GREATER ACCRA (03)
  3, 35, "Accra",
  3, 36, "Adenta",
  3, 37, "Ashaiman",
  3, 38, "Dangme East",
  3, 39, "Dangme West",
  3, 40, "Ga East",
  3, 41, "Ga West",
  3, 42, "Ledzokuku-Krowor",
  3, 43, "Tema",
  3, 44, "Weija (Ga South)",
  
  # VOLTA (04)
  4, 45, "Adaklu-Anyigbe",
  4, 46, "Akatsi",
  4, 47, "Biakoye",
  4, 48, "Ho",
  4, 49, "Hohoe",
  4, 50, "Jasikan",
  4, 51, "Kadjebi",
  4, 52, "Keta",
  4, 53, "Ketu North",
  4, 54, "Ketu South",
  4, 55, "Kpandai",
  4, 56, "Krachi East",
  4, 57, "Krachi West",
  4, 58, "Nkwanta North",
  4, 59, "Nkwanta South",
  4, 60, "North Tongu",
  4, 61, "South Tongu",
  4, 62, "South Dayi",
  
  # EASTERN (05)
  5, 63, "Akwapim North",
  5, 64, "Akwapim South",
  5, 65, "Akyemansa",
  5, 66, "Asuogyaman",
  5, 67, "Atiwa",
  5, 68, "Birim Central Municipal",
  5, 69, "Birim North",
  5, 70, "Birim South",
  5, 71, "East Akim",
  5, 72, "Fanteakwa",
  5, 73, "Kwaebibirem",
  5, 74, "Kwahu East",
  5, 75, "Kwahu North (Afram Plains)",
  5, 76, "Kwahu South",
  5, 77, "Kwahu West",
  5, 78, "Lower Manya Krobo",
  5, 79, "New Juaben",
  #5, 80, "Suhum / Kraboa Coaltar", # in id_df there are two districts
  5, 81, "Upper Manya Krobo",
  5, 82, "West Akim",
  5, 83, "Yilo Krobo",
  
  # ASHANTI (06)
  6, 84, "Adansi North",
  6, 85, "Adansi South",
  6, 86, "Afigya Kwabre",
  6, 87, "Ahafo Ano North",
  6, 88, "Ahafo Ano South",
  6, 89, "Amansie Central",
  6, 90, "Amansie East",
  6, 91, "Amansie West",
  6, 92, "Asante Akim North",
  6, 93, "Asante Akim South",
  6, 94, "Atwima Mponua",
  6, 95, "Atwima Nwabiagya",
  6, 96, "Atwima Kwanwoma",
  6, 97, "Bekwai Municipal",
  6, 98, "Bosome Freho",
  6, 99, "Bosumtwi",
  6,100, "Ejisu Juaben",
  6,101, "Ejura Sekyedumase",
  6,102, "Kumasi Metropolitan Assembly (Kma)",
  6,103, "Kwabre  East",
  6,104, "Mampong",
  6,105, "Obuasi",
  6,106, "Offinso Municipal",
  6,107, "Offinso North",
  6,108, "Sekyere Afram Plains",
  6,109, "Sekyere Central",
  6,110, "Sekyere East",
  6,111, "Sekyere South",
  
  # BRONG AHAFO (07)
  7,112, "Asunafo North",
  7,113, "Asunafo South",
  7,114, "Asutifi",
  7,115, "Atebubu",
  7,116, "Berekum",
  7,117, "Dormaa Municipal",
  7,118, "Dormaa East",
  7,119, "Jaman North",
  7,120, "Jaman South",
  7,121, "Kintampo North",
  7,122, "Kintampo South",
  7,123, "Nkoranza North",
  7,124, "Nkoranza South",
  7,125, "Pru",
  7,126, "Sene",
  7,127, "Sunyani Municipal",
  7,128, "Sunyani West",
  7,129, "Tain",
  7,130, "Tano North",
  7,131, "Tano South",
  7,132, "Techiman",
  7,133, "Wenchi",
  
  # NORTHERN (08)
  8,134, "Bole",
  8,135, "Bunkpurugu Yonyo",
  8,136, "Central Gonja",
  8,137, "Chereponi",
  8,138, "East Gonja",
  8,139, "East Mamprusi",
  8,140, "Gushiegu",
  8,141, "Karaga",
  8,142, "Kpandai",
  8,143, "Nanumba North",
  8,144, "Nanumba South",
  8,145, "Saboba",
  8,146, "Savelugu Nanton",
  #8,147, "Sawla-Tuna-Kalba", # multiple regions in our data, can't match to a single one
  8,148, "Tamale Metro",
  8,149, "Tolon Kumbugu",
  8,150, "West Gonja",
  8,151, "Mamprusi  West",
  8,152, "Yendi",
  8,153, "Zabzugu Tatali",
  
  # UPPER EAST (09)
  9,154, "Bawku Municipal",
  9,155, "Bawku West",
  9,156, "Bolgatanga Municipal",
  9,157, "Bongo",
  9,158, "Builsa",
  9,159, "Garu Tempane",
  9,160, "Kassena Nankana",
  9,161, "Kassena Nankana West",
  9,162, "Talensi Nabdam",
  
  # UPPER WEST (10)
  10,163, "Jirapa",
  10,164, "Lambussie Karni",
  10,165, "Lawra",
  10,166, "Nadowli",
  10,167, "Sissala East",
  10,168, "Sissala West",
  10,169, "Wa Municipal",
  10,170, "Wa East",
  10,171, "Wa West"
)

gha_id <- id_df %>% 
  filter(country == "GHA")

gha_district_lookup_collapse <- gha_id %>% 
  transmute(
    district_name,
    district_original = strsplit(district_name, ",\\s*")
  ) %>%
  unnest(district_original) %>%
  transmute(
    district_original = stringr::str_trim(district_original),
    district_name
  )

gha_food_p <- gha_food_p %>%
  left_join(gha_district_lookup, by = c("id1", "id2")) %>%
  left_join(gha_district_lookup_collapse, by = "district_original") %>% 
  select(-district_original) %>% 
  filter(!is.na(district_name)) 

gha_ltrs <- c("b", "c", "d", "e")

# divide all the variables in pessewas by 100
gha_food_p <- gha_food_p %>%
  mutate(
    across(
      all_of(paste0("s11a_", gha_ltrs, "iii")),
      ~ .x / 100
    )
  )

# create total expenditures by source, adding cedis and pessewas
for (l in gha_ltrs) {
  gha_food_p <- gha_food_p %>%
    mutate(
      !!paste0("X", l) := rowSums(
        select(., paste0("s11a_", l, "ii"), paste0("s11a_", l, "iii")),
        na.rm = TRUE
      ),
      !!paste0("Q", l) := .data[[paste0("s11a_", l, "i")]]
    )
}

# get the most common unit per item - to avoid noise
dominant_units_gha <- gha_food_p %>%
  filter(!is.na(s11a_f)) %>%
  count(itname, unit = s11a_f) %>%
  group_by(itname) %>%
  slice_max(n, n = 1) %>%
  select(itname, unit)

df_prices_gha <- gha_food_p %>%
  mutate(
    X = rowSums(select(., starts_with("X")), na.rm = TRUE),
    Q = rowSums(select(., starts_with("Q")), na.rm = TRUE),
    unit = s11a_f
  ) %>%
  #filter(!is.na(unit)) %>% 
  inner_join(dominant_units_gha, by = c("itname", "unit")) %>% # keep only dominant units
  group_by(district_name, itname, unit) %>%
  reframe(
    N = n(), # number of observations
    X = mean(X, trim = 0.01, na.rm = TRUE),
    Q = mean(Q, trim = 0.01, na.rm = TRUE)
  ) %>%
  mutate(P = X / Q) %>% 
  filter(!is.na(P), !is.infinite(P), P > 0)  # avoid infinity, NA & 0
## the following isnt necessary if dominant units is used
# # drop unfrequent combos of items-units across the country
# group_by(itname, unit) %>%
# mutate(N_ctry = sum(N)) %>%
# ungroup() %>% 
# filter(N_ctry >= 100) # only keep if very common, otherwise too much noise

# get base cpi - national average
gha_cpi_base <- df_prices_gha %>%
  group_by(itname, unit) %>%
  reframe(
    Q0 = weighted.mean(Q, w = N, na.rm = TRUE)
  )

# get district level values
gha_cpi_district <- df_prices_gha %>%
  left_join(gha_cpi_base, by = c("itname", "unit")) %>% 
  mutate(p_q = P*Q0) %>% 
  group_by(district_name) %>%
  reframe(
    CPI_raw = sum(P * Q0, na.rm = TRUE) 
  ) %>% 
  transmute(
    district_name,
    CPI = 100 * CPI_raw / mean(CPI_raw, na.rm = TRUE)
  ) 

gha_cons <- read_data(here( 
  raw.dir, "AFRICA/LSMS/others/GHA/gha_panel.dta"
)) %>%
  filter(year == 2010) %>%   # wave 1
  group_by(hhid) %>% 
  mutate(
    sex = ifelse(female == 1, 2, 1), # female should be 2, male 1
    ae = get_adulteq(age, sex)
  ) %>% 
  ungroup() %>%
  mutate(
    id1 = as.character(id1), 
    id2 = as.character(id2)
  ) %>% 
  left_join(
    gha_district_lookup %>% 
      mutate(
        id1 = as.character(id1), 
        id2 = as.character(id2)
      ), 
    by = c("id1", "id2")
  ) %>%
  left_join(gha_district_lookup_collapse, by = "district_original") %>% 
  select(-district_original) %>% 
  filter(!is.na(district_name)) %>% 
  left_join(gha_cpi_district, by = "district_name") %>% 
  filter(
    !is.na(district_name), # drop NA - 3 observations
    consumption > 0 # drop two observations with 0 consumption
  ) %>% 
  mutate(
    consumption_real = consumption / (CPI/100),
    consumption_ae = consumption / ae,
    consumption_real_ae = consumption_real / ae,
  ) %>%
  group_by(district_name) %>% 
  reframe(
    across(
      c(consumption, consumption_real, consumption_ae, consumption_real_ae),
      ~ weighted.mean(.x, w = ppweight3, na.rm = TRUE) # using pop. weights
    )  
  ) %>% 
  mutate(across(c(starts_with("cons")), ~log(.x)))

gha_price_df <- gha_id %>% 
  left_join(gha_cons, by = "district_name") %>% 
  filter(!is.na(consumption))

rm(gha_id, gha_food_p, gha_district_lookup, gha_district_lookup_collapse,
   gha_cpi_base, gha_cons, dominant_units_gha, df_prices_gha, gha_cpi_district)
#*******************************************************************************
# Ethipoia ----

eth_id <- id_df %>% 
  filter(country == "ETH")

# note - the price diffs are quite large, hence diffs between nominal and real is large
# need to check how is price_index_hce defined
eth_cons <- read_dta(here(raw.dir, "Africa", "LSMS", "Ethiopia", "ESS 13", "cons_agg_w2.dta")) %>% 
  mutate(
    ipums_id = case_when(
      str_length(saq01) == 1 ~ paste0("23100", saq01),
      str_length(saq01) == 2 ~ paste0("2310", saq01),
      TRUE ~ NA
    ),
    consumption = total_cons_ann,
    consumption_real = total_cons_ann / price_index_hce,
    consumption_ae = nom_totcons_aeq,
    consumption_real_ae = nom_totcons_aeq / price_index_hce,
    weight = pw2
  ) %>% 
  group_by(ipums_id) %>% 
  reframe(
    across(
      c(consumption, consumption_real, consumption_ae, consumption_real_ae),
      ~ weighted.mean(.x, w = weight, na.rm = TRUE)
    )
  ) %>% 
  mutate(across(c(starts_with("cons")), ~log(.x)))

eth_price_df <- eth_id %>% 
  left_join(eth_cons, by = "ipums_id") 

rm(eth_id, eth_cons)

#*******************************************************************************
# Nigeria ----

# spatial prices and real and nominal consumption are available in 2018
# but this isn't the wave we're using
# there are also food conversion factors by item in the previous waves
# but this hasn't been made into a spatial price aggregate
# can be used to create real data, so far we only have nominal

nga_id <- id_df %>% 
  filter(country == "NGA")

nga_cons <- read_dta(here(raw.dir, "Africa", "LSMS", "Nigeria", "GHS 10", "cons_agg_wave1_visit2.dta")) %>% 
  select(hhid, hhweight, totcons, state)

nga_ae <-  read_dta(here(raw.dir, "Africa", "LSMS", "Nigeria", "GHS 10", "sect1_harvestw1.dta")) %>% 
  select(state, hhid, indiv, sex = s1q2, age = s1q4) %>% 
  mutate(
    ae = get_adulteq(age, sex)
  ) %>% 
  group_by(hhid) %>% 
  reframe(
    ae = sum(ae, na.rm = T)
  ) 

nga_cons <- nga_cons %>% 
  left_join(nga_ae, by = "hhid") %>% 
  mutate(
    ipums_id = case_when(
      str_length(state) == 1 ~ paste0("56600", state),
      str_length(state) == 2 ~ paste0("5660", state),
      TRUE ~ NA
    ),
    consumption = totcons,
    consumption_ae = totcons / ae
  ) %>% 
  group_by(ipums_id) %>% 
  reframe(
    across(
      c(consumption, consumption_ae),
      ~ weighted.mean(.x, w = hhweight, na.rm = TRUE)
    )
  ) %>% 
  mutate(across(c(starts_with("cons")), ~log(.x)))

nga_price_df <- nga_id %>% 
  left_join(nga_cons, by = "ipums_id") 

rm(nga_id, nga_ae)

#*******************************************************************************

# Niger ----
## we don't have the units

#*******************************************************************************
### merge and save ----
ctry_prices <- bind_rows(
  tza_price_df, mwi_price_df, zaf_price_df, mli_price_df, uga_price_df,
  gha_price_df, eth_price_df, nga_price_df
  )

saveRDS(ctry_prices, here(out.dir, "R", "prices.rds")) 
write_dta(ctry_prices, here(out.dir, "prices.dta")) 
