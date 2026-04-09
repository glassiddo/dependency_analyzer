#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    December, 2025
#* Title:   Get and clean the aggregate values from Gabon 1st census summary
#* Desc:    Take the values from the report on the census and clean them
#*******************************************************************************
source("code/SSA_env_SetUp.R")

# everything is from the report about the results of the census
# https://instatgabon.org/uploads/folder_1/gab-1993-rec-o1_principaux_resultats-1.pdf

ids <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  filter(country == "GAB")

gab_units <- ids %>% 
  select(ipums_id, admin_name)

## assumptions
## 1) age distribution is identical across provinces, based on national one
## age distribution from p.9 for the entire country
## 2) the distribution of industry in rural regions and in urban regions 
## is identical across provinces
## theoretically this could be avoided by using the jobs breakdown for the four
## largest cities in the country (p.65) but it'll still be problematic


### population -----
pop_age <- data.frame(
  age_group = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70+"),
  urban = c(116259,104876,90735,78438,74232,67025,57151,
            42103,29868,21132,16814,14255,10567,7380,11461),
  rural = c(37753,35611,30955,22362,17279,14725,12295,
            10151,9743,11074,12900,15187,14014,10711,17920),
  total = c(154012,140487,121690,100800,91511,81750,69446,
            52254,39611,32206,29714,29442,24581,18091,29381)
)

prime_age_groups <- c(
  "15-19","20-24","25-29","30-34","35-39",
  "40-44","45-49","50-54","55-59","60-64"
)

prime_age_pop_share <- pop_age %>% 
  summarise(
    total = sum(total[age_group %in% prime_age_groups]) / sum(total)
  ) %>% 
  pull()

# total population by per province urban/rural, p.5
pop <- data.frame(
  admin_name = c("Estuaire","Haut-Ogooue","Moyen-Ogooue","Ngounie",
               "Nyanga","Ogooue-Ivindo","Ogooue-Lolo",
               "Ogooue-Maritime","Woleu-Ntem"),
  urban = c(427950,76378,18726,37520,21815,17775,9379,87659,35094),
  rural = c(35237,27923,23590,40261,17615,31087,24536,10254,62177),
  total = c(463187,104301,42316,77781,39430,48862,43915,97913,97271)
) %>% 
  left_join(gab_units, by = "admin_name") %>% 
  transmute(
    ipums_id,
    bl_pop = total * prime_age_pop_share,
    bl_rural_pop = (rural/total) * bl_pop,
    bl_urban_pop = (urban/total) * bl_pop,
  ) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 0)))

rm(pop_age, prime_age_pop_share)

saveRDS(pop, here(build.dir, "Countries", "GAB", "93_pop_aggregates.rds"))

### employment -----

# activity rate from p.33
# get from it the share of work force that is prime aged
pa_scale <- data.frame(
  age_group = c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                "45-49", "50-54", "55-59", "60-64", "65 & *"),
  total_population = c(121626, 100448, 90429, 80673, 68833, 51871, 39383,
                       32045, 29570, 29348, 24495, 47294),
  active_population = c(3716, 19897, 46663, 57837, 56069, 44068, 34019,
                        27243, 24657, 20770, 15932, 25073)
) %>% 
  mutate(pa = ifelse(age_group %in% prime_age_groups, 1, 0)) %>%
  reframe(
    scale = sum(active_population[pa==1]) / sum(active_population)
  ) %>% 
  pull()

# activity by province from p.35
labor_by_prov <- data.frame(
  admin_name = c("Estuaire", "Haut-Ogooue", "Moyen-Ogooue", "Ngounie", 
                 "Nyanga", "Ogooue-Ivindo", "Ogooue-Lolo", 
                 "Ogooue-Maritime", "Woleu-Ntem"),
  urban_active = c(152979, 19225, 6007, 8693, 6750, 6224, 5739, 30165, 9632),
  rural_active = c(18881, 11455, 12003, 15957, 9144, 15503, 11552, 6562, 29473),
  urban_unemp = c(37276, 3878, 1020, 1101, 1405, 803, 1515, 7317, 1746),
  rural_unemp = c(3040, 879, 781, 1062, 941, 1104, 1005, 271, 2478)
) %>%
  left_join(gab_units, by = "admin_name") %>%
  transmute(
    ipums_id,
    emloyed_urban = (urban_active - urban_unemp) * pa_scale,
    employed_rural = (rural_active - rural_unemp) * pa_scale
  ) 

# sector data from p.41
# get the share in rural and urban areas working in each sector
sectors <- data.frame(
  Secteur_Activite = c(
    "Agriculture",
    "Elevage",
    "Pêche",
    "Exploitation forest, sylviculture",
    "Industrie du bois",
    "Production de pétrole",
    "Raffinage du pétrole",
    "Forage, recherche pétrole & mineral",
    "Extraction de minerais métalliques",
    "Extraction d'autres minerais",
    "Industries agro-alimentaires",
    "Fabric, boissons, glaces... ind. du tabac",
    "Autres industries",
    "Indus, textile, de l'habillement, du cuir",
    "Imprimerie, presse, édition",
    "Industrie chimique, métallurgie",
    "Fabrig, mat. de construction, ind. verre",
    "Industrie mécanique",
    "Production, distrib. d'eau & électricité",
    "B.TP., génie civil",
    "Transport terrestre",
    "Transport maritime",
    "Transport aérien",
    "Auxilliaires de transport",
    "Postes et télécommunications",
    "Services de réparation",
    "Hotels, cafés, restaurants",
    "Services hôteliers",
    "Services rendus aux entreprises",
    "Services rendus aux particuliers",
    "Services domestiques des ménages",
    "Commerce, import-export, distribution",
    "Banques",
    "Assurances",
    "Administrations d'Etat",
    "Administration de sécurité sociale",
    "Autres administrations",
    "Ambassades et consulats",
    "Organisations internationales",
    "Organismes religieux"
  ),
  Total = c(
    121240, 605, 3255, 3232, 5835, 1688, 1362, 1136, 2438, 861, 3571, 410, 
    3971, 1221, 232, 59, 111, 33, 2834, 9610, 10746, 980, 1586, 656, 1807, 
    3151, 1576, 241, 2874, 18959, 10867, 38902, 1715, 252, 45344, 1571, 
    1088, 205, 1755, 343
  ),
  Urban = c(
    20946, 95, 1820, 850, 3607, 1657, 1314, 1092, 2166, 71, 2628, 398, 
    3786, 1174, 229, 43, 106, 29, 2682, 8899, 9983, 954, 1500, 611, 1761, 
    3066, 1540, 218, 2801, 18075, 10387, 36558, 1695, 247, 42108, 1543, 
    575, 199, 1649, 291
  ),
  Rural = c(
    100294, 510, 1435, 2382, 2228, 31, 48, 44, 272, 790, 943, 12, 185, 
    47, 3, 16, 5, 4, 152, 711, 763, 26, 86, 45, 46, 85, 36, 23, 73, 884, 
    480, 2344, 20, 5, 3236, 28, 513, 6, 106, 52
  )
) %>%
  mutate(
    indgen = case_when(
      Secteur_Activite %in% c("Agriculture", "Elevage", "Pêche", 
                              "Exploitation forest, sylviculture", 
                              "Industrie du bois") ~ 10, # agriculture etc
      Secteur_Activite %in% c("Industries agro-alimentaires",
                              "Fabric, boissons, glaces... ind. du tabac",
                              "Autres industries",
                              "Indus, textile, de l'habillement, du cuir",
                              "Imprimerie, presse, édition",
                              "Industrie chimique, métallurgie",
                              "Fabrig, mat. de construction, ind. verre",
                              "Industrie mécanique") ~ 30, # manufacturing
      Secteur_Activite %in% c("Production de pétrole",
                              "Raffinage du pétrole",
                              "Forage, recherche pétrole & mineral",
                              "Extraction de minerais métalliques",
                              "Extraction d'autres minerais") ~ 20, # mining
      # services - we now treat it as everything else 
      TRUE ~ 110
    )
  ) %>% 
  group_by(indgen) %>%
  reframe(
    Urban = sum(Urban, na.rm = T),
    Rural = sum(Rural, na.rm = T),
  ) %>%
  transmute(
    indgen,
    urban_share = Urban / sum(Urban),
    rural_share = Rural / sum(Rural)
  ) 

emp <- labor_by_prov %>% 
  crossing(sectors) %>% 
  transmute(
    ipums_id, indgen,
    urban_workers = emloyed_urban * urban_share,
    rural_workers = employed_rural * rural_share,
    sector_workers = urban_workers + rural_workers
  ) %>%
  group_by(ipums_id) %>%
  reframe(
    bl_ag = sum(sector_workers[indgen == 10]),
    bl_mining = sum(sector_workers[indgen == 20]),
    bl_mfg = sum(sector_workers[indgen == 30]),
    bl_svc = sum(sector_workers[indgen == 110]),
    bl_empl = sum(sector_workers),
    bl_urban_emp = sum(urban_workers)
  ) %>% 
  left_join(pop %>% select(ipums_id, bl_pop), by = "ipums_id") %>%
  mutate(
    bl_nonag = bl_mining + bl_mfg + bl_svc,
    bl_u = bl_pop - bl_empl 
  ) %>% 
  select(-bl_pop) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 0)))

saveRDS(emp, here(build.dir, "Countries", "GAB", "93_emp_aggregates.rds"))

### migration -----

# birth place vs current from p.21 part 1
mig <- data.frame(
  birth_province = c("Estuaire","Haut-Ogooue","Moyen-Ogooue","Ngounie",
                     "Nyanga","Ogooue-Ivindo","Ogooue-Lolo",
                     "Ogooue-Maritime","Woleu-Ntem", "Abroad"),
  `Estuaire` = c(213042, 16639, 14465, 35465, 18361, 12836, 8670, 13200, 41046, 87215),
  `Haut-Ogooue` = c(6002, 79613, 510, 1404, 602, 913, 4796, 1009, 1014, 7720),
  `Moyen-Ogooue` = c(3168, 290, 24099, 6275, 1706, 731, 768, 1951, 854, 2319),
  `Ngounie` = c(5125, 525, 1163, 63602, 1929, 269, 362, 1889, 542, 1896),
  `Nyanga` = c(2483, 137, 275, 1295, 32087, 160, 101, 1064, 297, 1350),
  `Ogooue-Ivindo` = c(3213, 493, 719, 1368, 324, 38911, 717, 248, 1101, 1657),
  `Ogooue-Lolo` = c(2043, 3203, 321, 805, 478, 665, 34115, 707, 258, 1209),
  `Ogooue-Maritime` = c(6013, 951, 5313, 7046, 6148, 499, 1326, 55350, 2669, 12270),
  `Woleu-Ntem` = c(4775, 340, 411, 567, 276, 976, 176, 405, 78122, 10853),
  check.names = FALSE  
) %>% 
  pivot_longer(
    cols = -birth_province,
    names_to = "destination_province",
    values_to = "m_raw"
  ) %>% 
  filter(birth_province != "Abroad") %>% 
  left_join(
    ids %>%
      select(
        ipums_id_o = ipums_id,
        prev_region = region,
        admin_name
        ), 
    by = c("birth_province" = "admin_name")
    ) %>% 
  left_join(
    ids %>% 
      select(
        ipums_id_d = ipums_id,
        region = region,
        admin_name
      ),
      by = c("destination_province" = "admin_name")
    ) %>% 
  mutate(
    nonmig = ipums_id_o == ipums_id_d
  ) %>% 
  group_by(ipums_id_d) %>% 
  mutate(
    m_oo = sum(m_raw[nonmig], na.rm = TRUE),
    m_od = ifelse(nonmig, 0, m_raw)
  ) %>% 
  ungroup() %>% 
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
    country_name = "Gabon",
    prev_country_name = "Gabon",
    ipums_id_d = as.character(ipums_id_d),
    ipums_id_o = as.character(ipums_id_o),
    mig_start_yr = NA, 
    mig_end_yr = 1993,
    mig_prd_len = NA,
    mig_measure = "since birth"
  ) %>% 
  select(-c(m_raw, nonmig, birth_province, destination_province))

saveRDS(mig, here(build.dir, "Countries", "GAB", "93_mig_aggregates.rds"))
