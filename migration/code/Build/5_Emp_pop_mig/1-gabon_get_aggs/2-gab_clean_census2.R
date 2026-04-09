#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    December, 2025
#* Title:   Get and clean the aggregate values from Gabon 2nd census summary
#* Desc:    Take the values from the report on the census and clean them
#*******************************************************************************
source("code/SSA_env_SetUp.R")

# everything is from the report about the results of the census
# https://instatgabon.org/uploads/folder_1/RESULTATS-GLOBAUX-RGPL2013-OK_DECEMBRE-2015-1.pdf

ids <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  filter(country == "GAB")

gab_units <- ids %>% 
  select(ipums_id, admin_name)

## assumptions
## 1) age distribution is identical across provinces, based on national one
## 2) the share of population working in agriculture is same 12+ and 15-64

### population -----

## need prime age population
# across the entire country, 61.4% are prime aged (15-64) (text, p.53)
# in urban, its 62.1%
# in rural, its 55.2% (calculations based on Tableau 12 in p.53)

pa_share_total <- 0.614
# pa_share_urban <- 0.621
# pa_share_rural <- 0.552

# total population by per province urban/rural, p.5
pop <- data.frame(
  admin_name = c("Estuaire","Haut-Ogooue","Moyen-Ogooue","Ngounie",
                 "Nyanga","Ogooue-Ivindo","Ogooue-Lolo",
                 "Ogooue-Maritime","Woleu-Ntem"),
  urban = c(877147, 211531, 45652, 69992, 43014, 36819, 40987, 148925, 103579),
  rural = c(18542, 39268, 23635, 30846, 9840, 26474, 24784, 8637, 51407),
  total = c(895689, 250799, 69287, 100838, 52854, 63293, 65771, 157562, 154986)
  ) %>%
  left_join(gab_units, by = "admin_name") %>%
  transmute(
    ipums_id,
    el_pop = total * pa_share_total,
    el_rural_pop = (rural/total) * el_pop,
    el_urban_pop = (urban/total) * el_pop
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

saveRDS(pop, here(build.dir, "Countries", "GAB", "13_pop_aggregates.rds"))

### employment -----

### p112 activity rates by province and by urban/rural

# # already prime aged: (16-65, instead of 15-64, but very similar) - p.74
# activity_urban_rural <- data.frame(
#   status = c("Active", "Inactive", "Undeclared"),
#   urban = c(509678, 373657, 35816),
#   rural = c(67564, 38683, 6123),
#   total = c(577242, 412340, 41939)
#   ) %>%
#   group_by(status) %>% 
#   reframe(
#     urban_share = sum(urban) / sum(total),
#     rural_share = sum(rural) / sum(total)
#   ) %>% 
#   filter(status == "Active")

# same
activity_provinces <- data.frame(
  admin_name = c("Estuaire","Haut-Ogooue","Moyen-Ogooue","Ngounie",
                   "Nyanga","Ogooue-Ivindo","Ogooue-Lolo",
                   "Ogooue-Maritime","Woleu-Ntem"),
  Hommes_Actif = c(191049, 52424, 13431, 13266, 6628, 7587, 8677, 37365, 28612),
  Hommes_Inactif = c(81740, 18431, 4565, 8569, 4369, 6612, 4415, 9755, 12666),
  Hommes_Non_declare = c(13662, 2263, 839, 1072, 658, 644, 701, 1742, 1149),
  Hommes_Ensemble = c(286451, 73118, 18835, 22907, 11655, 14843, 13793, 48862, 42427),
  Femmes_Actif = c(122431, 23721, 8522, 9783, 4278, 3821, 6812, 21437, 17398),
  Femmes_Inactif = c(136244, 33967, 8247, 13657, 7951, 10790, 7094, 21192, 22076),
  Femmes_Non_declare = c(9686, 1967, 1052, 1153, 793, 773, 843, 1447, 1495),
  Femmes_Ensemble = c(268361, 59655, 17821, 24593, 13022, 15384, 14749, 44076, 40969)
  ) %>% 
  transmute(
    admin_name,
    active = Hommes_Actif + Femmes_Actif,
    total = Hommes_Ensemble + Femmes_Ensemble
  )

### p130 - aged 12 and above so not fully accurate, see assumptions
ag_share <- data.frame(
  admin_name = c("Estuaire","Haut-Ogooue","Moyen-Ogooue","Ngounie",
                 "Nyanga","Ogooue-Ivindo","Ogooue-Lolo",
                 "Ogooue-Maritime","Woleu-Ntem"),
  share_ag = c(7.6, 28.6, 33.2, 44.7, 39.7, 47.9, 46.3, 7.8, 36.4)
) %>% 
  mutate(share_ag = share_ag / 100)

emp <- activity_provinces %>% 
  left_join(ag_share, by = "admin_name") %>% 
  left_join(gab_units, by = "admin_name") %>%
  transmute(
    ipums_id,
    el_ag = active * share_ag,
    el_nonag = active - el_ag,
    el_empl = active,
    el_u = total - active
  ) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 0)))

saveRDS(emp, here(build.dir, "Countries", "GAB", "13_emp_aggregates.rds"))

### migration -----

# birth place vs current from p.43-44 (tableau 33)
# the 'born and still in same province' numbers are taken from tableau 34, col c
# can also be computed as last column - one before last in tableau 33
mig <- data.frame(
  destination_province = c("Estuaire","Haut-Ogooue","Moyen-Ogooue","Ngounie",
                           "Nyanga","Ogooue-Ivindo","Ogooue-Lolo",
                           "Ogooue-Maritime","Woleu-Ntem"),
  `Estuaire` = c(470609, 10895, 7110, 6903, 3436, 3607, 3016, 14964, 10297),
  `Haut-Ogooue` = c(44804, 163953, 1079, 1003, 435, 827, 4458, 2932, 1155),
  `Moyen-Ogooue` = c(21404, 910, 35748, 1769, 484, 472, 365, 6327, 1064),
  `Ngounie` = c(47783, 2049, 7893, 74094, 2260, 541, 977, 8675, 1440),
  `Nyanga` = c(25208, 980, 2159, 2981, 37781, 251, 362, 7929, 683),
  `Ogooue-Ivindo` = c(23599, 1940, 1877, 652, 259, 50639, 775, 1100, 2303),
  `Ogooue-Lolo` = c(17018, 8149, 1368, 721, 247, 601, 43971, 1623, 688),
  `Ogooue-Maritime` = c(21724, 1421, 2910, 1695, 1273, 227, 643, 89515, 890),
  `Woleu-Ntem` = c(65450, 2249, 2246, 1011, 523, 1399, 529, 3408, 107763),
  check.names = FALSE  
  ) %>%
  # there are also foreign born which aren't here
  pivot_longer(
    cols = -destination_province,
    names_to = "birth_province",
    values_to = "m_raw"
  ) %>% 
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
    mig_end_yr = 2013,
    mig_prd_len = NA,
    mig_measure = "since birth"
  ) %>% 
  select(-c(m_raw, nonmig, birth_province, destination_province))

saveRDS(mig, here(build.dir, "Countries", "GAB", "13_mig_aggregates.rds"))
