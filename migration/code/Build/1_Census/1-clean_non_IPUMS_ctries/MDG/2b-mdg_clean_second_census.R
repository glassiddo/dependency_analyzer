source("code/SSA_env_SetUp.R")

#### some of the cleaning here might need to improved (e.g, industry)
original <- read_sav(here(raw.dir, "Countries", "MDG", "Census", "2018",
                          "INSTAT_BD_SPSS_RESIDENTS_10pc_RGPH-3_2018.sav")) %>%
  dtplyr::lazy_dt()

district_match <- original %>% 
  select(
    prev_province = PROVINCE, 
    birth_province = PROVINCE,
    prev_region = REGION,
    birth_region = REGION,
    prev_district = DISTRICT,
    birth_district = DISTRICT
  ) %>% 
  unique()

ctry_df <- original %>% 
  mutate(
    province = PROVINCE,
    birth_district = P09D,
    prev_district = P12D,
    migration_x_yrs_ago = P11,
  ) %>% 
  left_join(district_match %>% select(contains("birth")), by = "birth_district") %>% 
  left_join(district_match %>% select(contains("prev")), by = "prev_district") %>% 
  mutate(
    country = "450",
    last_province = as.numeric(paste0(country, "00", prev_province)),
    last_district = as.numeric(paste0(
      last_province, 
      #"0", 
      prev_district
      ))
  ) %>% 
  transmute(
    country = country,
    year = 2018,
    geo1_mg = as.numeric(paste0(country, "00", PROVINCE)),
    geo2_mg = as.numeric(paste0(
      geo1_mg, 
      #"0", 
      DISTRICT
      )),
    mig1_10_mg = case_when(
      migration_x_yrs_ago <= 10 & !is.na(last_province) ~ 
        last_province,
      TRUE ~ geo1_mg 
      # assuming that if not migrated in that period,
      # lived in the same province before
      # but it could also indicate unknown...
    ),
    mig2_10_mg = case_when(
      migration_x_yrs_ago <= 10 & !is.na(last_district) ~ 
        last_district,
      TRUE ~ geo2_mg 
    ),
    bplmg1 = case_when(
      !is.na(birth_province) ~ as.numeric(paste0(country, "00", birth_province)),
      TRUE ~ as.numeric(paste0(country, "0", 98)) # unknown
    ),
    bplmg2 = case_when(
      !is.na(birth_district) ~ as.numeric(paste0(
        bplmg1, 
        #"0", 
        birth_district
        )),
      TRUE ~ as.numeric(paste0(bplmg1, 998)) # unknown
    ),
    urban = ifelse(MILIEU == 2, 1, 2), # urban should be 2, rural 1
    age = P08,
    empstat = case_when(
      P23 == 1 ~ 1, # employed
      P23 %in% c(2,3) ~ 2, # unemployed
      P23 %in% c(3,4,5,6,7,8) ~ 3, # inactive
      TRUE ~ 0 # NIU
    ),
    indgen = case_when( # need to verify those
      P26C %in% 1:3 ~ 10, # agriculture
      P26C %in% 5:9 ~ 20, # mining
      P26C %in% 10:33 ~ 30, # manufacturing
      P26C %in% 35:99 ~ 110, # services - various types 
      # but no need to tell apart which one 
      is.na(P26C) ~ 0, # NIU
      TRUE ~ 999 ## unknown
    ),
    perwt = 10 # seems like its constant! (compared official pop. records to numbers here)
    # other variables that exist - nationality, reason for last migration,
    # whether the household is engaged in agriculture/livestock/fishing/none (F1-F4)
  ) %>% 
  as.data.frame()

geo1_labels <- c(
  "Antananarivo" = 450001,
  "Fianarantsoa" = 450002,
  "Toamasina" = 450003,
  "Mahajanga" = 450004,
  "Toliary" = 450005,
  "Antsiranana" = 450006,
  "Unknown" = 450098
)

# geo2_labels <- c(
#   "Analamanga" = 450001011,
#   "Vakinankaratra" = 450001012,
#   "Itasy" = 450001013,
#   "Bongolava" = 450001014,
#   "Haute Matsiatra" = 450002021,
#   "Amoron I Mania" = 450002022,
#   "Vatovavy Fitovinany" = 450002023,
#   "Ihorombe" = 450002024,
#   "Atsimo Atsinanana" = 450002025,
#   "Atsinanana" = 450003031,
#   "Analanjirofo" = 450003032,
#   "Alaotra Mangoro" = 450003033,
#   "Boeny" = 450004041,
#   "Sofia" = 450004042,
#   "Betsiboka" = 450004043,
#   "Melaky" = 450004044,
#   "Atsimo Andrefana" = 450005051,
#   "Androy" = 450005052,
#   "Anosy" = 450005053,
#   "Menabe" = 450005054,
#   "Diana" = 450006061,
#   "Sava" = 450006062
# )

geo2_labels <- c(
  # Antananarivo (450001)
  "Antananarivo Renivohitra" = 450001111,
  "Ankazobe" = 450001112,
  "Anjozorobe" = 450001113,
  "Ambohidratrimo" = 450001114,
  "Manjakandriana" = 450001115,
  "Antananarivo Avaradrano" = 450001116,
  "Antananarivo Atsimondrano" = 450001117,
  "Andramasina" = 450001118,
  "Antsirabe I" = 450001121,
  "Ambatolampy" = 450001122,
  "Faratsiho" = 450001123,
  "Mandoto" = 450001124,
  "Antanifotsy" = 450001125,
  "Betafo" = 450001126,
  "Antsirabe II" = 450001127,
  "Miarinarivo" = 450001131,
  "Arivonimamo" = 450001132,
  "Soavinandriana" = 450001133,
  "Tsiroanomandidy" = 450001141,
  "Fenoarivobe" = 450001142,
  
  # Fianarantsoa (450002)
  "Fianarantsoa I" = 450002211,
  "Ambohimahasoa" = 450002212,
  "Ikalamavony" = 450002213,
  "Isandra" = 450002214,
  "Lalangina" = 450002215,
  "Vohibato" = 450002216,
  "Ambalavao" = 450002217,
  "Ambositra" = 450002221,
  "Fandriana" = 450002222,
  "Ambatofinandrahana" = 450002223,
  "Manandriana" = 450002224,
  "Manakara Atsimo" = 450002231,
  "Nosy-Varika" = 450002232,
  "Mananjary" = 450002233,
  "Ifanadiana" = 450002234,
  "Ikongo" = 450002235,
  "Vohipeno" = 450002236,
  "Ihosy" = 450002241,
  "Ivohibe" = 450002242,
  "Iakora" = 450002243,
  "Farafangana" = 450002251,
  "Vondrozo" = 450002252,
  "Vangaindrano" = 450002253,
  "Midongy-Atsimo" = 450002254,
  "Befotaka" = 450002255,
  
  # Toamasina (450003)
  "Toamasina I" = 450003311,
  "Toamasina II" = 450003312,
  "Brickaville" = 450003313,
  "Vatomandry" = 450003314,
  "Antanambao Manampontsy" = 450003315,
  "Mahanoro" = 450003316,
  "Marolambo" = 450003317,
  "Fenerive Est" = 450003321,
  "Maroantsetra" = 450003322,
  "Mananara-Avaratra" = 450003323,
  "Soanierana Ivongo" = 450003324,
  "Sainte Marie" = 450003325,
  "Vavatenina" = 450003326,
  "Ambatondrazaka" = 450003331,
  "Andilamena" = 450003332,
  "Amparafaravola" = 450003333,
  "Moramanga" = 450003334,
  "Anosibe-An'Ala" = 450003335,
  
  # Mahajanga (450004)
  "Mahajanga I" = 450004411,
  "Mahajanga II" = 450004412,
  "Mitsinjo" = 450004413,
  "Marovoay" = 450004414,
  "Soalala" = 450004415,
  "Ambato Boeni" = 450004416,
  "Antsohihy" = 450004421,
  "Analalava" = 450004422,
  "Bealanana" = 450004423,
  "Befandriana Nord" = 450004424,
  "Port-Berge (Boriziny-Vaovao)" = 450004425,
  "Mandritsara" = 450004426,
  "Mampikony" = 450004427,
  "Maevatanana" = 450004431,
  "Tsaratanana" = 450004432,
  "Kandreho" = 450004433,
  "Maintirano" = 450004441,
  "Besalampy" = 450004442,
  "Ambatomainty" = 450004443,
  "Morafenobe" = 450004444,
  "Antsalova" = 450004445,
  
  # Toliary (450005)
  "Toliary I" = 450005511,
  "Beroroha" = 450005512,
  "Morombe" = 450005513,
  "Ankazoabo" = 450005514,
  "Sakaraha" = 450005515,
  "Toliary II" = 450005516,
  "Benenitra" = 450005517,
  "Betioky Atsimo" = 450005518,
  "Ampanihy Ouest" = 450005519,
  "Ambovombe-Androy" = 450005521,
  "Bekily" = 450005522,
  "Beloha" = 450005523,
  "Tsihombe" = 450005524,
  "Taolagnaro" = 450005531,
  "Betroka" = 450005532,
  "Amboasary-Atsimo" = 450005533,
  "Morondava" = 450005541,
  "Miandrivazo" = 450005542,
  "Belo Sur Tsiribihina" = 450005543,
  "Mahabo" = 450005544,
  "Manja" = 450005545,
  
  # Antsiranana (450006)
  "Antsiranana I" = 450006611,
  "Antsiranana II" = 450006612,
  "Ambilobe" = 450006613,
  "Nosy-Be" = 450006614,
  "Ambanja" = 450006615,
  "Sambava" = 450006621,
  "Vohemar" = 450006622,
  "Andapa" = 450006623,
  "Antalaha" = 450006624
)

urban_labels <- c(
  "Rural" = 1,
  "Urban" = 2
)

ctry_df <- ctry_df %>%
  mutate(
    geo1_mg = labelled(geo1_mg, labels = geo1_labels),
    mig1_10_mg = labelled(mig1_10_mg, labels = geo1_labels),
    bplmg1 = labelled(bplmg1, labels = geo1_labels),
    geo2_mg = labelled(geo2_mg, labels = geo2_labels),
    mig2_10_mg = labelled(mig2_10_mg, labels = geo2_labels),
    bplmg2 = labelled(bplmg2, labels = geo2_labels),
    urban = labelled(urban, labels = urban_labels)
  )

# ctry_df %>% 
#   transmute(
## should be 9 
#     s1 = str_length(geo2_mg),
#     s2 = str_length(mig2_5_mg), 
#     s3 = str_length(bplmg2)) %>%
#   distinct()

rm(original, district_match)

write_dta(ctry_df, here(
  raw.dir, "Countries", "MDG", "Census", "consistent18.dta"
))
