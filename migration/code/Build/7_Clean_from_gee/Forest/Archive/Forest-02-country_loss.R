# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Jan 19, 2022
# Title:   Build Forest Loss Panels
# Desc:    Create Forest Loss Panels, 5-year aggregates, and cover for each country
# Output:  
#          
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")
source("code/Functions/F-Forest.R")

forestloss_df <- read_dta( paste0(forest.dir, "lossbyyear.dta" )) %>%
  remove_all_labels()

forestloss30_df <- readRDS(file.path(build.dir,"Forest Cover/lossbyyear30.rds"))

#*******************************************************************************
# Benin ----
#*******************************************************************************
# create panel
ben_df <- get_country("Benin")
ben30_df <- get_country30("Benin")

# create ids to match with census data
# from Liam: codes are in alphabetical order region name + 10 * district name 
# in alphabetical order
dist_id_df <- ben_df %>%
  select(district_code, district_name, region_name, region_code) %>%
  unique() %>%
  arrange(region_name, district_name) %>%
  group_by(region_name) %>%
  mutate(dist_id = row_number(district_name))

reg_id_df <- ben_df %>%
  select(region_name, region_code) %>%
  unique() %>%
  arrange(region_name) %>%
  mutate(reg_id = row_number(region_name))

id_df <- full_join(dist_id_df, reg_id_df) %>%
  mutate(district = 10* reg_id + dist_id) %>%
  select(-reg_id, -dist_id)

ben_df <- full_join(ben_df, id_df)

ben_5yr <- f_5yr_loss( ben_df ) %>%
  full_join(id_df )

# forest cover at start and end of period
ben_cover <- f_cover(ben_df, district_name)

saveRDS( ben_5yr, paste0(ben.dir, "Forest Cover/loss_5yr.rds"))
saveRDS( ben_df, paste0(ben.dir, "Forest Cover/loss_ann.rds"))
saveRDS( ben_cover, paste0(ben.dir, "Forest Cover/forest_cover.rds"))
saveRDS( ben30_df, paste0(ben.dir, "Forest Cover/loss30.rds"))

  
rm(dist_id_df, reg_id_df, id_df)

#*******************************************************************************
# Botswana ----
#*******************************************************************************

bwa30_df <- read_dta(
  "data/Hansen Forest Loss/Output/lossbyyear30_Botswana.dta") %>%
  remove_all_labels() %>%
  loss_long(c(geolevel1, admin_name)) %>%
  select(-geolevel1) %>%
  rename(district_name = admin_name) 

saveRDS( bwa30_df, paste0(build.dir, "Botswana/Forest Cover/loss30.rds"))

#*******************************************************************************
# Burkina Faso ----
#*******************************************************************************

bfa_df <- get_country("Burkina Faso") %>%
  mutate(district_name = ifelse(district_name == "Komonjdjari",
                                "Komandjoari", district_name))

bfa30_df <- get_country30("Burkina Faso") %>%
  mutate(district_name = ifelse(district_name == "Komonjdjari",
                                "Komandjoari", district_name))

saveRDS( bfa_df, paste0(build.dir, "Burkina Faso/Forest Cover/loss_ann.rds"))
saveRDS( bfa30_df, paste0(build.dir, "Burkina Faso/Forest Cover/loss30.rds"))


#*******************************************************************************
# Guinea ----
#*******************************************************************************

gin_df <- get_country("Guinea") %>%
  mutate(
    district_name = case_when(
      district_name == "Telemele" ~ "Telimele",
      district_name %in% c("Coyah", "Dubreka") ~ "Coyah, Dubreka",
      # sub prefectures of Conakry the capital
      district_name %in% c("Dixinn", "Kaloum", "Matam", "Matoto", "Ratoma") 
      ~ "Conakry",
      TRUE ~ district_name
    )
  ) %>%
  # aggregate across the combined districts
  group_by(district_name, region_name, year) %>%
  summarise(across(c(loss, treecover, area), ~sum(.x)), .groups = 'drop') %>%
  mutate(region_name = ifelse(district_name == "Fria", "Boke", region_name))

gin30_df <- get_country30("Guinea") %>%
  mutate(
    district_name = case_when(
      district_name == "Telemele" ~ "Telimele",
      district_name %in% c("Coyah", "Dubreka") ~ "Coyah, Dubreka",
      # sub prefectures of Conakry the capital
      district_name %in% c("Dixinn", "Kaloum", "Matam", "Matoto", "Ratoma") 
      ~ "Conakry",
      TRUE ~ district_name
    )
  ) %>%
  # aggregate across the combined districts
  group_by(district_name, region_name, year) %>%
  summarise(across(c(loss, treecover), ~sum(.x)), .groups = 'drop') %>%
  mutate(region_name = ifelse(district_name == "Fria", "Boke", region_name))

saveRDS( gin_df, paste0(build.dir, "Guinea/Forest Cover/loss_ann.rds"))
saveRDS( gin30_df, paste0(build.dir, "Guinea/Forest Cover/loss30.rds"))



#*******************************************************************************
# Ghana ----
#*******************************************************************************

gha30_df <- read_dta(
  "data/Hansen Forest Loss/Output/lossbyyear30_Ghana.dta") %>%
  remove_all_labels() %>%
  loss_long(c(geolevel2, admin_name)) %>%
  select(-geolevel2) %>%
  rename(district_name = admin_name) %>%
  mutate(district_name = str_to_title(district_name))

saveRDS(gha30_df, paste0(build.dir, "Ghana/Forest Cover/loss30.rds"))

#*******************************************************************************
# Kenya ----
#*******************************************************************************

ken_df <- get_country("Kenya")
ken30_df <- get_country30("Kenya") 

ken30_df %<>%
  mutate(
    district_name = case_when(
      district_name %in% c("Baringo", "Koibatek", "Laikipia")
      ~paste0("Baringo, Baringo North, East Pokot, Koibatek, Laikipia East, ",
               "Laikipia North, Laikipia West"),
      district_name %in% c("Bomet", "Buret", "Kericho")
      ~"Bomet, Buret, Kericho, Kipkelion, Sotik",
      district_name %in% c("Bondo", "Siaya") ~"Bondo, Rarieda, Siaya",
      district_name %in% c("Gucha", "Nyamira")
      ~paste0("Borabu, Gucha, Gucha South, Kisii Central, Kisii South, Manga, ",
              "Masaba, Nyamira"),
      district_name %in% c("Bungoma", "Mt Elgon")
      ~ "Bungoma East, Bungoma North, Bungoma South, Bungoma West, Mt. Elgon",
      district_name %in% c("Busia", "Teso")
      ~"Bunyala, Busia, Samia, Teso North, Teso South",
      district_name %in% c("Butere Mumias", "Kakamega", "Lugari", "Vihiga")
      ~paste0("Butere, Emuhaya, Hamisi, Kakamega Central, Kakamega East, ",
              "Kakamega North, Kakamega South, Lugari, Mumias, Vihiga"),
      district_name %in% c("Marsabit", "Moyale") 
      ~"Chalbi, Laisamis, Marsabit, Moyale",
      # This one DNE
      district_name %in% c("Eldoret East", "Eldoret West", "Wareng")
      ~"Eldoret East, Eldoret West, Wareng",
      district_name %in% c("Embu", "Machakos", "Makueni", "Mbeere")
      ~paste0("Embu, Kangundo, Kibwezi, Machakos, Makueni, Mbeere, Mbooni, ",
      "Mwala, Nzaui, Yatta"),
      district_name %in% c("Garissa", "Ijara") ~"Fafi, Garissa, Ijara, Lagdera",
      district_name %in% c("Isiolo", "Meru South", "Tharaka")
      ~paste0("Garba Tulla, Igembe, Imenti Central, Imenti North, ",
      "Imenti South, Isiolo, Maara, Meru South, Tharaka, Tigania"),
      district_name %in% c("Kiambu", "Muranga", "Nyandarua", "Thika")
      ~paste0("Gatanga, Gatundu, Githunguri, Kiambu (Kiambaa), Kiambu West, ",
      "Kikuyu, Lari, Muranga North, Muranga South, Nyandarua North, ",
      "Nyandarua South, Ruiru, Thika East, Thika West"),
      district_name %in% c("Homa Bay", "Kuria", "Migori", "Rachuonyo", "Suba")
      ~"Homa Bay, Kuria East, Kuria West, Migori, Rachuonyo, Rongo, Suba",
      district_name %in% c("Kajiado", "Nakuru")
      ~paste0("Kaijiado Central, Kaijiado North, Loitoktok, Molo, Naivasha, ",
      "Nakuru, Nakuru North"),
      district_name %in% c("Kilifi", "Malindi") ~"Kaloleni, Kilifi, Malindi",
      district_name %in% c("Keiyo", "Marakwet") ~"Keiyo, Marakwet",
      district_name %in% c("Kinango", "Kwale", "Msambweni") 
      ~"Kinango, Kwale, Msambweni",
      district_name %in% c("Kisumu", "Nyando") 
      ~"Kisumu East, Kisumu West, Nyando",
      district_name %in% c("Kitui", "Mwingi") 
      ~"Kitui North, Kitui South (Mutomo), Kyuso, Mwingi",
      district_name == "Trans Nzoia" 
      ~"Kwanza, Trans Nzoia East, Trans Nzoia West",
      district_name == "Mandera" ~"Mandera Central, Mandera East, Mandera West",
      district_name %in% c("Mombasa", "Kilindini") ~"Mombasa, Kilindini",
      district_name %in% c("Nairobi", "Westlands") 
      ~"Nairobi East, Nairobi North, Nairobi West, Westlands",
      district_name %in% c("Nandi North", "Nandi South", "Tinderet") 
      ~"Nandi Central, Nandi East, Nandi North, Nandi South, Tinderet",
      district_name %in% c("Narok", "Trans Mara") 
      ~"Narok North, Narok South, Trans Mara",
      district_name == "Nyeri" ~"Nyeri North, Nyeri South",
      district_name == "West Pokot" ~"Pokot Central, Pokot North, West Pokot",
      district_name == "Samburu" 
      ~"Samburu Central, Samburu East, Samburu North",
      district_name == "Taita Taveta" ~"Taita, Taveta",
      district_name == "Tana River" ~"Tana Delta, Tana River",
      district_name == "Turkana" 
      ~"Turkana Central, Turkana North, Turkana South",
      district_name == "Wajir" ~"Wajir East, Wajir North, Wajir South, Wajir West",
      TRUE~district_name)) %>%
  # aggregate across the combined districts
  group_by(district_name, region_name, year) %>%
  summarise(across(c(loss, treecover), ~sum(.x)), .groups = 'drop')

ken_df %<>% select(district_name) %>% unique()

saveRDS( ken_df, paste0(build.dir, "Kenya/Forest Cover/loss_ann.rds"))
saveRDS( ken30_df, paste0(build.dir, "Kenya/Forest Cover/loss30.rds"))

#*******************************************************************************
# Lesotho ----
#*******************************************************************************

lso30_df <- get_country30("Lesotho") 
saveRDS( lso30_df, paste0(build.dir, "Lesotho/Forest Cover/loss30.rds"))

#*******************************************************************************
# Mali ----
#*******************************************************************************

mli_df <- get_country("Mali") %>%
  mutate(
    district_name = ifelse(
      district_name %in% c("Kidal", "Abeibara", "Tessalit", "Tin-Essako"),
      "Kidal, Abeibara, Tessalit, Tin-Essako", district_name),
    region_name = ifelse(
      region_name %in% c("Gao", "Kidal"),"Gao, Kidal", region_name)) %>%
  # aggregate across the combined districts
  group_by(district_name, region_name, year) %>%
  summarise(across(c(loss, treecover, area), ~sum(.x)), .groups = 'drop') 

saveRDS( mli_df, paste0(build.dir, "Mali/Forest Cover/loss_ann.rds"))

mli30_df <- get_country30("Mali") %>%
  mutate(
    district_name = ifelse(
      district_name %in% c("Kidal", "Abeibara", "Tessalit", "Tin-Essako"),
      "Kidal, Abeibara, Tessalit, Tin-Essako", district_name),
    region_name = ifelse(
      region_name %in% c("Gao", "Kidal"),"Gao, Kidal", region_name)) %>%
  # aggregate across the combined districts
  group_by(district_name, region_name, year) %>%
  summarise(across(c(loss, treecover), ~sum(.x)), .groups = 'drop') 

saveRDS( mli30_df, paste0(build.dir, "Mali/Forest Cover/loss30.rds"))

#*******************************************************************************
# Mozambique ----
#*******************************************************************************

moz_df <- get_country("Mozambique")
moz30_df <- get_country30("Mozambique")

moz_df %<>%
  mutate(
    district_name = str_to_title(district_name),
    district_name = case_when(
      district_name %in% c("Chigubo", "Massangena")~ "Chigubo, Massangena",
      district_name %in% c("Ibo", "Quissanga")~ "Ibo, Quissanga",
      district_name %in% c("Mavago", "Mecula")~ "Mavago, Mecula",
      district_name %in% c("Morrumbene", "Maxixe")~ "Morrumbene, Maxixe",
      district_name == "Pemba"~ "Metuge",
      district_name == "Luenha"~ "Changara",
      district_name == "Lago Niassa"~ "Lago",
      substr(district_name, 1, 6) == "Nacala"~ "Nacala",
      TRUE~district_name)) %>%
  # aggregate across the combined districts
  group_by(district_name, region_name, year) %>%
  summarise(across(c(loss, treecover, area), ~sum(.x)), .groups = 'drop')

moz30_df %<>%
  mutate(
    district_name = str_to_title(district_name),
    district_name = case_when(
      district_name %in% c("Chigubo", "Massangena")~ "Chigubo, Massangena",
      district_name %in% c("Ibo", "Quissanga")~ "Ibo, Quissanga",
      district_name %in% c("Mavago", "Mecula")~ "Mavago, Mecula",
      district_name %in% c("Morrumbene", "Maxixe")~ "Morrumbene, Maxixe",
      district_name == "Pemba"~ "Metuge",
      district_name == "Luenha"~ "Changara",
      district_name == "Lago Niassa"~ "Lago",
      substr(district_name, 1, 6) == "Nacala"~ "Nacala",
      TRUE~district_name)) %>%
  # aggregate across the combined districts
  group_by(district_name, region_name, year) %>%
  summarise(across(c(loss, treecover), ~sum(.x)), .groups = 'drop')

saveRDS( moz_df, paste0(build.dir, "Mozambique/Forest Cover/loss_ann.rds"))
saveRDS( moz30_df, paste0(build.dir, "Mozambique/Forest Cover/loss30.rds"))

#*******************************************************************************
# Mauritius ----
#*******************************************************************************

mus30_df <- get_country30("Mauritius") %>%
  # this seems to be the match
  mutate(region_name = ifelse(
    region_name == "Administrative unit not available",
    "Rodrigues, Agalega Islands, Saint Brandon",
    region_name))
saveRDS( mus30_df, paste0(build.dir, "Mauritius/Forest Cover/loss30.rds"))


#*******************************************************************************
# Senegal ----
#*******************************************************************************

sen_df <- get_country("Senegal") %>%
  mutate(
    district_name = case_when(
      district_name == "Birkilane"~ "Birkelane",
      district_name == "Koungueul"~ "Koungheul",
      district_name == "Mbacke"~ "M'backe",
      district_name == "Mbour"~ "M'bour",
      district_name == "Tivaoune"~ "Tivaouane",
      district_name == "Medina yoro foula"~ "Medina Yoro Foulah",
      district_name == "Maleme hodar"~ "Malem Hoddar",
      TRUE~district_name)
  ) %>%
  combine_districts("Senegal") %>%
  # must exclude region name at the moment because of combinations...
  group_by(district_name, year) %>%
  summarise(across(c(loss, treecover, area), ~sum(.x)), .groups = 'drop')

sen30_df <- get_country30("Senegal") %>%
  mutate(
    district_name = case_when(
      district_name == "Birkilane"~ "Birkelane",
      district_name == "Koungueul"~ "Koungheul",
      district_name == "Mbacke"~ "M'backe",
      district_name == "Mbour"~ "M'bour",
      district_name == "Tivaoune"~ "Tivaouane",
      district_name == "Medina yoro foula"~ "Medina Yoro Foulah",
      district_name == "Maleme hodar"~ "Malem Hoddar",
      TRUE~district_name)
  ) %>%
  combine_districts("Senegal") %>%
  # must exclude region name at the moment because of combinations...
  group_by(district_name, year) %>%
  summarise(across(c(loss, treecover), ~sum(.x)), .groups = 'drop')


saveRDS( sen_df, paste0(build.dir, "Senegal/Forest Cover/loss_ann.rds"))
saveRDS( sen30_df, paste0(build.dir, "Senegal/Forest Cover/loss30.rds"))

#*******************************************************************************
# Sierra Leone ----
#*******************************************************************************

sle30_df <- get_country30("Sierra Leone") 
saveRDS( sle30_df, paste0(build.dir, "Sierra Leone/Forest Cover/loss30.rds"))

#*******************************************************************************
# South Africa ----
#*******************************************************************************
# create panel
saf_df <- read_dta( "data/Hansen Forest Loss/Output/lossbyyear_SA.dta") %>%
  select(mn_name, pr_name, year, loss_, treecover, area) %>%
  remove_all_labels() %>%
  rename(loss = loss_, district_name = mn_name, region_name = pr_name) %>%
  mutate(
    district_name = case_when(
      district_name == "!Kheis" ~"Kheis",
      district_name == "//Khara Hais" ~"Khara Hais",
      district_name == "KhÃ¯Â¿Â½i-Ma" ~"Khai-Ma",
      district_name == "Emalahleni" & region_name == "Eastern Cape" 
      ~"Emalahleni-EC",
      district_name == "Emalahleni" & region_name == "Mpumalanga"
      ~"Emalahleni-MP",
      district_name == "Naledi" & region_name == "Free State"
      ~"Naledi-FS",
      district_name == "Naledi" & region_name == "North West"
      ~"Naledi-NW",
      TRUE~district_name
    )) %>%
  group_by(district_name) %>%
  mutate(treecover = mean(treecover, na.rm = TRUE),
         area = mean(area, na.rm = TRUE)) %>%
  ungroup()

saf_5yr <- f_5yr_loss( saf_df ) %>%
  mutate(
    loss_nna = ifelse(log_loss == -Inf, NA, log_loss),
    log_loss = ifelse(log_loss == -Inf, min(loss_nna, na.rm = TRUE), log_loss),
    loss_nna = ifelse(log_loss_area == -Inf, NA, log_loss_area),
    log_loss_area = ifelse(log_loss_area == -Inf, min(loss_nna, na.rm = TRUE), 
                           log_loss_area),
    loss_nna = ifelse(log_loss_cover == -Inf, NA, log_loss_cover),
    log_loss_cover = ifelse(log_loss_cover == -Inf, min(loss_nna, na.rm = TRUE), 
                            log_loss_cover)) %>%
  select(-loss_nna)
  

# forest cover at start and end of period
saf_cover <- f_cover(saf_df, district_name)

# 30 percent cutoff 
saf30_df <- read_dta(
  "data/Hansen Forest Loss/Output/lossbyyear30_SA.dta") %>%
  remove_all_labels() %>%
  loss_long(starts_with("mn")) %>%
  select(mn_name, mn_mdb_c, year, loss, treecover) %>%
  rename(district_name = mn_name, region_name = mn_mdb_c) %>%
  mutate(
    region_name = substr(region_name, 1, 2),
    district_name = case_when(
      district_name == "!Kheis" ~"Kheis",
      district_name == "//Khara Hais" ~"Khara Hais",
      district_name == "KhÃ¯Â¿Â½i-Ma" ~"Khai-Ma",
      district_name == "Emalahleni" & region_name == "EC" ~"Emalahleni-EC",
      district_name == "Emalahleni" & region_name == "MP" ~"Emalahleni-MP",
      district_name == "Naledi" & region_name == "FS" ~"Naledi-FS",
      district_name == "Naledi" & region_name == "NW" ~"Naledi-NW",
      TRUE~district_name)) %>%
  select(-region_name)

saveRDS( saf_5yr, paste0(saf.dir, "Forest Cover/loss_5yr.rds"))
saveRDS( saf_df, paste0(saf.dir, "Forest Cover/loss_ann.rds"))
saveRDS( saf_cover, paste0(saf.dir, "Forest Cover/forest_cover.rds"))
saveRDS( saf30_df, paste0(saf.dir, "Forest Cover/loss30.rds"))


# consistent
zaf30_df <- get_country30("South Africa") %>%
  combine_districts(Country = "South Africa")
saveRDS( zaf30_df, paste0(build.dir, "South Africa/Forest Cover/loss30d.rds"))


#*******************************************************************************
# Tanzania ----
#*******************************************************************************

# create panel
tza_df <- get_country("United Republic of Tanzania") %>%
  mutate(
    # rename districts so consistent with shape files
    district_name = ifelse(district_name == "Chake Chake", "Chakechake", district_name),
    district_name = ifelse(district_name == "Handeni Township Authority", "Handeni Mji", district_name),
    district_name = ifelse(district_name == "Kaskazini A", "Kaskazini ‘A’", district_name),
    district_name = ifelse(district_name == "Kaskazini B", "Kaskazini ‘B’", district_name),
    district_name = ifelse(district_name == "Kigoma Municipal-Ujiji", "Kigoma Urban", district_name),
    district_name = ifelse(district_name == "Mafinga Township Authority", "Mafinga", district_name),
    district_name = ifelse(district_name == "Makambako Township Authority", "Makambako", district_name),
    district_name = ifelse(district_name == "Masasi Township Authority", "Masasi  Township Authority", district_name),
    district_name = ifelse(district_name == "Mbarali", "Mbalali", district_name),
    district_name = ifelse(district_name == "Micheweni", "Michweweni", district_name),
    district_name = ifelse(district_name == "Morogoro", "Morogoro Rural", district_name),
    district_name = ifelse(district_name == "Moshi", "Moshi Rural", district_name),
    district_name = ifelse(district_name == "Moshi Municipal", "Moshi Urban", district_name),
    district_name = ifelse(district_name == "Mtwara Urban", "Mtwara Mikindani", district_name),
    district_name = ifelse(district_name == "Musoma", "Musoma Rural", district_name),
    district_name = ifelse(district_name == "Musoma Municipal", "Musoma Urban", district_name),
    district_name = ifelse(district_name == "Nyang'wale", "Nyang'hwale", district_name),
    district_name = ifelse(district_name == "Singida", "Singida Rural", district_name),
    district_name = ifelse(district_name == "Tanga", "Tanga Urban", district_name),
    district_name = ifelse(district_name == "Urambo", "Uramba", district_name),
    # create names for consistent regions
    region_name_cons = case_when(
      region_name == "Dar-es-salaam" ~ "Dar es Salaam",
      region_name %in% c("Arusha", "Manyara") ~ "Arusha, Manyara",
      region_name %in% c("Geita", "Kagera", "Mwanza", "Shinyanga", "Simiyu") 
      ~ "Geita, Kagera, Mwanza, Shinyanga, Simiyu",
      region_name %in% c("Iringa", "Njombe") ~ "Iringa, Njombe",
      region_name %in% c("Katavi", "Rukwa") ~ "Katavi, Rukwa",
      region_name == "Kaskazini Pemba" ~ "Pemba North",
      region_name == "Kusini Pemba" ~ "Pemba South",
      region_name == "Kaskazini Unguja" ~ "Zanzibar North",
      region_name == "Mjini Magharibi" ~ "Zanzibar Town/West",
      region_name == "Kusini Unguja" ~ "Zanzibar South",
      TRUE ~ region_name)) %>%
  filter(district_name != "Administrative unit not available") 

tza_5yr <- f_5yr_loss( tza_df )

# forest cover at start and end of period
tza_cover <- f_cover(tza_df, district_name) 

tza30_df <- get_country("United Republic of Tanzania", forestloss30_df) %>%
  mutate(
    # rename districts so consistent with shape files
    district_name = ifelse(district_name == "Chake Chake", "Chakechake", district_name),
    district_name = ifelse(district_name == "Handeni Township Authority", "Handeni Mji", district_name),
    district_name = ifelse(district_name == "Kaskazini A", "Kaskazini ‘A’", district_name),
    district_name = ifelse(district_name == "Kaskazini B", "Kaskazini ‘B’", district_name),
    district_name = ifelse(district_name == "Kigoma Municipal-Ujiji", "Kigoma Urban", district_name),
    district_name = ifelse(district_name == "Mafinga Township Authority", "Mafinga", district_name),
    district_name = ifelse(district_name == "Makambako Township Authority", "Makambako", district_name),
    district_name = ifelse(district_name == "Masasi Township Authority", "Masasi  Township Authority", district_name),
    district_name = ifelse(district_name == "Mbarali", "Mbalali", district_name),
    district_name = ifelse(district_name == "Micheweni", "Michweweni", district_name),
    district_name = ifelse(district_name == "Morogoro", "Morogoro Rural", district_name),
    district_name = ifelse(district_name == "Moshi", "Moshi Rural", district_name),
    district_name = ifelse(district_name == "Moshi Municipal", "Moshi Urban", district_name),
    district_name = ifelse(district_name == "Mtwara Urban", "Mtwara Mikindani", district_name),
    district_name = ifelse(district_name == "Musoma", "Musoma Rural", district_name),
    district_name = ifelse(district_name == "Musoma Municipal", "Musoma Urban", district_name),
    district_name = ifelse(district_name == "Nyang'wale", "Nyang'hwale", district_name),
    district_name = ifelse(district_name == "Singida", "Singida Rural", district_name),
    district_name = ifelse(district_name == "Tanga", "Tanga Urban", district_name),
    district_name = ifelse(district_name == "Urambo", "Uramba", district_name)
  ) %>%
  filter(district_name != "Administrative unit not available") 

# regional loss for consistent regions in census
tza30reg_df <- get_country30("United Republic of Tanzania") %>%
  mutate(
    region_name = case_when(
      region_name %in% c("Arusha", "Manyara") ~ "Arusha, Manyara",
      region_name %in% c("Geita", "Kagera", "Mwanza", "Shinyanga", "Simiyu") 
      ~ "Geita, Kagera, Mwanza, Shinyanga, Simiyu",
      region_name %in% c("Iringa", "Njombe") ~ "Iringa, Njombe",
      region_name %in% c("Katavi", "Rukwa") ~ "Katavi, Rukwa",
      region_name == "Dar-es-salaam" ~ "Dar Es Salaam",
      region_name == "Kaskazini Pemba" ~ "Pemba North",
      region_name == "Kusini Pemba" ~ "Pemba South",
      region_name == "Kaskazini Unguja" ~"Zanzibar North",
      region_name == "Mjini Magharibi" ~"Zanzibar Town/West",
      region_name == "Kusini Unguja" ~"Zanzibar South",
      TRUE~region_name)) %>%
  group_by(region_name, year) %>%
  summarise(across(c(loss, treecover), ~sum(.x)), .groups = 'drop') 

saveRDS( tza_df, paste0(tza.dir, "Forest Cover/loss_ann.rds"))
saveRDS( tza_5yr, paste0(tza.dir, "Forest Cover/loss_5yr.rds"))
saveRDS( tza_cover, paste0(tza.dir, "Forest Cover/forest_cover.rds"))
saveRDS( tza30_df, paste0(tza.dir, "Forest Cover/loss30.rds"))

saveRDS( tza30reg_df, paste0(tza.dir, "Forest Cover/loss30_regional.rds"))


#*******************************************************************************
# Uganda ----
#*******************************************************************************

# create panel
uga_df <- read_dta( "data/Hansen Forest Loss/Output/lossbyyear_uganda.dta") %>%
  select(d, dname2019, year, loss_, treecover, area) %>%
  remove_all_labels() %>%
  rename(loss = loss_, district_name = d) %>%
  group_by(district_name) %>%
  mutate(treecover = mean(treecover, na.rm = TRUE),
         area = mean(area, na.rm = TRUE))

uga_5yr <- f_5yr_loss( uga_df %>% mutate(region_name = "All") ) %>%
  select(-region_name)

# forest cover at start and end of period
uga_cover <- f_cover(uga_df, district_name) 

uga30_df <- get_country30("Uganda") %>%
  select(-district_name, -district_code, -region_code) %>%
  rename(district_name = region_name) %>%
  mutate(district_name = case_when(
    district_name %in% c("Adjumani", "Moyo")~"Adjumani, Moyo",
    district_name %in% c("Apac", "Oyam", "Kole")~"Apac, Oyam, Kole",
    district_name %in% c("Arua", "Yumbe", "Koboko", "Maracha")
    ~"Arua, Yumbe, Koboko, Maracha",
    district_name %in% c("Bugiri", "Iganga", "Mayuge", "Namutumba", "Luuka", 
                         "Namayingo")
    ~"Bugiri, Iganga, Mayuge, Namutumba, Luuka, Namayingo",
    district_name %in% c("Bundibugyo", "Ntoroko")~"Bundibugyo, Ntoroko",
    district_name %in% c("Bushenyi", "Mbarara", "Ntungamo", "Ibanda", 
                         "Isingiro", "Kiruhura", "Buhweju", "Mitooma", 
                         "Rubirizi", "Sheema")
    ~paste0("Bushenyi, Mbarara, Ntungamo, Ibanda, Isingiro, Kiruhura, ",
            "Buhweju, Mitooma, Rubirizi, Sheema"),
    district_name %in% c("Busia", "Tororo", "Butaleja")
    ~"Busia, Tororo, Butaleja",
    district_name %in% c("Gulu", "Amuru", "Nwoya", "Omoro")
    ~"Gulu, Amuru, Nwoya, Omoro",
    district_name %in% c("Kabale", "Rubanda")~"Kabale, Rubanda",
    district_name %in% c("Kabarole", "Kamwenge", "Kyenjojo", "Kyegegwa")
    ~"Kabarole, Kamwenge, Kyenjojo, Kyegegwa",
    district_name %in% c("Kamuli", "Kaliro", "Buyende")
    ~"Kamuli, Kaliro, Buyende",
    district_name %in% c("Kapchorwa", "Bukwo", "Kween")
    ~"Kapchorwa, Bukwo, Kween",
    district_name %in% c("Katakwi", "Soroti", "Kaberamaido", "Amuria", "Serere")
    ~"Katakwi, Soroti, Kaberamaido, Amuria, Serere",
    district_name %in% c("Kibaale", "Kagadi", "Kakumiro")
    ~"Kibaale, Kagadi, Kakumiro",
    district_name %in% c("Kiboga", "Kyankwanzi")~"Kiboga, Kyankwanzi",
    district_name %in% c("Kitgum", "Pader", "Agago", "Lamwo")
    ~"Kitgum, Pader, Agago, Lamwo",
    district_name %in% c("Kotido", "Moroto", "Nakapiripirit", "Abim", "Kaabong",
                         "Amudat", "Napak")~
      "Kotido, Moroto, Nakapiripirit, Abim, Kaabong, Amudat, Napak",
    district_name %in% c("Kumi", "Bukedea", "Ngora")~"Kumi, Bukedea, Ngora",
    district_name %in% c("Lira", "Amolatar", "Dokolo", "Alebtong", "Otuke")
    ~"Lira, Amolatar, Dokolo, Alebtong, Otuke",
    district_name %in% c("Luwero", "Nakasongola", "Nakaseke")
    ~"Luwero, Nakasongola, Nakaseke",
    district_name %in% c("Masaka", "Ssembabule", "Bukomansimbi", "Kalungu", "Lwengo")
    ~"Masaka, Ssembabule, Bukomansimbi, Kalungu, Lwengo",
    district_name %in% c("Masindi", "Buliisa", "Kiryandongo")
    ~"Masindi, Buliisa, Kiryandongo",
    district_name %in% c("Mbale", "Sironko", "Bududa", "Manafwa", "Bulambuli")
    ~"Mbale, Sironko, Bududa, Manafwa, Bulambuli",
    district_name %in% c("Mpigi", "Wakiso", "Butambala", "Gomba")
    ~"Mpigi, Wakiso, Butambala, Gomba",
    district_name %in% c("Mubende", "Mityana")~"Mubende, Mityana",
    district_name %in% c("Mukono", "Kayunga", "Buikwe", "Buvuma")
    ~"Mukono, Kayunga, Buikwe, Buvuma",
    district_name %in% c("Nebbi", "Zombo")~"Nebbi, Zombo",
    district_name %in% c("Pallisa", "Budaka", "Kibuku")
    ~"Pallisa, Budaka, Kibuku",
    district_name %in% c("Rakai", "Lyantonde")~"Rakai, Lyantonde",
    district_name %in% c("Rukungiri", "Kanungu")~"Rukungiri, Kanungu",
    TRUE ~ district_name)) %>%
  group_by(district_name, year) %>%
  summarise(across(c(loss, treecover), ~sum(.x)), .groups = 'drop') 

saveRDS( uga_df, paste0(uga.dir, "Forest Cover/loss_ann.rds"))
saveRDS( uga_5yr, paste0(uga.dir, "Forest Cover/loss_5yr.rds"))
saveRDS( uga_cover, paste0(uga.dir, "Forest Cover/forest_cover.rds"))
saveRDS( uga30_df, paste0(uga.dir, "Forest Cover/loss30.rds"))

#*******************************************************************************
# Zambia ----
#*******************************************************************************

zmb_df <- get_country("Zambia")
zmb30_df <- get_country30("Zambia") %>%
  mutate(
    district_name = case_when(
      district_name %in% c("Chavuma", "Zambezi") ~ "Chavuma, Zambezi",
      district_name %in% c("Chibombo", "Kabwe", "Kapiri-Mposhi", "Mkushi") 
      ~ "Chibombo, Kabwe, Kapiri Mposhi, Mkushi",
      district_name %in% c("Chienge", "Nchelenge") ~ "Chienge, Nchelenge",
      district_name %in% c("Chipata", "Mambwe") ~ "Chipata, Mambwe",
      district_name %in% c("Chongwe", "Kafue") ~ "Chongwe, Kafue",
      district_name %in% c("Ikelenge", "Mwinilunga") ~ "Ikelenge, Mwinilunga",
      district_name %in% c("Isoka", "Mafinga", "Nakonde") 
      ~ "Isoka, Mafinga, Nakonde",
      district_name %in% c("Itezhi-tezhi", "Namwala") ~ "Itezhi Tezhi, Namwala",
      district_name %in% c("Kalomo", "Kazungula") ~ "Kalomo, Kazungula",
      district_name %in% c("Kasama", "Mungwi") ~ "Kasama, Mungwi",
      district_name %in% c("Lufwanyama", "Masaiti", "Mpongwe") 
      ~ "Lufwanyama, Masaiti, Mpongwe",
      district_name %in% c("Mansa", "Milenge") ~ "Mansa, Milenge",
      district_name %in% c("Mbala", "Mpulungu") ~ "Mbala, Mpulungu",
      district_name %in% c("Nyimba", "Petauke") ~ "Nyimba, Petauke",
      district_name %in% c("Senanga", "Shang'ombo") ~ "Senanga, Shang'ombo",
      district_name == "Mufumbwe" ~ "Mufumbwe (Chizera)",
      TRUE ~ district_name)) %>%
  # aggregate across the combined districts
  group_by(district_name, region_name, year) %>%
  summarise(across(c(loss, treecover), ~sum(.x)), .groups = 'drop')

saveRDS( zmb_df, paste0(build.dir, "Zambia/Forest Cover/loss_ann.rds"))
saveRDS( zmb30_df, paste0(build.dir, "Zambia/Forest Cover/loss30.rds"))

#*******************************************************************************
# Save Stata ---- 
# datasets of annual tree loss
#*******************************************************************************

write_dta(ben_df, file.path(ben.dir, "Forest Cover/loss_ann.dta"))
write_dta(saf_df, file.path(saf.dir, "Forest Cover/loss_ann.dta"))
write_dta(tza_df, file.path(tza.dir, "Forest Cover/loss_ann.dta"))
write_dta(uga_df, file.path(uga.dir, "Forest Cover/loss_ann.dta"))
