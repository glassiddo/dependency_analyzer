#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    Jul 11, 2022
#* Title:   Read and clean SAF census
#* Output:  
#* NB: create: 
#* 1. location - municipality, district, province
#* 2. prev location
#* 3. birth location
#* 4. age
#* 5. duration
#* 6. weight
#* 7. urban
#****************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")
source("code/Functions/F-SAF.R")

#****************************************************************************
# Muni xwalk ----
xwalk01 <- read_xls(
  paste0(raw.dir, "South Africa/census 2001/meta/Geography Codes/", 
         "SA Census 2001 Main Place Codes.xls")) %>%
  rename_with(~str_to_lower(.x)) %>%
  select(starts_with("mn"), starts_with("pr")) %>%
  unique() %>%
  mutate(mn_name01 = mn_name,
         mn_name = gsub(" Metro", "", mn_name),
         mn_name = case_when(
           mn_name == "||Khara Hais" ~"Khara Hais",
           mn_name == "Ethekwini" ~"eThekwini",
           mn_name == "Nelson Mandela" ~"Nelson Mandela Bay",
           mn_name == "Mooi Mpofana" ~"Mpofana",
           mn_name == "Msunduzi" ~"The Msunduzi",
           mn_name %in% c("Kagisano","Molopo") ~"Kagisano/Molopo",
           mn_name == "Sol Plaatje" ~"Sol Plaatjie",
           mn_name == "Sunday's River Valley" ~"Sundays River Valley",
           mn_name == "Umsombomvu" ~"Umsobomvu",
           mn_name == "uMuziwabantu" ~"UMuziwabantu",
           mn_name == "uPhongolo" ~"UPhongolo",
           mn_name == "Emalahleni" & pr_name == "EASTERN CAPE" ~"Emalahleni-EC",
           mn_name == "Emalahleni" & pr_name == "MPUMALANGA" ~"Emalahleni-MP",
           mn_name == "Naledi" & pr_name == "FREE STATE" ~"Naledi-FS",
           mn_name == "Naledi" & pr_name == "NORTH WEST" ~"Naledi-NW",
           # places that changed names
           mn_name == "Plettenberg Bay" ~"Bitou",
           mn_name == "City Council of Klerksdorp" ~"City of Matlosana",
           mn_name == "Greater Groblersdal" ~"Elias Motsoaledi",
           mn_name == "Utrecht" ~"Emadlangeni",
           mn_name == "Highlands" ~"Emakhazeni",
           mn_name == "Greater Marble Hall" ~"Ephraim Mogale", 
           mn_name == "Highveld East" ~"Govan Mbeki", 
           mn_name == "Moshaweng" ~"Joe Morolong", 
           mn_name == "eNdondakusuka" ~"Mandeni", 
           mn_name == "Mbonambi" ~"Mfolozi", 
           mn_name == "Qaukeni" ~"Ngquza Hill", 
           mn_name == "Zeerust" ~"Ramotshere Moiloa",
           mn_name == "Setla-Kgobi" ~"Ratlou", 
           mn_name == "Middelburg" ~"Steve Tshwete",
           mn_name == "Potchefstroom" ~"Tlokwe City Council", 
           mn_name == "Delmas" ~"Victor Khanye", 
           mn_name %in% c("Bo Karoo", "Seme") ~"Pixley Ka Seme", 
           mn_name == "Kungwini" ~"City of Tshwane", 
           mn_name == "Langeberg" ~"Hessequa", 
           mn_name == "Breede River/Winelands" ~"Langeberg",
           mn_name == "West Rand" ~"Randfontein", 
           mn_name == "Nokeng tsa Taemane" ~"City of Tshwane", 
           TRUE ~mn_name)) %>%
  rename(mn_code01 = mn_code) %>%
  select(-starts_with("pr")) %>%
  unique()

xwalk11 <- read_xlsx(
  paste0(raw.dir, "South Africa/census 2011/", 
         "census-2011-geography-datafirst.xlsx"),
  sheet = "Geographical_Hierarchies") %>%
  rename_with(~str_to_lower(.x)) %>%
  select(mn_name, mn_code, pr_name, pr_code) %>%
  unique() %>%
  mutate(mn_name11 = mn_name,
         mn_name = case_when(
           mn_name == "//Khara Hais" ~"Khara Hais",
           mn_name == "Emalahleni" & pr_name == "Eastern Cape" ~"Emalahleni-EC",
           mn_name == "Emalahleni" & pr_name == "Mpumalanga" ~"Emalahleni-MP",
           mn_name == "Naledi" & pr_name == "Free State" ~"Naledi-FS",
           mn_name == "Naledi" & pr_name == "North West" ~"Naledi-NW",
           TRUE ~mn_name
         )) %>%
  rename(mn_code11 = mn_code)

muni_xwalk <- full_join(xwalk01, xwalk11) %>%
  mutate(mn_code = ifelse(is.na(mn_code11) == FALSE, mn_code11, 999)) %>%
  rename(district_name = mn_name,
         district = mn_code,
         region_name = pr_name,
         region = pr_code)

# make life easier by making a second one for prev
p_muni_xwalk <- muni_xwalk %>% rename_with(~paste0("prev_", .x))

rm(xwalk01, xwalk11)

#****************************************************************************
# 2001 ----
mp_xwalk <- read_xls(
  paste0(raw.dir, "South Africa/census 2001/meta/Geography Codes/", 
         "SA Census 2001 Main Place Codes.xls")) %>%
  rename_with(~str_to_lower(.x)) %>%
  select(-starts_with("md"))

saf01_df <- read_dta(paste0(
  raw.dir, 
  "South Africa/census 2001/SA Census 2001 Person_v1.1_20111024.dta") 
  ) %>%
  select(munic_co, dc_munic, pr_code, p02_age, starts_with("p12"), weight, 
         DENSITY, p19b_ind, der10_em) %>%
  mutate(migration_year = p12b_96y + 1995,
         migration_year = ifelse(migration_year == 2004, NA, migration_year),
         urban = ifelse(DENSITY == 2, 0, DENSITY),
         empl_ag = ifelse(p19b_ind %in% c(111:119), 1, 0),
         empl_mining = ifelse(p19b_ind %in% c(210:290), 1, 0),
         employed = ifelse(der10_em == 1, 1, 0),
         empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                            missing = 0)) %>%
  rename(age = p02_age, wgt = weight, prev_mp_code = p12a_ppr,
         mn_code = munic_co)

# get muni from main place
pmp <- saf01_df %>% select(prev_mp_code) %>% unique() %>%
  rename(mp_code = prev_mp_code) %>%
  inner_join(mp_xwalk) %>%
  rename_with(~paste0("prev_", .x)) 

# get muni names from munic_co
mc <- saf01_df %>% select(mn_code) %>% unique() %>%
  left_join(mp_xwalk) %>%
  select(-starts_with("mp")) %>%
  unique() 

saf01_df %<>%
  full_join(mc) %>%
  left_join(pmp) %>%
  select(-starts_with("prev_mp"), -starts_with("p12")) %>%
  mutate(
    prev_mn_code = ifelse(is.na(migration_year) == TRUE, mn_code, prev_mn_code),
    prev_mn_name = ifelse(is.na(migration_year) == TRUE, mn_name, prev_mn_name),
    prev_pr_code = ifelse(is.na(migration_year) == TRUE, pr_code, prev_pr_code),
    prev_pr_name = ifelse(is.na(migration_year) == TRUE, pr_name, prev_pr_name),
    ) %>%
  # add in consistent codes for xwalk
  select(age, wgt, migration_year, ends_with("name"), ends_with("code"), 
         urban, starts_with("empl")) %>%
  rename_with(~paste0(.x, "01"), c(ends_with("name"), ends_with("code"))) %>%
  left_join(muni_xwalk) %>%
  left_join(p_muni_xwalk) %>%
  select(-ends_with("11"), -ends_with("01")) %>%
  mutate(year = 2001)


rm(mc, pmp, mp_xwalk)
#****************************************************************************
# 2011 ----
mp_xwalk <- read_xlsx(
  paste0(raw.dir, "South Africa/census 2011/", 
         "census-2011-geography-datafirst.xlsx"),
  sheet = "Geographical_Hierarchies") %>%
  rename_with(~str_to_lower(.x)) %>%
  select(starts_with("mn"), starts_with("dc"), starts_with("pr")) %>%
  # select(-ends_with("_c"), -ends_with("_st")) %>%
  unique()

saf11a_df <- read_dta(paste0(
  raw.dir, 
  "South Africa/census 2011/sa-census-2011-person-prov-1to5-v1.2-20150825.dta") 
) %>%
  rename_with(~str_to_lower(.x)) %>%
  select(starts_with("p_"), f02_age, starts_with("p11"), person_10per_wgt, 
         p29a_industry, derp_employ_status) %>%
  mutate(urban = ifelse(p_geotype %in% c(2,3), 0, p_geotype),
         empl_ag = ifelse(p29a_industry %in% c(110:119), 1, 0),
         empl_mining = ifelse(p29a_industry %in% c(210:290), 1, 0),
         employed = ifelse(derp_employ_status == 1, 1, 0),
         empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                            missing = 0)) %>%
  rename(age = f02_age, wgt = person_10per_wgt, 
         migration_year = p11a_resyearmoved,
         pr_code = p_province, mn_code = p_munic, dc_code = p_district,
         prev_mn_code = p11c_prevresmunic, prev_pr_code = p11b_prevresprov) %>%
  select(-starts_with("p11"), -p_geotype, -p29a_industry, -derp_employ_status)

saf11b_df <- read_dta(paste0(
  raw.dir, 
  "South Africa/census 2011/sa-census-2011-person-prov-6to9-v1.2-20150825.dta") 
) %>%
  rename_with(~str_to_lower(.x)) %>%
  select(starts_with("p_"), f02_age, starts_with("p11"), person_10per_wgt,
         p29a_industry, derp_employ_status) %>%
  mutate(urban = ifelse(p_geotype %in% c(2,3), 0, p_geotype),
         empl_ag = ifelse(p29a_industry %in% c(110:119), 1, 0),
         empl_mining = ifelse(p29a_industry %in% c(210:290), 1, 0),
         employed = ifelse(derp_employ_status == 1, 1, 0),
         empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                            missing = 0)) %>%
  rename(age = f02_age, wgt = person_10per_wgt, 
         migration_year = p11a_resyearmoved,
         pr_code = p_province, mn_code = p_munic, dc_code = p_district,
         prev_mn_code = p11c_prevresmunic, prev_pr_code = p11b_prevresprov) %>%
  select(-starts_with("p11"), -p_geotype, -p29a_industry, -derp_employ_status)

# prev muni
pmc <- bind_rows(
  saf11a_df %>% select(prev_mn_code) %>% unique(),
  saf11b_df %>% select(prev_mn_code) %>% unique()
  ) %>%
  unique() %>%
  drop_na() %>%
  filter(prev_mn_code != 999) %>%
  rename(mn_code = prev_mn_code) %>%
  left_join(mp_xwalk) %>%
  rename_with(~paste0("prev_", .x)) 

# get muni names from munic_co
mc <- bind_rows(
  saf11a_df %>% select(mn_code) %>% unique(),
  saf11b_df %>% select(mn_code) %>% unique()
) %>%
  left_join(mp_xwalk) 


saf11_df <- bind_rows(saf11a_df, saf11b_df) %>%
  full_join(mc) %>%
  left_join(pmc) %>%
  mutate(
    prev_mn_code = ifelse(is.na(migration_year) == TRUE, mn_code, prev_mn_code),
    prev_mn_name = ifelse(is.na(migration_year) == TRUE, mn_name, prev_mn_name),
    prev_pr_code = ifelse(is.na(migration_year) == TRUE, pr_code, prev_pr_code),
    prev_pr_name = ifelse(is.na(migration_year) == TRUE, pr_name, prev_pr_name),
  )

# add in consistent codes for xwalk
muni_xwalk %<>% select(-ends_with("01")) %>%
  drop_na() %>%
  unique() 
p_muni_xwalk %<>% select(-ends_with("01")) %>%
  drop_na() %>%
  unique() 

saf11_df %<>%
  select(-ends_with("_c"), -ends_with("_st")) %>%
  rename_with(~paste0(.x, "11"), c(ends_with("name"), ends_with("code"))) %>%
  left_join(muni_xwalk) %>%
  left_join(p_muni_xwalk) %>%
  rename(dc_code = dc_code11, dc_name = dc_name11) %>%
  select(-ends_with("11"), -ends_with("01")) %>%
  mutate(year = 2011)

rm(mc, pmc, mp_xwalk, saf11a_df, saf11b_df)
#****************************************************************************
# urban ----
#* define urban status for each district (municipality)
#* take the mean in each wave first, then average across waves

urban_df <- bind_rows(
  saf01_df %>% select(district, district_name, urban, year),
  saf11_df %>% select(district, district_name, urban, year)) %>% 
  group_by(district, district_name, year) %>%
  summarise(urban_geo = round(mean(urban)), .groups = 'drop') 
  
saf11_df <- left_join(saf11_df, urban_df %>% filter(year == 2011))

saf01_df <- left_join(saf01_df, urban_df %>% filter(year == 2001))

saveRDS(saf01_df, paste0(build.dir, "South Africa/Census/census01-3.rds"))
saveRDS(saf11_df, paste0(build.dir, "South Africa/Census/census11-3.rds"))
