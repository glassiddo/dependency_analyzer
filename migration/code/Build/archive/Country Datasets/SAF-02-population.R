# Project: Migration Africa
# Author:  Sam Marshall
# Created: Feb 28, 2022
# Title:   South Africa Census Populations
# Output:  population.rds
# NB: The forest data needs to be adjusted to be at the municipality level...
###############################################################################

source("code/SSA_env_SetUp.R")
source("code/Functions/F-SAF.R")
library(stringi)

###############################################################################
##### 1. District
###############################################################################

pop_df <- read_excel(paste0(saf_raw.dir, "Census_populations.xlsx"),
                     range = "A1:F244") %>%
  filter(Status != "Province") %>%
  mutate(Name = stri_trans_general(str = Name, id = "Latin-ASCII"),
         paren_pos = StrPos(Name, "\\(", pos = 1),
         Name = ifelse(is.na(paren_pos) == FALSE, substr(Name, 1, paren_pos - 2), Name),
         Name = ifelse(Name == "!Kheis", "Kheis", Name),
         Name = ifelse(Name == "//Khara Hais", "Khara Hais", Name),
         Name = ifelse(Name == "Lepelle-Nkumpi", "Lepele-Nkumpi", Name),
         Name = ifelse(Name == "Mookgophong", "Mookgopong", Name),
         Name = ifelse(Name == "Nquthu", "Nqutu", Name),
         Name = ifelse(Name == "Pixley ka Seme", "Pixley Ka Seme", Name),
         Name = ifelse(Name == "Sol Plaatje", "Sol Plaatjie", Name),
         Name = ifelse(Name == "Sunday's River Valley", "Sundays River Valley", Name),
         Name = ifelse(Name == "The Big Five False Bay", "The Big 5 False Bay", Name),
         Name = ifelse(Name == "uMuziwabantu", "UMuziwabantu", Name),
         Name = ifelse(Name == "uPhongolo", "UPhongolo", Name)) %>%
  select(-paren_pos) 

names(pop_df) <- c("district_name", "type", "pop_96", "pop_01","pop_11", "pop_16")

pop_df %<>%
  mutate(district_name = ifelse(district_name == "Emalahleni" & pop_96 == 130670, 
                                "Emalahleni-EC", district_name),
         district_name = ifelse(district_name == "Emalahleni", "Emalahleni-MP",
                                district_name),
         district_name = ifelse(district_name == "Naledi" & pop_96 == 54116, 
                                "Naledi-NW", district_name),
         district_name = ifelse(district_name == "Naledi", "Naledi-FS", 
                                district_name)) %>%
  select(-type)

sa_2016 <- read_dta( paste0(saf.dir, "Census/Census_2016.dta" )) %>%
  remove_all_labels() %>%
  mutate(N = 1) %>%
  group_by(muni_name) %>%
  summarise(urban_wgt = sum(urban * p_wgt),
            urban = sum(urban),
            pop_wgt = sum(p_wgt),
            pop = sum(N), .groups = 'drop') %>%
  mutate(urban_wgt = round(urban_wgt / pop_wgt),
         urban = round(urban / pop)) %>%
  rename(district_name = muni_name, urban_unweight = urban, urban = urban_wgt) %>%
  select(-pop, -pop_wgt)

pop_df <- full_join(pop_df, sa_2016)

saveRDS( pop_df, paste0(saf.dir, "Support/population.rds"))

###############################################################################
##### 2. District crosswalk
###############################################################################

sa_1996 <- read_dta( paste0(saf.dir, "Census/Census_1996.dta" )) %>%
  remove_all_labels() %>%
  select(ends_with("name")) %>% 
  select(-starts_with("prev")) %>%
  unique()

sa_2001 <- read_dta( paste0(saf.dir, "Census/Census_2001.dta" )) %>%
  remove_all_labels() %>%
  select(ends_with("name")) %>% 
  select(-starts_with("prev")) %>%
  unique()

sa_2011 <- read_dta( paste0(saf.dir, "Census/Census_2011.dta" )) %>%
  remove_all_labels() %>%
  select(ends_with("name")) %>% 
  select(-starts_with("prev")) %>%
  unique()

sa_2011_ren <- sa_2011 %>%
  select(-district_name) %>%
  rename(district_name = muni_name) %>%
  adj_muni( 2011) %>%
  mutate(yr11 = 2011,
         dot_pos = StrPos(province_name, ". "),
         dot_pos = ifelse(is.na(dot_pos) == TRUE, 9, dot_pos),
         province_name = ifelse(dot_pos == 2, substring(province_name, 4), 
                                province_name)) %>%
  select(-dot_pos) 

sa_2001_ren <- sa_2001 %>%
  select(muni_name) %>%
  rename(district_name = muni_name) %>%
  adj_muni( 2001) %>%
  unique() %>%
  mutate(yr01 = 2001) 

xwalk_df <- full_join(sa_2011_ren, sa_2001_ren) %>%
  arrange(district_name) %>%
  mutate(
    province_name = ifelse(district_name %in% c("Aberdeen Plain"),
                           "Eastern Cape", province_name),
    province_name = ifelse(district_name %in% c("Benede Oranje", "Diamondfields",
                                                "Kalahari", "Namaqualand"),
                           "Northern Cape", province_name),
    province_name = ifelse(district_name %in% c("Breede River", "Central Karoo",
                                                "South Cape", "West Coast"),
                           "Western Cape", province_name),
    province_name = ifelse(district_name %in% c("Gaints Castle Game Reserve",
                                                "Mkhomazi Wilderness Area",
                                                "St Lucia Park"),
                           "KwaZulu-Natal", province_name),
    province_name = ifelse(district_name %in% c("Kruger Park"),
                           "Limpopo", province_name),
    province_name = ifelse(district_name %in% c("Lowveld"),
                           "Mpumalanga", province_name),
    province_name = str_to_title(province_name)) %>%
  arrange(province_name, district_name) %>%
  mutate(district = row_number())

# get region codes
region_df <- xwalk_df %>% select(province_name) %>% unique() %>%
  arrange(province_name) %>%
  mutate(region = row_number())

xwalk_df <- full_join(xwalk_df, region_df) %>%
  rename(region_name = province_name) %>%
  select(-yr11, -yr01)

saveRDS( xwalk_df, paste0(saf.dir, "Support/muni_xwalk.rds"))

# TODO later
# magesterial districts
# sa_1996_md <- sa_1996 %>% rename(md_name = muni_name) %>% adj_md() %>%
#   select(md_name) %>% mutate(yr96 = 1996)
# sa_2001_md <- sa_2001 %>% select(magesterial_district_name) %>% 
#   rename(md_name = magesterial_district_name) %>% unique() %>% 
#   mutate(yr01 = 2001)
# 
# sa_1996_md <- sa_1996 %>% rename(md_name = muni_name) %>% adj_md() %>%
#   select(-district_name) %>% mutate(yr96 = 1996)
# 
# sa_2001_md <- sa_2001 %>% select(-district_name) %>% 
#   rename(md_name = magesterial_district_name) %>% unique() %>% 
#   mutate(yr01 = 2001)
# 
# md_xwalk <- full_join(sa_1996_md, sa_2001_md) %>%
#   arrange(md_name)
# 
# temp_df <- sa_1996 %>% arrange(muni_name) 

  



