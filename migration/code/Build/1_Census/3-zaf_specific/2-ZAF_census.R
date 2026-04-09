#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    South Africa Census
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

ctries_dir <- "Countries"
ctry_str <- "ZAF"

geo_labels <- function(dat_df, geo, name, fmt = "title") {
  lab_df <- tibble(d = get_labels(dat_df[[{{geo}}]], values = "p")) %>%
    mutate(v_st = StrPos(d, "\\[", 1),
           v_end = StrPos(d, "\\]", 1),
           {{geo}} := as.numeric(substr(d, v_st + 1, v_end - 1)),
           {{name}} := substring(d, v_end + 2) ) %>%
    mutate(
      across(ends_with("name"), ~str_to_title(.x)),
      across(ends_with("name"), ~gsub("ã©", "e", .x)),
      across(ends_with("name"), ~gsub("ã¨", "e", .x)),
      across(ends_with("name"), ~gsub("ã¯", "i", .x)),
      across(ends_with("name"), ~gsub("Ã¯", "i", .x)),
      across(ends_with("name"), ~gsub("ã³", "o", .x)),
      across(ends_with("name"), ~gsub("ãº", "u", .x)),
      across(ends_with("name"), ~gsub("ã¡", "a", .x)),
      across(ends_with("name"), ~gsub("ã´", "o", .x)),
      across(ends_with("name"), ~gsub("ãç", "c", .x)),
      across(ends_with("name"), ~gsub("ã­", "a", .x)),
      across(ends_with("name"), ~gsub("å", "e", .x)),
      across(ends_with("name"), ~gsub("ãÂ", "e", .x)),
      across(ends_with("name"), ~gsub("Ã", "e", .x, ignore.case = TRUE)),
      across(ends_with("name"), ~str_to_title(.x)),
      across(ends_with("name"), ~ifelse(.x == "Foreign Country", 
                                        "Abroad", .x))
    ) %>%
    select(-d, -v_st, -v_end) 
  # 
  # formatting options of variable name
  if (fmt == "title") {
    lab_df %>% mutate({{name}} := str_to_title({{name}}))
  }
}


#****************************************************************************
# 2001 ----
xwalk01_df <- readRDS(here(build.dir, ctries_dir, ctry_str, "Support", "xwalk01_curr.rds"))
pl_xwalk01_df <- readRDS(here(build.dir, ctries_dir, ctry_str, "Support", "xwalk01_prev.rds")) %>%
  select(-c(dc_munic, munic_co, geolevel1, geolevel2)) %>%
  rename(prev_district = admin_id_l2, prev_district_name = admin_name_l2,
         prev_region = admin_id_l1, prev_region_name = admin_name_l1)

saf01_df <- read_dta(here(
  raw.dir, 
  ctries_dir, ctry_str, "census 2001", "SA Census 2001 Person_v1.1_20111024.dta") 
) %>%
  select(munic_co, dc_munic, pr_code, age = p02_age, starts_with("p12"), 
         wgt = weight, DENSITY, p19b_ind, der10_em, prev_country_code = p09b_cnt) %>%
  mutate(migration_year = p12b_96y + 1995,
         migration_year = ifelse(migration_year == 2004, NA, migration_year),
         urban = ifelse(DENSITY == 2, 0, DENSITY),
         empl_ag = ifelse(p19b_ind %in% c(111:114), 1, 0),
         empl_mining = ifelse(p19b_ind %in% c(210:290), 1, 0),
         employed = ifelse(der10_em == 1, 1, 0),
         empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                            missing = 0),
         # sectors
         empl_mfg = ifelse(p19b_ind %in% c(115:412), 1, 0),
         empl_svc = ifelse(p19b_ind %in% c(10:30,413:990), 1, 0),
         empl_trd = ifelse(p19b_ind %in% c(115:412,611:619), 1, 0),
         empl_ntrd = ifelse(p19b_ind %in% c(10:30,413:505,621:990), 1, 0),
         #make previous district
         prev_dist = ifelse(p12a_ppr == 9, NA, p12a_ppr) %>%
           as.character() %>%
           substr(., 1, 3) %>%
           # remove sci format
           ifelse(substr(.,2,3) == "e+",substr(.,1,1), .) %>%
           ifelse(as.numeric(substr(.,2,3)) %in% c(76,81:88),substr(.,2,3), .) %>%
           #Overberg, nature parks are unmatched
           ifelse(. == 193, 113, .)) %>%
  select(age, wgt, migration_year, dc_munic, pr_code, prev_dist, 
         prev_country_code, urban, starts_with("empl")) %>%
  full_join(xwalk01_df) %>%
  rename(district = admin_id_l2, district_name = admin_name_l2,
         region = admin_id_l1, region_name = admin_name_l1,
         geo1_sf = geolevel1, geo2_sf = geolevel2) %>%
  left_join(pl_xwalk01_df) %>%
  select(-prev_dist)

urban_df <- saf01_df %>% 
  group_by(district) %>%
  summarise(urban_geo = weighted.mean(urban, w = wgt, na.rm = TRUE), 
            .groups = 'drop') %>%
  mutate(urban_geo = round(urban_geo))

pc <- geo_labels(saf01_df, "prev_country_code", prev_country_name)

saf01_df %<>% full_join(urban_df) %>%
  left_join(pc) %>%
  mutate(year = 2001)

saveRDS(saf01_df, here(build.dir, ctries_dir, ctry_str, "Census", "census01.rds"))

rm(saf01_df, urban_df, xwalk01_df, pl_xwalk01_df, pc)

#****************************************************************************
# 2011 ----
xwalk11_df <- readRDS(here(build.dir, ctries_dir, ctry_str, "Support", "xwalk11_curr.rds"))
pl_xwalk11_df <- readRDS(here(build.dir, ctries_dir, ctry_str, "Support", "xwalk11_prev.rds")) %>%
  select(-c(p_province, p_munic, p_district, geolevel1, geolevel2)) %>%
  rename(prev_district = admin_id_l2, prev_district_name = admin_name_l2,
         prev_region = admin_id_l1, prev_region_name = admin_name_l1,
         prev_mn_code = p11c_prevresmunic) %>%
  drop_na(prev_mn_code)

saf11a_df <- read_dta(here(
  raw.dir, ctries_dir, ctry_str,
  "census 2011", "sa-census-2011-person-prov-1to5-v1.2-20150825.dta") 
) %>%
  rename_with(~str_to_lower(.x)) %>%
  select(starts_with("p_"), starts_with("p11"), 
         p29a_industry, derp_employ_status,
         age = f02_age, wgt = person_10per_wgt, 
         migration_year = p11a_resyearmoved, prev_mn_code = p11c_prevresmunic,
         prev_country_code = p08_countryofbirth) %>%
  mutate(urban = ifelse(p_geotype %in% c(2,3), 0, p_geotype),
         empl_ag = ifelse(p29a_industry %in% c(110:114), 1, 0),
         empl_mining = ifelse(p29a_industry %in% c(210:290), 1, 0),
         derp_employ_status = ifelse(is.na(derp_employ_status) == TRUE, 0, 
                                     derp_employ_status),
         employed = ifelse(derp_employ_status == 1, 1, 0),
         empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                            missing = 0),
         # sectors
         empl_mfg = ifelse(p29a_industry %in% c(115:412), 1, 0),
         empl_svc = ifelse(p29a_industry %in% c(10:30,413:990), 1, 0),
         empl_trd = ifelse(p29a_industry %in% c(115:412,611:619), 1, 0),
         empl_ntrd = ifelse(p29a_industry %in% c(10:30,413:505,621:990), 1, 0),) %>%
  #rename(age = f02_age, wgt = person_10per_wgt, 
  #       migration_year = p11a_resyearmoved, prev_mn_code = p11c_prevresmunic) %>%
  select(-c(starts_with("p11"), p_geotype, p29a_industry, derp_employ_status)) %>%
  left_join(xwalk11_df) %>%
  rename(district = admin_id_l2, district_name = admin_name_l2,
         region = admin_id_l1, region_name = admin_name_l1,
         geo1_sf = geolevel1, geo2_sf = geolevel2) %>%
  left_join(pl_xwalk11_df) %>%
  select(-prev_mn_code)


saf11b_df <- read_dta(here(
  raw.dir, 
  ctries_dir, ctry_str,
  "census 2011", "sa-census-2011-person-prov-6to9-v1.2-20150825.dta") 
) %>%
  rename_with(~str_to_lower(.x)) %>%
  select(starts_with("p_"), starts_with("p11"), p29a_industry, 
         derp_employ_status, age = f02_age, wgt = person_10per_wgt, 
         migration_year = p11a_resyearmoved, prev_mn_code = p11c_prevresmunic,
         prev_country_code = p08_countryofbirth) %>%
  mutate(urban = ifelse(p_geotype %in% c(2,3), 0, p_geotype),
         empl_ag = ifelse(p29a_industry %in% c(110:119), 1, 0),
         empl_mining = ifelse(p29a_industry %in% c(210:290), 1, 0),
         derp_employ_status = ifelse(is.na(derp_employ_status) == TRUE, 0, 
                                     derp_employ_status),
         employed = ifelse(derp_employ_status == 1, 1, 0),
         empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                            missing = 0),
         # sectors
         empl_mfg = ifelse(p29a_industry %in% c(115:412), 1, 0),
         empl_svc = ifelse(p29a_industry %in% c(10:30,413:990), 1, 0),
         empl_trd = ifelse(p29a_industry %in% c(115:412,611:619), 1, 0),
         empl_ntrd = ifelse(p29a_industry %in% c(10:30,413:505,621:990), 1, 0),) %>%
  #rename(age = f02_age, wgt = person_10per_wgt, 
  #       migration_year = p11a_resyearmoved, prev_mn_code = p11c_prevresmunic) %>%
  select(-c(starts_with("p11"), p_geotype, p29a_industry, derp_employ_status)) %>%
  left_join(xwalk11_df) %>%
  rename(district = admin_id_l2, district_name = admin_name_l2,
         region = admin_id_l1, region_name = admin_name_l1,
         geo1_sf = geolevel1, geo2_sf = geolevel2) %>%
  left_join(pl_xwalk11_df) %>%
  select(-prev_mn_code)

saf11_df <- bind_rows(saf11a_df, saf11b_df) 

pc <- geo_labels(saf11_df, "prev_country_code", prev_country_name)

urban_df <- saf11_df %>% 
  group_by(district) %>%
  summarise(urban_geo = weighted.mean(urban, w = wgt, na.rm = TRUE), 
            .groups = 'drop') %>%
  mutate(urban_geo = round(urban_geo))

saf11_df %<>% full_join(urban_df) %>%
  left_join(pc) %>%
  mutate(year = 2011)

saveRDS(saf11_df, here(build.dir, ctries_dir, ctry_str, "Census", "census11.rds"))

rm(saf11_df, saf11a_df, saf11b_df, pl_xwalk11_df, pc)

