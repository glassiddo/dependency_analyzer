#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    Apr 3, 2023
#* Title:   South Africa District xwalk
#* Output:  
#****************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

ctries_dir <- "Countries"
ctry_str <- "ZAF"

#****************************************************************************

# id ----
# create ids from ipums census data
id_df <- readRDS(here(build.dir, ctries_dir, ctry_str, "Census", "census11-1.rds")) %>%
  select(starts_with(c("district", "region", "geo"))) %>%
  select(-ends_with(c("0","1","2","3","4","5","6","7","8","9"))) %>%
  distinct() %>%
  rename_with(~"geolevel2", starts_with("geo2") ) %>%
  rename_with(~"geolevel1", starts_with("geo1") ) %>%
  rename(admin_name_l2 = district_name, admin_id_l2 = district,
         admin_name_l1 = region_name, admin_id_l1 = region)

#****************************************************************************
# 2001 ----
# get south africa 2001 district codes
saf01_df <- read_dta(here(
  raw.dir, 
  ctries_dir, ctry_str,
  "census 2001", "SA Census 2001 Person_v1.1_20111024.dta"))

l201_df <- saf01_df %>% 
  select(dc_munic, pr_code) %>%
  distinct() %>%
  mutate(
    admin_id_l2 = case_when(
      dc_munic %in% c(16:20) ~ 11,
      dc_munic == 774 ~ 21,
      dc_munic %in% c(773,76,32,37,31,34,35,83,33,36,82,84) ~ 22,
      dc_munic == 30 ~ 23,
      dc_munic == 6 ~ 24,
      dc_munic %in% c(42,88,9,8,38,40,39,81,7) ~ 25,
      dc_munic == 10 ~ 31,
      dc_munic %in% c(572,15,22,12,26,44,13,21,24,43,14) ~ 32,
      dc_munic == 29 ~ 33,
      dc_munic == 275 ~ 34,
      dc_munic == 27 ~ 35,
      dc_munic %in% c(23,25) ~ 36,
      dc_munic == 28 ~ 37,
      dc_munic %in% c(2,3) ~ 41,
      dc_munic == 5 ~ 42,
      dc_munic == 171 ~ 43,
      dc_munic == 4 ~ 44,
      dc_munic == 1 ~ 45,
      
    )
  )

# current location xwalk
xwalk01_df <- full_join(l201_df, id_df)

# match the prev loc codes with current district codes
prcode <- saf01_df %>%
  filter(p12a_ppr != 9) %>%
  mutate(prev_dist = as.character(p12a_ppr),
         prev_dist = substr(prev_dist, 1, 3),
         # remove sci format
         prev_dist = ifelse(substr(prev_dist,2,3) == "e+",
                            substr(prev_dist,1,1), prev_dist),
         prev_dist = ifelse(as.numeric(substr(prev_dist,2,3)) %in% c(76,81:88),
                            substr(prev_dist,2,3), prev_dist),
         #Overberg, nature parks are unmatched
         prev_dist = ifelse(prev_dist == 193, 113, prev_dist)) %>%
  select(prev_dist) %>%
  distinct() 

# district to muni
pl_xwalk01_df <- saf01_df %>% 
  select(dc_munic, munic_co) %>%
  distinct() %>%
  mutate(prev_dist = as.character(munic_co)) %>%
  full_join(prcode) %>%
  left_join(xwalk01_df) %>%
  select(-pr_code) %>%
  distinct()

saveRDS(xwalk01_df, here(build.dir, ctries_dir, ctry_str, "Support", "xwalk01_curr.rds"))
saveRDS(pl_xwalk01_df, here(build.dir, ctries_dir, ctry_str, "Support", "xwalk01_prev.rds"))

rm(saf01_df, xwalk01_df, pl_xwalk01_df, prcode, l201_df)

#****************************************************************************
# 2011 ----
saf11a_df <- read_dta(here(
  raw.dir, ctries_dir, ctry_str,
  "census 2011", "sa-census-2011-person-prov-1to5-v1.2-20150825.dta") 
) %>%
  rename_with(~str_to_lower(.x))

saf11b_df <-  read_dta(here(
  raw.dir, ctries_dir, ctry_str,
  "census 2011", "sa-census-2011-person-prov-6to9-v1.2-20150825.dta") 
) %>%
  rename_with(~str_to_lower(.x))

l211_df <- bind_rows(
  saf11a_df %>% select(p_province, p_district) %>% distinct(),
  saf11b_df %>% select(p_province, p_district) %>% distinct() ) %>%
  mutate(
    admin_id_l2 = case_when(
      p_district %in% c(416:420,499) ~ 11,
      p_district == 798 ~ 21,
      p_district %in% c(797,799,832,637,831,934,935,947,933,936) ~ 22,
      p_district == 830 ~ 23,
      p_district == 306 ~ 24,
      p_district %in% c(307,308,309,345,742,638,748,640,639) ~ 25,
      p_district == 210 ~ 31,
      p_district %in% c(599,215,522,212,556,244,213,260,521,554,543,214) ~ 32,
      p_district == 559 ~ 33,
      p_district == 299 ~ 34,
      p_district == 527 ~ 35,
      p_district %in% c(523,555) ~ 36,
      p_district == 528 ~ 37,
      p_district %in% c(102,103) ~ 41,
      p_district == 105 ~ 42,
      p_district == 199 ~ 43,
      p_district == 104 ~ 44,
      p_district == 101 ~ 45,
    )
  )

# current location xwalk
xwalk11_df <- full_join(l211_df, id_df)

prcode <- bind_rows(
  saf11a_df %>% select(p11c_prevresmunic) %>% 
    filter(is.na(p11c_prevresmunic) == FALSE),
  saf11b_df %>% select(p11c_prevresmunic) %>% 
    filter(is.na(p11c_prevresmunic) == FALSE)) %>%
  distinct() %>%
  mutate(p_munic = p11c_prevresmunic)

pl_xwalk11_df <- bind_rows(
  saf11a_df %>% select(p_province, p_munic, p_district) %>% distinct(),
  saf11b_df %>% select(p_province, p_munic, p_district) %>% distinct() ) %>%
  # no one from 976 + 999
  full_join(prcode) %>%
  left_join(xwalk11_df)

saveRDS(xwalk11_df, here(build.dir, ctries_dir, ctry_str, "Support", "xwalk11_curr.rds"))
saveRDS(pl_xwalk11_df, here(build.dir, ctries_dir, ctry_str, "Support", "xwalk11_prev.rds"))  
  