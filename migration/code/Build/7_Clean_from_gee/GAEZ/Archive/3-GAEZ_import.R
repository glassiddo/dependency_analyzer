#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    Feb 6, 2023
#* Title:   Build GAEZ datasets for each country
#* Desc:    
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

#*******************************************************************************
# level 2 ----
#*******************************************************************************

gaez_df <- read_csv(paste0(raw.dir, "Africa/GAEZ data/GAEZADM2.CSV")) %>%
  rename(country_code = CNTRY_CODE,
         country = CNTRY_NAME,
         geolevel2 = GEOLEVEL2,
         district_name = ADMIN_NAME) %>%
  select(-c(BPL_CODE)) %>%
  pivot_longer(
    -c(country_code, geolevel2, country, district_name),
    names_to = "var",
    values_to = "val"
    ) %>%
  mutate(crop = str_sub(var, 1, 3),
         year = as.numeric(str_sub(var, 4, 7)),
         type = str_sub(var, 8, 10)) %>%
  select(-var) %>%
  pivot_wider(
    id_cols = c(country_code, geolevel2, country, district_name, year),
    names_from = c(crop, type),
    names_sep = "_",
    values_from = val
  ) %>%
  drop_na(geolevel2) %>%
  filter(district_name != "Lake Malawi") %>%
  mutate(district_name = iconv(district_name, "latin1", "ASCII", "byte") %>%
           str_replace_all(. , "<c3><a1>", "a") %>%
           str_replace_all(. , "<c3><ad>", "a") %>%
           str_replace_all(. , "<c3><a8>", "e") %>%
           str_replace_all(. , "<c3><a9>", "e") %>%
           str_replace_all(. , "<c3><89>", "e") %>%
           str_replace_all(. , "<c3><af>", "i") %>%
           str_replace_all(. , "<c3><b3>", "o") %>%
           str_replace_all(. , "<c5><93>", "e") %>%
           str_replace_all(. , "<c3><ba>", "u") %>%
           str_replace_all(. , "<c3><a7>", "c") %>%
           str_replace_all(. , "<c3><b4>", "o") %>%
           str_to_title(.)) 

saveRDS(gaez_df, paste0(build.dir, "Africa/GAEZ data/l2_GAEZ.rds"))

#*******************************************************************************
# level 1 ----
#*******************************************************************************

gaez_df <- read_csv(paste0(raw.dir, "Africa/GAEZ data/GAEZADM1.CSV")) %>%
  rename(country_code = CNTRY_CODE,
         country = CNTRY_NAME,
         geolevel1 = GEOLEVEL1,
         region_name = ADMIN_NAME) %>%
  select(-c(BPL_CODE)) %>%
  pivot_longer(
    -c(country_code, geolevel1, country, region_name),
    names_to = "var",
    values_to = "val"
  ) %>%
  mutate(crop = str_sub(var, 1, 3),
         year = as.numeric(str_sub(var, 4, 7)),
         type = str_sub(var, 8, 10)) %>%
  select(-var) %>%
  pivot_wider(
    id_cols = c(country_code, geolevel1, country, region_name, year),
    names_from = c(crop, type),
    names_sep = "_",
    values_from = val
  ) %>%
  drop_na(geolevel1) %>%
  filter(region_name %!in% c("Waterbodies", "Lake Malawi")) %>%
  mutate(region_name = iconv(region_name, "latin1", "ASCII", "byte") %>%
           str_replace_all(. , "<c3><a1>", "a") %>%
           str_replace_all(. , "<c3><ad>", "a") %>%
           str_replace_all(. , "<c3><a8>", "e") %>%
           str_replace_all(. , "<c3><a9>", "e") %>%
           str_replace_all(. , "<c3><89>", "e") %>%
           str_replace_all(. , "<c3><af>", "i") %>%
           str_replace_all(. , "<c3><b3>", "o") %>%
           str_replace_all(. , "<c5><93>", "e") %>%
           str_replace_all(. , "<c3><ba>", "u") %>%
           str_replace_all(. , "<c3><a7>", "c") %>%
           str_replace_all(. , "<c3><b4>", "o") %>%
           str_to_title(.)) 

saveRDS(gaez_df, paste0(build.dir, "Africa/GAEZ data/l1_GAEZ.rds"))

