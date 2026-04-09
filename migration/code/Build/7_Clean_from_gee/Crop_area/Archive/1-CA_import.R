#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    Feb 2, 2023
#* Title:   Build crop area datasets for each country
#* Desc:    
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

#*******************************************************************************
# level 2 ----
#*******************************************************************************

ca_wide_df <- read_csv(paste0(raw.dir, "Africa/Crop Area/AFRADM2CROPLAND.CSV")) %>%
  rename(country_code = codeadm0,
         country = nameadm0,
         geolevel2 = codeadm2,
         district_name = nameadm2)

# do each series separately for ease
npix_df <- ca_wide_df %>%
  select(-starts_with("sharcrop")) %>%
  pivot_longer(starts_with("npix"),
               names_to = "year",
               values_to = "npix") %>%
  mutate(year = as.numeric(str_sub(year, 9, 12))) %>%
  drop_na(geolevel2) %>%
  filter(district_name != "Lake Malawi")

sharecrop_df <- ca_wide_df %>%
  select(-starts_with("npix")) %>%
  pivot_longer(starts_with("sharcrop"),
               names_to = "year",
               values_to = "sharecrop") %>%
  mutate(year = as.numeric(str_sub(year, 9, 12))) %>%
  drop_na(geolevel2) %>%
  filter(district_name != "Lake Malawi")

## level 2 join ----
l2_ca_df <- full_join(npix_df, sharecrop_df) %>%
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
           str_to_title(.)) 

saveRDS(l2_ca_df, paste0(build.dir, "Africa/Crop Area/l2_croparea.rds"))

#*******************************************************************************
# level 1 ----
#*******************************************************************************

ca_wide_df <- read_csv(paste0(raw.dir, "Africa/Crop Area/AFRADM1CROPLAND.CSV")) %>%
  rename(country_code = codeadm0,
         country = nameadm0,
         geolevel1 = codeadm1,
         region_name = nameadm1)

# do each series separately for ease
npix_df <- ca_wide_df %>%
  select(-starts_with("sharcrop")) %>%
  pivot_longer(starts_with("npix"),
               names_to = "year",
               values_to = "npix") %>%
  mutate(year = as.numeric(str_sub(year, 9, 12))) %>%
  drop_na(geolevel1) %>%
  filter(region_name %!in% c("Waterbodies", "Lake Malawi"))

sharecrop_df <- ca_wide_df %>%
  select(-starts_with("npix")) %>%
  pivot_longer(starts_with("sharcrop"),
               names_to = "year",
               values_to = "sharecrop") %>%
  mutate(year = as.numeric(str_sub(year, 9, 12))) %>%
  drop_na(geolevel1) %>%
  filter(region_name %!in% c("Waterbodies", "Lake Malawi"))

l1_ca_df <- full_join(npix_df, sharecrop_df) %>%
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
           str_to_title(.)) 

saveRDS(l1_ca_df, paste0(build.dir, "Africa/Crop Area/l1_croparea.rds"))
