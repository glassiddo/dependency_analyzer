# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Jan 31, 2023
# Title:   Build Nightlight datasets for each country
# Desc:    
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

#*******************************************************************************
# level 2 ----

## unadjusted ----
ntlv_df <- read_csv(paste0(raw.dir, "Africa/Night Light/NTLVALUEADM2.CSV")) %>%
  rename(country_code = codeadm0,
         country = nameadm0,
         geolevel2 = codeadm2,
         district_name = nameadm2) %>%
  pivot_longer(starts_with("ntlv"),
               names_to = "year",
               values_to = "ntlv") %>%
  mutate(year = as.numeric(str_sub(year, 9, 12))) %>%
  group_by(country_code, country, geolevel2, district_name, year) %>%
  summarise(ntlv = max(ntlv, na.rm = TRUE),
            .groups = 'drop') %>%
  drop_na(geolevel2) %>%
  filter(district_name != "Lake Malawi")

# Eti Osa in Nigeria has missing data, can replace in all with NA
#temp <- ntlv_df %>% filter(ntlv == -Inf)

ntla_df <- read_csv(paste0(raw.dir, "Africa/Night Light/NTLAREAADM2.CSV")) %>%
  rename(country_code = codeadm0,
         country = nameadm0,
         geolevel2 = codeadm2,
         district_name = nameadm2) %>%
  pivot_longer(starts_with("ntla"),
               names_to = "year",
               values_to = "ntla") %>%
  mutate(year = as.numeric(str_sub(year, 9, 12))) %>%
  group_by(country_code, country, geolevel2, district_name, year) %>%
  summarise(ntla = max(ntla, na.rm = TRUE),
            .groups = 'drop') %>%
  drop_na(geolevel2) %>%
  filter(district_name != "Lake Malawi")

## consistent ----
ntlacc_df <- read_csv(paste0(raw.dir, "Africa/Night Light/CCNTLAREAADM2.CSV")) %>%
  rename(country_code = codeadm0,
         country = nameadm0,
         geolevel2 = codeadm2,
         district_name = nameadm2) %>%
  rename_with(~gsub("ccntlay", "ntla", .x), starts_with("ccntlay")) %>%
  pivot_longer(starts_with("ntla"),
               names_to = "year",
               values_to = "ntla") %>%
  mutate(year = as.numeric(str_sub(year, 5, 8))) %>%
  drop_na(geolevel2) %>%
  filter(district_name != "Lake Malawi") %>%
  rename(ntlacc = ntla)

ntlvcc_df <- read_csv(paste0(raw.dir, "Africa/Night Light/CCNTLVALUEADM2.CSV")) %>%
  rename(country_code = codeadm0,
         country = nameadm0,
         geolevel2 = codeadm2,
         district_name = nameadm2) %>%
  rename_with(~gsub("ccntlvy", "ntlv", .x), starts_with("ccntlvy")) %>%
  pivot_longer(starts_with("ntlv"),
               names_to = "year",
               values_to = "ntlv") %>%
  mutate(year = as.numeric(str_sub(year, 5, 8))) %>%
  drop_na(geolevel2) %>%
  filter(district_name != "Lake Malawi") %>%
  rename(ntlvcc = ntlv)

## level 2 join ----
l2_nl_df <- full_join(ntla_df, ntlv_df) %>%
  full_join(ntlacc_df) %>%
  full_join(ntlvcc_df) %>%
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
  
saveRDS(l2_nl_df, paste0(build.dir, "Africa/Night Light/l2_nightlight.rds"))

#TODO fill in this section and then update the other night light file...
# level 1 ----
## unadjusted ----
ntlv_df <- read_csv(paste0(raw.dir, "Africa/Night Light/NTLVALUEADM1.CSV")) %>%
  rename(country_code = codeadm0,
         country = nameadm0,
         geolevel1 = codeadm1,
         region_name = nameadm1) %>%
  pivot_longer(starts_with("ntlv"),
               names_to = "year",
               values_to = "ntlv") %>%
  mutate(year = as.numeric(str_sub(year, 9, 12))) %>%
  group_by(country_code, country, geolevel1, region_name, year) %>%
  summarise(ntlv = max(ntlv, na.rm = TRUE),
            .groups = 'drop') %>%
  drop_na(geolevel1) %>%
  filter(region_name %!in% c("Waterbodies", "Lake Malawi")) %>%
  mutate(geolevel1 = as.numeric(geolevel1))

ntla_df <- read_csv(paste0(raw.dir, "Africa/Night Light/NTLAREAADM1.CSV")) %>%
  rename(country_code = codeadm0,
         country = nameadm0,
         geolevel1 = codeadm1,
         region_name = nameadm1) %>%
  pivot_longer(starts_with("ntla"),
               names_to = "year",
               values_to = "ntla") %>%
  mutate(year = as.numeric(str_sub(year, 9, 12))) %>%
  group_by(country_code, country, geolevel1, region_name, year) %>%
  summarise(ntla = max(ntla, na.rm = TRUE),
            .groups = 'drop') %>%
  drop_na(geolevel1) %>%
  filter(region_name %!in% c("Waterbodies", "Lake Malawi")) %>%
  mutate(geolevel1 = as.numeric(geolevel1))

## consistent ----
ntlacc_df <- read_csv(paste0(raw.dir, "Africa/Night Light/CCNTLAREAADM1.CSV")) %>%
  rename(country_code = codeadm0,
         country = nameadm0,
         geolevel1 = codeadm1,
         region_name = nameadm1) %>%
  rename_with(~gsub("ccntlay", "ntla", .x), starts_with("ccntlay")) %>%
  pivot_longer(starts_with("ntla"),
               names_to = "year",
               values_to = "ntla") %>%
  mutate(year = as.numeric(str_sub(year, 5, 8))) %>%
  drop_na(geolevel1) %>%
  filter(region_name %!in% c("Waterbodies", "Lake Malawi")) %>%
  mutate(geolevel1 = as.numeric(geolevel1)) %>%
  rename(ntlacc = ntla)

ntlvcc_df <- read_csv(paste0(raw.dir, "Africa/Night Light/CCNTLVALUEADM1.CSV")) %>%
  rename(country_code = codeadm0,
         country = nameadm0,
         geolevel1 = codeadm1,
         region_name = nameadm1) %>%
  rename_with(~gsub("ccntlvy", "ntlv", .x), starts_with("ccntlvy")) %>%
  pivot_longer(starts_with("ntlv"),
               names_to = "year",
               values_to = "ntlv") %>%
  mutate(year = as.numeric(str_sub(year, 5, 8))) %>%
  drop_na(geolevel1) %>%
  filter(region_name %!in% c("Waterbodies", "Lake Malawi")) %>%
  mutate(geolevel1 = as.numeric(geolevel1)) %>%
  rename(ntlvcc = ntlv)

l1_nl_df <- full_join(ntla_df, ntlv_df) %>%
  full_join(ntlacc_df) %>%
  full_join(ntlvcc_df) %>%
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

saveRDS(l1_nl_df, paste0(build.dir, "Africa/Night Light/l1_nightlight.rds"))

rm(ntla_df, ntlv_df)