# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Dec 6, 2022
# Title:   Build Nightlight datasets for each country
# Desc:    
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")
source("code/Functions/F-Bartik.R")

#*******************************************************************************
# night light ----

## level 2 ----
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

l2_nl_df <- full_join(ntla_df, ntlv_df) %>%
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

## level 1 ----

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

l1_nl_df <- full_join(ntla_df, ntlv_df) %>%
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

rm(ntla_df, ntlv_df)

#*******************************************************************************
# Benin ----
c_name <- n_ctry("BEN")
l2_df <- l2_nl_df %>% filter(country == c_name) %>% ren_ben(lvl = 2)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name) %>% ren_ben(lvl = 1)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Botswana ----
c_name <- n_ctry("BWA")
l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Burkina Faso ----
c_name <- n_ctry("BFA")
l2_df <- l2_nl_df %>% filter(country == c_name)

# urb_df <- read_urb(c_name, 2)
# nl_df <- l2_df %>% group_by(district_name) %>% summarise(ntla = mean(ntla))
# match_df <- full_join(nl_df, urb_df) %>% arrange(district_name)

saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

# urb_df <- read_urb(c_name, 1)
# nl_df <- l1_df %>% group_by(region_name) %>% summarise(ntla = mean(ntla))
# match_df <- full_join(nl_df, urb_df)

#*******************************************************************************
# Egypt ----
c_name <- n_ctry("EGY")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Ghana ----
c_name <- n_ctry("GHA")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Guinea GIN ----
c_name <- n_ctry("GIN")
l2_df <- l1_nl_df %>% filter(country == c_name) %>%
  rename(district_name = region_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

#*******************************************************************************
# Kenya ----
c_name <- n_ctry("KEN")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Lesotho ----
c_name <- n_ctry("LSO")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Mali ----
c_name <- n_ctry("MLI")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Mauritius ----
c_name <- n_ctry("MUS")
l2_df <- l2_nl_df %>% filter(country == c_name) %>%
  filter(str_detect(district_name, "Unknown Municipality") == FALSE)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Mozambique MOZ ----
c_name <- n_ctry("MOZ")
l2_df <- l2_nl_df %>% filter(country == c_name) %>%
  filter(district_name %!in% c("Aeroporto", "Lake Malawi")) %>%
  mutate(district_name = case_when(
    district_name %in% c("Morrumbene", "Maxixe")~  "Morrumbene, Maxixe",
    district_name %in% c("Nacala-Porto", "Nacala-Velha") 
    ~"Nacala-Porto, Nacala-Velha",
    substr(district_name, 1, 15) == "Distrito Urbano"~ "Maputo City",
    TRUE~district_name)) %>%
  group_by(district_name, year) %>%
  #TODO to be precise I should be weighting by the size of each location
  summarise(across(c(ntla, ntlv), ~mean(.x)), .groups = 'drop')
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Senegal ----
c_name <- n_ctry("SEN")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Sierra Leone ----
c_name <- n_ctry("SLE")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))


#*******************************************************************************
# South Africa ----
c_name <- n_ctry("SAF")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Tanzania ----
c_name <- n_ctry("TZA")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Uganda ----
c_name <- n_ctry("UGA")
l2_df <- l1_nl_df %>% filter(country == c_name) %>%
  rename(district_name = region_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

#*******************************************************************************
# Zambia ----
c_name <- n_ctry("ZMB")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))
  

