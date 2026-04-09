# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Jan 31, 2023
# Title:   Build Nightlight datasets for each country
# Desc:    
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

ntl2_long <- function(fl_name) {
  out_df <- read_csv(paste0(raw.dir, "Africa/Night Light/", fl_name,".CSV")) %>%
    rename(country_code = codeadm0,
           country = nameadm0,
           geolevel2 = codeadm2,
           district_name = nameadm2) %>%
    pivot_longer(contains("ntl"),
                 names_to = "year",
                 values_to = "ntl") %>%
    mutate(year = as.numeric(str_sub(year, -4))) %>%
    group_by(country_code, country, geolevel2, district_name, year) %>%
    summarise(ntl = max(ntl, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(ntl = ifelse(ntl == -Inf, NA, ntl)) %>%
    drop_na(geolevel2) %>%
    filter(district_name != "Lake Malawi")
}

ntl1_long <- function(fl_name) {
  out_df <- read_csv(paste0(raw.dir, "Africa/Night Light/", fl_name,".CSV")) %>%
    rename(country_code = codeadm0,
           country = nameadm0,
           geolevel1 = codeadm1,
           region_name = nameadm1) %>%
    pivot_longer(contains("ntl"),
                 names_to = "year",
                 values_to = "ntl") %>%
    mutate(year = as.numeric(str_sub(year, -4))) %>%
    group_by(country_code, country, geolevel1, region_name, year) %>%
    summarise(ntl = max(ntl, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(ntl = ifelse(ntl == -Inf, NA, ntl)) %>%
    drop_na(geolevel1) %>%
    filter(region_name %!in% c("Waterbodies", "Lake Malawi")) %>%
    mutate(geolevel1 = as.numeric(geolevel1))
}

#*******************************************************************************
# level 2 ----

## unadjusted ----
ntla_df <- ntl2_long("NTLAREAADM2") %>%
  rename(ntla = ntl)

ntlv_df <- ntl2_long("NTLVALUEADM2") %>%
  rename(ntlv = ntl)

# Eti Osa in Nigeria has missing data, can replace in all with NA
#temp <- ntlv_df %>% filter(ntlv == -Inf)

## consistent ----
ntlacc_df <- ntl2_long("CCNTLAREAADM2") %>%
  rename(ntlacc = ntl)

ntlvcc_df <- ntl2_long("CCNTLVALUEADM2") %>%
  rename(ntlvcc = ntl)

## harmonized ----
# A lot of missing data in these...
#ntlah_df <- read_csv(paste0(raw.dir, "Africa/Night Light/HGNTLAREAADM2.CSV"))

ntlah_df <- ntl2_long("HGNTLAREAADM2") %>%
  rename(ntlah = ntl)

#ntlvh_df <- read_csv(paste0(raw.dir, "Africa/Night Light/HGNTLVALUEADM2.CSV"))

ntlvh_df <- ntl2_long("HGNTLVALUEADM2") %>%
  rename(ntlvh = ntl)

## dusk to dawn nl ----
ntldu_df <- read_csv(paste0(raw.dir, "Africa/Night Light/NTLDAWNADM2.CSV")) %>%
  rename(country_code = CNTRY_CODE,
         country = CNTRY_NAME,
         geolevel2 = GEOLEVEL2,
         district_name = ADMIN_NAME) 

nldu_df <- full_join(
  # night time light value
  ntldu_df %>%
    select(-starts_with("nla")) %>%
    pivot_longer(contains("nl"),
                 names_to = "year",
                 values_to = "nlv") %>%
    mutate(year = as.numeric(str_sub(year, -4))) %>%
    group_by(country_code, country, geolevel2, district_name, year) %>%
    summarise(nlv = mean(nlv, na.rm = TRUE),
              .groups = 'drop') %>%
    drop_na(geolevel2) %>%
    filter(district_name != "Lake Malawi"),
  # night time light area
  ntldu_df %>%
    select(-starts_with("nlv")) %>%
    pivot_longer(contains("nl"),
                 names_to = "year",
                 values_to = "nla") %>%
    mutate(year = as.numeric(str_sub(year, -4))) %>%
    group_by(country_code, country, geolevel2, district_name, year) %>%
    summarise(nla = mean(nla, na.rm = TRUE),
              .groups = 'drop') %>%
    drop_na(geolevel2) %>%
    filter(district_name != "Lake Malawi")
) %>%
  mutate(country_code = as.numeric(country_code))

# harmonization
nl_harm <- full_join(ntlv_df, ntla_df) %>%
  full_join(nldu_df) %>%
  mutate(
    # dusk to dawn ratio
    dv2013 = ntlv / nlv,
    da2013 = ntla / nla,
    across(c(dv2013, da2013), ~ifelse(.x %in% c(-Inf, Inf, NaN), 1, .x))
  ) %>%
  group_by(country_code, country, geolevel2, district_name) %>%
  mutate(dv = max(dv2013, na.rm = TRUE),
         da = max(da2013, na.rm = TRUE),
         across(c(dv, da), ~ifelse(.x %in% c(-Inf, Inf, NaN, 0), 1, .x))) %>%
  ungroup() %>%
  rename(nlvda = nlv, nlada = nla) %>%
  mutate(nlv = ifelse(year <= 2013, ntlv, nlvda * dv),
         nla = ifelse(year <= 2013, ntla, nlada * da)) %>%
  select(country_code, country, geolevel2, district_name, year, nlvda, nlada, 
         nlv, nla)

## level 2 join ----
l2_nl_df <- full_join(ntla_df, ntlv_df) %>%
  full_join(ntlacc_df) %>%
  full_join(ntlvcc_df) %>%
  full_join(ntlah_df) %>%
  full_join(ntlvh_df) %>%
  full_join(nl_harm) %>%
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

#*******************************************************************************
# level 1 ----
## unadjusted ----
ntla_df <- ntl1_long("NTLAREAADM1") %>%
  rename(ntla = ntl)

ntlv_df <- ntl1_long("NTLVALUEADM1") %>%
  rename(ntlv = ntl)

## consistent ----
ntlacc_df <- ntl1_long("CCNTLAREAADM1") %>%
  rename(ntlacc = ntl)

ntlvcc_df <- ntl1_long("CCNTLVALUEADM1") %>%
  rename(ntlvcc = ntl)

## harmonized ----
ntlah_df <- ntl1_long("HGNTLAREAADM1") %>%
  rename(ntlah = ntl)

ntlvh_df <- ntl1_long("HGNTLVALUEADM1") %>%
  rename(ntlvh = ntl)

## dusk to dawn nl ----
ntldu_df <- read_csv(paste0(raw.dir, "Africa/Night Light/NTLDAWNADM1.CSV")) %>%
  rename(country_code = CNTRY_CODE,
         country = CNTRY_NAME,
         geolevel1 = GEOLEVEL1,
         region_name = ADMIN_NAME) 

nldu_df <- full_join(
  # night time light value
  ntldu_df %>%
    select(-starts_with("nla")) %>%
    pivot_longer(contains("nl"),
                 names_to = "year",
                 values_to = "nlv") %>%
    mutate(year = as.numeric(str_sub(year, -4))) %>%
    group_by(country_code, country, geolevel1, region_name, year) %>%
    summarise(nlv = mean(nlv, na.rm = TRUE),
              .groups = 'drop') %>%
    drop_na(geolevel1) %>%
    filter(region_name %!in% c("Waterbodies", "Lake Malawi")),
  # night time light area
  ntldu_df %>%
    select(-starts_with("nlv")) %>%
    pivot_longer(contains("nl"),
                 names_to = "year",
                 values_to = "nla") %>%
    mutate(year = as.numeric(str_sub(year, -4))) %>%
    group_by(country_code, country, geolevel1, region_name, year) %>%
    summarise(nla = mean(nla, na.rm = TRUE),
              .groups = 'drop') %>%
    drop_na(geolevel1) %>%
    filter(region_name %!in% c("Waterbodies", "Lake Malawi"))
) %>%
  mutate(geolevel1 = as.numeric(geolevel1))

# harmonization
nl_harm <- full_join(ntlv_df, ntla_df) %>%
  full_join(nldu_df) %>%
  mutate(
    # dusk to dawn ratio
    dv2013 = ntlv / nlv,
    da2013 = ntla / nla,
    across(c(dv2013, da2013), ~ifelse(.x %in% c(-Inf, Inf, NaN), 1, .x))
  ) %>%
  group_by(country_code, country, geolevel1, region_name) %>%
  mutate(dv = max(dv2013, na.rm = TRUE),
         da = max(da2013, na.rm = TRUE),
         across(c(dv, da), ~ifelse(.x %in% c(-Inf, Inf, NaN, 0), 1, .x))) %>%
  ungroup() %>%
  rename(nlvda = nlv, nlada = nla) %>%
  mutate(nlv = ifelse(year <= 2013, ntlv, nlvda * dv),
         nla = ifelse(year <= 2013, ntla, nlada * da)) %>%
  select(country_code, country, geolevel1, region_name, year, nlvda, nlada, 
         nlv, nla)

### NB ----
# nla in the harmonized data can go above 1

## level 1 join ----
l1_nl_df <- full_join(ntla_df, ntlv_df) %>%
  full_join(ntlacc_df) %>%
  full_join(ntlvcc_df) %>%
  full_join(ntlah_df) %>%
  full_join(ntlvh_df) %>%
  full_join(nl_harm) %>%
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