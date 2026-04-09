#* Project: Migration Africa
#* Author:  Sam Marshall
#* Created: June 9, 2022
#* Title:   Clean census files for each country
#* Output:  
#* Notes:  
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")
source("code/Build/1_Census/0-scrape_ipums_and_function/F-Census.R")

#*NB: Kenya no mining data
#* Senegal has inconsistent coding
#* Cameroon: used 2005 for urban status

ctries_dir <- "Countries"
cens_dir <- "Census"


#*******************************************************************************
# Angola ----
# not an IPUMS country, see cleaning in Build/Census/Clean non-IPUMS/AGO
#*******************************************************************************

cname <- "AGO"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent14.dta")
)   

r <- geo_labels(ctry_df, "geo1_ao", region_name) 
d <- geo_labels(ctry_df, "geo2_ao", district_name) 

pr <- geo_labels(ctry_df, "mig1_5_ao", prev_region_name)
pd <- geo_labels(ctry_df, "mig2_5_ao", prev_district_name)

pc <- geo_labels(ctry_df, "migctry5", prev_country_name) 

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, pc) %>% 
  urban_status(2014) %>%
  mutate(across(contains("name"), 
                ~ifelse(str_sub(.x, 1, 3) == "Niu","Niu (Not In Universe)", .x)))

region <- ctry_df %>% 
  select(region_name) %>%
  unique() %>%
  arrange(region_name) %>%
  mutate(region = row_number())

prev_region <- region %>% select(contains("region")) %>% unique() %>%
  rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_region_name) %>% 
              unique() ) %>%
  mutate(
    prev_region = case_when(
      prev_region_name == "Abroad"~ 100,
      prev_region_name == "Niu (Not In Universe)"~ 98,
      prev_region_name == "Unknown"~ 99,
      TRUE ~ as.numeric(prev_region)
    )
  ) %>%
  unique()

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name) %>% unique() ) %>% 
  code_prev_dist(10, 101) ## not sure whats the point of this

ago14_df <- ctry_df %<>% 
  full_join(district) %>%
  left_join(prev_district)

saveRDS(ago14_df, here(build.dir, ctries_dir, cname, cens_dir, "census14.rds"))

rm(cname, ctry_df, d, r, pr, pc, region, prev_region, 
   district, prev_district)
rm(ago14_df)

#*******************************************************************************
# Benin ----
# no emigration data in second census
#*******************************************************************************

cname <- "BEN"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent02_13.dta")
  ) %>%
  rename(duration = migyrs1)

d <- geo_labels(ctry_df, "geo2_bj", district_name) 
r <- geo_labels(ctry_df, "geo1_bj", region_name) 

pd <- geo_labels(ctry_df, "mig2_p_bj", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_p_bj", prev_region_name)
br <- geo_labels(ctry_df, "bplbj1", birth_region_name)
bd <- geo_labels(ctry_df, "bplbj2", birth_district_name)
pc <- geo_labels(ctry_df, "migctryp", prev_country_name) 

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, bd, br, pc, dur = TRUE) %>%
  urban_status(2002) %>%
  ren_ben(lvl = 2) %>%
  ren_ben(lvl = 1) %>%
  mutate(across(contains("name"), 
                ~ifelse(str_sub(.x, 1, 3) == "Niu","Niu (Not In Universe)", .x)))

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% 
              select(prev_district_name, prev_region_name) %>% 
              unique() ) %>%
  code_prev_dist(100, 1001, unk_r = 10) 

birth_district <- prev_district %>% 
  rename_with(~sub("prev_", "birth_", .x)) %>%
  # select(contains("region")) %>% unique() %>%
  {.}

ctry_df %<>% full_join(district) %>%
  left_join(prev_district) %>%
  left_join(birth_district)

ben02_df <- ctry_df %>% filter(year == 2002)
ben13_df <- ctry_df %>% filter(year == 2013)

saveRDS(ben02_df, here(build.dir, ctries_dir, cname, cens_dir, "census02.rds"))
saveRDS(ben13_df, here(build.dir, ctries_dir, cname, cens_dir, "census13.rds"))

rm(cname, ctry_df, d, r, pd, pr, br, bd, pc, district, prev_district, birth_district)
rm(ben02_df, ben13_df)

#*******************************************************************************
# Botswana ----
#* Birth district and location 5 years ago
#*******************************************************************************

cname <- "BWA"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent01_11.dta")
  ) %>% 
  mutate(urban = NA)

r <- geo_labels(ctry_df, "geo1_bw", region_name) 

pr <- geo_labels(ctry_df, "mig1_5_bw", prev_region_name) %>%
  # Southern region mislabeled as Borolong
  mutate(prev_region_name = ifelse(
    mig1_5_bw == 72010, 
    "Ngwaketse, Ngwaketse West, Ngwaketse Southern, Southern, Jwaneng",
    prev_region_name))
br <- geo_labels(ctry_df, "bplbw", birth_region_name)
pc <- geo_labels(ctry_df, "migctry5", prev_country_name) 

ctry_df <- clean_census(ctry_df, "indgen", r, pr, br, pc) %>%
  mutate(id_base = str_sub(as.character(geo1_bw),1, -3),
         bpl_sfx = as.character(bplbw) %>%
           ifelse(str_length(.) == 1, paste0("0", .), .),
         bpl1_bw = paste0(id_base, bpl_sfx),
    region_name = ifelse(region_name == "Lobaste", "Lobatse", region_name),
    birth_region_name = case_when(
      birth_region_name == "Barolong" ~"Borolong",
      birth_region_name == "Selibe-Phikwe"~ "Selebi Phikwe",
      birth_region_name == "Tshabon (Kgalagadi South)" 
      ~"Tshabong (Kgalagadi South)",
      birth_region_name == "Central Serowe/ Palapye"~ "Central Serowe/Palapye",
      substr(birth_region_name,1,6) == "Ghanzi" 
      ~ "Ghanzi, Central Kgalagadi Game Reserve (Ckgr)",
      TRUE~birth_region_name),
    # largest city by far
    urban = ifelse(region_name == "Gaborone", 1, 0)) %>%
  select(-id_base, -bpl_sfx)

region <- ctry_df %>% select(region_name) %>% unique() %>%
  arrange(region_name) %>%
  mutate(region = row_number())

prev_region <- region %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_region_name) %>% unique() ) %>%
  mutate(
    prev_region = case_when(
      prev_region_name == "Botswana, District Unknown"~ 97,
      prev_region_name == "Unknown"~ 98,
      prev_region_name == "Niu (Not In Universe)"~ 99,
      prev_region_name == "Abroad"~ 100,
      TRUE~as.numeric(prev_region) )) 

birth_region <- prev_region %>% rename_with(~sub("prev_", "birth_", .x)) 

ctry_df %<>% full_join(region) %>%
  left_join(prev_region) %>%
  left_join(birth_region) %>% 
  mutate(across(c(geo1_bw, geomig1_1, geomig1_5), ~paste0("0", .x)))
#### the last row adds a 0 in the beginning to turn the geo code into 6 digits
#### however, this turns it from double to character (as 072 != 72)
#### and thus prevents the bind_rows in 4-census-ipums-xwalk.R
#### so in the other code, every other code is forced into character
#### if there's another solution for that, feel free to change it

bwa01_df <- ctry_df %>% filter(year == 2001)
bwa11_df <- ctry_df %>% filter(year == 2011)

saveRDS(bwa01_df, here(build.dir, ctries_dir, cname, cens_dir, "census01.rds"))
saveRDS(bwa11_df, here(build.dir, ctries_dir, cname, cens_dir, "census11.rds"))

rm(cname, ctry_df, r, pr, br, pc, region, prev_region, birth_region)
rm(bwa01_df, bwa11_df)

#*******************************************************************************
# Burkina Faso ----
# no urban status for first round
#*******************************************************************************

cname <- "BFA"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent96_06.dta")
)

d <- geo_labels(ctry_df, "geo2_bf", district_name) 
r <- geo_labels(ctry_df, "geo1_bf", region_name) 

pd <- geo_labels(ctry_df, "migbf", prev_district_name)
bd <- geo_labels(ctry_df, "bplbf", birth_district_name)
pc <- geo_labels(ctry_df, "migctry1", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, bd, pc) %>%
  mutate(id_base = str_sub(as.character(geo2_bf),1, -3),
         mig_sfx = as.character(migbf),
         bpl_sfx = as.character(bplbf),
         across(c(mig_sfx, bpl_sfx), 
                ~ifelse(str_length(.x) == 1, paste0("0", .x), .x)),
         mig2_1_bf = paste0(id_base, mig_sfx),
         bpl2_bf = paste0(id_base, bpl_sfx),
         birth_district_name = ifelse(birth_district_name == "Foreign Country",
                                      "Abroad", birth_district_name)) %>%
  urban_status(2006, district_name) %>%
  select(-id_base, -bpl_sfx, mig_sfx)

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name) %>% unique() ) %>%
  code_prev_dist(100, 1001)

birth_district <- prev_district %>% rename_with(~sub("prev_", "birth_", .x))

ctry_df %<>% full_join(district) %>%
  left_join(prev_district) %>%
  left_join(birth_district)

bfa96_df <- ctry_df %>% filter(year == 1996)
bfa06_df <- ctry_df %>% filter(year == 2006)

saveRDS(bfa96_df, here(build.dir, ctries_dir, cname, cens_dir, "census96.rds"))
saveRDS(bfa06_df, here(build.dir, ctries_dir, cname, cens_dir, "census06.rds"))

rm(cname, ctry_df, d, r, pd, bd, pc, district, prev_district, birth_district)
rm(bfa96_df, bfa06_df)

#*******************************************************************************
# Cameroon ----
#* Could possibly use non-spatially adjusted locations
#*******************************************************************************

cname <- "CMR"
ctry_df <- read_dta(here(
  raw.dir, ctries_dir, cname, cens_dir, "consistent87_05.dta")
  ) %>%
  rename(duration = migyrs1)

d <- geo_labels(ctry_df, "geo2_cm", district_name) 
r <- geo_labels(ctry_df, "geo1_cm", region_name) 

pd <- geo_labels(ctry_df, "mig2_p_cm", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_p_cm", prev_region_name)
pc <- geo_labels(ctry_df, "migctryp", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, pc, dur = TRUE) %>%
  urban_status(2005) %>%
  mutate(
    across(contains("name"), ~ifelse(str_sub(.x, 1, 3) == "Niu",
                                     "Niu (Not In Universe)", .x)),
    prev_region_name = ifelse(
      prev_region_name == "Nord,  Adamoua , Extreme Nord",
      "Nord,  Adamoua, Extreme Nord", prev_region_name))

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name, prev_region_name) %>% 
              unique() ) %>%
  code_prev_dist(10, 1001, unk_r = 10) %>%
  mutate(prev_district = ifelse(prev_district == 40, 400, prev_district)) %>%
  unique()

ctry_df %<>% full_join(district) %>%
  left_join(prev_district) 

cmr87_df <- ctry_df %>% filter(year == 1987)
cmr05_df <- ctry_df %>% filter(year == 2005)

saveRDS(cmr87_df, here(build.dir, ctries_dir, cname, cens_dir, "census87.rds"))
saveRDS(cmr05_df, here(build.dir, ctries_dir, cname, cens_dir, "census05.rds"))

rm(cname, ctry_df, d, r, pd, pr, pc, district, prev_district)
rm(cmr87_df, cmr05_df)

#*******************************************************************************
# Ghana ----
# no emigration data in second census
#*******************************************************************************

cname <- "GHA"
ctry_df <- read_dta(here(
  raw.dir, ctries_dir, cname, cens_dir, "consistent00_10.dta")
) 
  
d <- geo_labels(ctry_df, "geo2_gh", district_name) 
r <- geo_labels(ctry_df, "geo1_gh", region_name) 

pd <- geo_labels(ctry_df, "mig2_5_gh", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_5_gh", prev_region_name)
br <- geo_labels(ctry_df, "bplgh", birth_region_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, br) %>%
  mutate(id_base = str_sub(as.character(geo1_gh),1, -3),
         bpl_sfx = as.character(bplgh) %>%
           ifelse(str_length(.) == 1, paste0("0", .), .),
         bpl1_gh = paste0(id_base, bpl_sfx)) %>%
  urban_status(2000) %>%
  select(-id_base, -bpl_sfx)

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name) %>% unique() ) %>%
  code_prev_dist(100, 10001) 

birth_region <- prev_district %>% rename_with(~sub("prev_", "birth_", .x)) %>%
  select(contains("region")) %>% unique()

ctry_df %<>% full_join(district) %>%
  left_join(prev_district) %>%
  left_join(birth_region)

gha00_df <- ctry_df %>% filter(year == 2000)
gha10_df <- ctry_df %>% filter(year == 2010)

saveRDS(gha00_df, here(build.dir, ctries_dir, cname, cens_dir, "census00.rds"))
saveRDS(gha10_df, here(build.dir, ctries_dir, cname, cens_dir, "census10.rds"))

rm(cname, ctry_df, d, r, pd, pr, br, district, prev_district, birth_region)
rm(gha00_df, gha10_df)

#*******************************************************************************
# Guinea ----
#*******************************************************************************

cname <- "GIN"
ctry_df <- read_dta(here(
  raw.dir, ctries_dir, cname, cens_dir, "consistent96_14.dta")
  ) %>% 
  rename(duration = migyrs1)

r <- geo_labels(ctry_df, "geo1_gn", region_name)
#r <- geo_labels(ctry_df, "regngn", region_name) 

pr <- geo_labels(ctry_df, "mig1_p_gn", prev_region_name)
br <- geo_labels(ctry_df, "bplgn", birth_region_name)
pc <- geo_labels(ctry_df, "migctryp", prev_country_name)

ctry_df <- clean_census(ctry_df, "gn1996a_occ", r, pr, br, pc, dur = TRUE) %>%
  mutate(id_base = str_sub(as.character(geo1_gn),1, -3),
         bpl_sfx = as.character(bplgn) %>%
           ifelse(str_length(.) == 1, paste0("0", .), .),
         bpl1_gn = paste0(id_base, bpl_sfx),
         across(contains("name"), ~case_when(
    .x == "N'zerekore" ~ "Nzerekore",
    .x %in% c("Born In Guinea, Prefecture Unknown","Guinea, Place Unknown")
    ~"Unknown",
    TRUE ~ .x))) %>%
  combine_br() %>%
  urban_status(1996, geo = region_name) %>%
  select(-id_base, -bpl_sfx)

region <- ctry_df %>% select(region_name) %>% unique() %>%
  arrange(region_name) %>%
  mutate(region = row_number())

prev_region <- region %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_region_name) %>% unique() ) %>%
  mutate(
    prev_region = case_when(
      prev_region_name == "Unknown"~ 98,
      prev_region_name == "Niu (Not In Universe)"~ 99,
      prev_region_name == "Abroad"~ 100,
      TRUE~as.numeric(prev_region) )) 

birth_region <- prev_region %>% rename_with(~sub("prev_", "birth_", .x)) 

ctry_df %<>% full_join(region) %>%
  left_join(prev_region) %>%
  left_join(birth_region) 

gin96_df <- ctry_df %>% filter(year == 1996)
gin14_df <- ctry_df %>% filter(year == 2014)

saveRDS(gin96_df, here(build.dir, ctries_dir, cname, cens_dir, "census96.rds"))
saveRDS(gin14_df, here(build.dir, ctries_dir, cname, cens_dir, "census14.rds"))

rm(cname, ctry_df, r, pr, br, pc, region, prev_region, birth_region)
rm(gin96_df, gin14_df)

#*******************************************************************************
# Ivory Coast ----
#*******************************************************************************

cname <- "CIV"  
ctry_df <- read_dta(here(
  raw.dir, ctries_dir, cname, cens_dir, "consistent98.dta")
  ) 

r <- geo_labels(ctry_df, "geo1_ci", region_name) 
d <- geo_labels(ctry_df, "geo2_ci", district_name) 

pr <- geo_labels(ctry_df, "mig1_1_ci", prev_region_name)
pd <- geo_labels(ctry_df, "mig2_1_ci", prev_district_name)

br <- geo_labels(ctry_df, "bpl1_ci", birth_region_name)
bd <- geo_labels(ctry_df, "bpl2_ci", birth_district_name)

pc <- geo_labels(ctry_df, "migctry1", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", r, d, pr, pd, br, bd, pc) %>% 
  urban_status(1998) %>% 
  mutate(
    across(contains("name"), ~ifelse(str_sub(.x, 1, 3) == "Niu",
                                     "Niu (Not In Universe)", .x)),
    region_name = case_when(
      region_name == "Vallée Du Bandam, Lacs, N'zi Comoé" ~
        "Vallée du Bandam, Lacs, N'Zi Comoé",
      # uppercase the z in 'N'zi' and lowercase d in 'Du'
      # to harmonise with the names of the GIS regions
      TRUE ~ region_name
    ),
    prev_region_name = case_when(
      prev_region_name == "Vallée Du Bandam, Lacs, N'zi Comoé" ~
        "Vallée du Bandam, Lacs, N'Zi Comoé",
      # uppercase the z in 'N'zi' and lowercase d in 'Du'
      # to harmonise with the names of the GIS regions
      TRUE ~ prev_region_name
    )
  ) 
  
region <- ctry_df %>% 
  select(region_name) %>%
  unique() %>%
  arrange(region_name) %>%
  mutate(region = row_number())

prev_region <- region %>% select(contains("region")) %>% unique() %>%
  rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_region_name) %>% 
              unique() ) %>%
  mutate(
    prev_region = case_when(
      prev_region_name == "Abroad"~ 100,
      prev_region_name == "Niu (Not In Universe)"~ 98,
      prev_region_name == "Unknown"~ 99,
      TRUE ~ as.numeric(prev_region)
    )
  ) %>%
  unique()

birth_region <- prev_region %>% rename_with(~sub("prev_", "birth_", .x)) 

district <- district_codes(ctry_df) 

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name) %>% unique() ) %>% 
  code_prev_dist(10, 101) ## not sure whats the point of this

birth_district <- prev_district %>% rename_with(~sub("prev_", "birth_", .x)) %>%
  # select(contains("region")) %>% unique() %>%
  {.}

civ98_df <- ctry_df %<>% full_join(district) %>%
  left_join(prev_district) %>%
  left_join(birth_district) 

saveRDS(civ98_df, here(build.dir, ctries_dir, cname, cens_dir, "census98.rds"))

rm(cname, ctry_df, d, r, pd, pr, pc, br, bd, region, prev_region, birth_region,
   district, prev_district, birth_district)
rm(civ98_df)

#*******************************************************************************
# Kenya ----
#*******************************************************************************

cname <- "KEN"
ctry_df <- read_dta(here(
  raw.dir, ctries_dir, cname, cens_dir, "consistent99_09.dta")
  ) %>% 
  rename(duration = migyrs1)

d <- geo_labels(ctry_df, "geo2_ke", district_name) 
r <- geo_labels(ctry_df, "geo1_ke", region_name) 

pd <- geo_labels(ctry_df, "mig2_p_ke", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_p_ke", prev_region_name)
pd1 <- geo_labels(ctry_df, "mig2_1_ke", prev_district_name1)
pr1 <- geo_labels(ctry_df, "mig1_1_ke", prev_region_name1)
bd <- geo_labels(ctry_df, "bplke", birth_district_name)
pc <- geo_labels(ctry_df, "migctry1", prev_country_name)

ctry_df <- clean_census(ctry_df, "ke1999a_econactx", d, r, pd, pr, pd1, pr1, 
                        bd, pc, dur = TRUE) %>%
  mutate(
    across(c(prev_district_name, prev_region_name),
           ~ifelse(is.na(.x) == TRUE, get(paste0(cur_column(),"1")), .x )),
    # these districts are split so just make any one
    birth_district_name = sub("Nyandaura", "Nyandarua", birth_district_name) %>%
      sub("Kajiado", "Kaijiado", .),
    birth_district_name = case_when(
      birth_district_name == "Butere/Mumias" ~"Butere",
      birth_district_name %in% c("Bungoma", "Kakamega", "Kisumu", "Kuria",
                                 "Laikipia", "Mandera", "Nairobi", "Nandi", 
                                 "Thika", "Trans Nzoia") 
      ~paste0(birth_district_name, " East"),
      birth_district_name %in% c("Kaijiado", "Kitui", "Muranga", "Narok", 
                                 "Nyandarua", 
                                 "Nyeri", "Teso", "Samburu",
                                 "Turkana", "Wajir") 
      ~paste0(birth_district_name, " North"),
      substr(birth_district_name, 1, 4) == "Meru" ~"Meru South",
      birth_district_name == "Narok Wouth" ~"Narok South",
      birth_district_name == "North Kisii" ~"Kisii South",
      birth_district_name == "Taita Taveta" ~"Taita, Taveta",
      # this one doesn't show up but most likely in same cons as this one
      birth_district_name == "Maragua" ~"Muranga South",
      # eldoret in UAsin gishu county
      birth_district_name == "Uasin Gishu" ~"Eldoret East",
      birth_district_name == "Foreign-Born" ~"Abroad",
      birth_district_name == "Unknown/Missing" ~"Unknown",
      TRUE~birth_district_name)) %>%
  select(-prev_district_name1, -prev_region_name1) %>%
  combine_bl() %>%
  urban_status(1999)

# create codes
district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  full_join(ctry_df %>% select(prev_district_name) %>% unique()) %>%
  code_prev_dist(10, 1001)

birth_district <- prev_district %>% rename_with(~sub("prev_", "birth_", .x)) 

ctry_df %<>% full_join(district) %>%
  left_join(prev_district) %>%
  left_join(birth_district)

ken99_df <- ctry_df %>% filter(year == 1999)
ken09_df <- ctry_df %>% filter(year == 2009)

saveRDS(ken99_df, here(build.dir, ctries_dir, cname, cens_dir, "census99.rds"))
saveRDS(ken09_df, here(build.dir, ctries_dir, cname, cens_dir, "census09.rds"))

rm(cname, ctry_df, d, r, pd, pd1, pr, pr1, bd, pc, district, prev_district, 
   birth_district)
rm(ken99_df, ken09_df)

#*******************************************************************************
# Lesotho ----
#*******************************************************************************

cname <- "LSO"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent96_06.dta")
) 

r <- geo_labels(ctry_df, "geo1_ls", region_name) 

pr <- geo_labels(ctry_df, "ls2006a_res10yr", prev_region_name)
pr1 <- geo_labels(ctry_df, "ls1996a_res10yr", prev_region_name1)
br <- geo_labels(ctry_df, "ls2006a_bpl", birth_region_name)
br1 <- geo_labels(ctry_df, "ls1996a_bpl", birth_region_name1)
pc <- geo_labels(ctry_df, "migctry0", prev_country_name)

ctry_df <- clean_census(ctry_df, "ls1996a_occ", r, pr, pr1, br, br1, pc) %>%
  mutate(
    across(c(prev_region_name, birth_region_name),
           ~ifelse(is.na(.x) == TRUE, get(paste0(cur_column(),"1")), .x )),
    across(c(prev_region_name, birth_region_name),
           ~case_when(
             .x == "Same Village" ~region_name,
             .x == "Different Village, This District" ~region_name,
             .x == "Different Village, Same District" ~region_name,
             .x == "Botha-Bothe" ~"Butha Buthe",
             .x == "Thaba Tseka" ~"Thaba-Tseka",
             .x == "Republic Of South Africa" ~"South Africa",
             .x %in% c("Others", "Other Countries") ~"Abroad",
             TRUE ~ .x))) %>%
  select(-prev_region_name1, -birth_region_name1) %>%
  filter(region_name != "Unknown") %>%
  urban_status(1996, geo = region_name)

region <- ctry_df %>% select(region_name) %>% unique() %>%
  arrange(region_name) %>%
  mutate(region = row_number())

prev_region <- region %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_region_name) %>% unique() ) %>%
  mutate(
    prev_region = case_when(
      prev_region_name == "Niu (Not In Universe)"~ 99,
      prev_region_name == "Abroad"~ 100,
      prev_region_name == "South Africa"~ 101,
      TRUE~as.numeric(prev_region) )) 

birth_region <- prev_region %>% rename_with(~sub("prev_", "birth_", .x)) 

ctry_df %<>% full_join(region) %>%
  left_join(prev_region) %>%
  left_join(birth_region)

lso96_df <- ctry_df %>% filter(year == 1996)
lso06_df <- ctry_df %>% filter(year == 2006)

saveRDS(lso96_df, here(build.dir, ctries_dir, cname, cens_dir, "census96.rds"))
saveRDS(lso06_df, here(build.dir, ctries_dir, cname, cens_dir, "census06.rds"))

rm(cname, ctry_df, r, pr, pr1, br, br1, pc, region, prev_region, birth_region)
rm(lso96_df, lso06_df)

#*******************************************************************************
# Malawi ----
#*******************************************************************************

cname <- "MWI"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent98_08.dta")
  ) %>%  
  rename(duration = migyrs1)

d <- geo_labels(ctry_df, "geo2_mw", district_name) 
r <- geo_labels(ctry_df, "geo1_mw", region_name) 

pr <- geo_labels(ctry_df, "mig1_p_mw", prev_region_name)
br <- geo_labels(ctry_df, "bplmw", birth_region_name)
pc <- geo_labels(ctry_df, "migctryp", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pr, br, pc, dur = TRUE) %>%
  urban_status(1998) %>%
  mutate(
    across(contains("name"), ~ifelse(str_sub(.x, 1, 3) == "Niu",
                                     "Niu (Not In Universe)", .x)),
    birth_region_name = case_when(
      birth_region_name %in% c("Blantyre", "Blantyre City") 
      ~"Blantyre, Blantyre City",
      birth_region_name == "Kasunga" ~"Kasungu",
      birth_region_name %in% c("Lilongwe", "Lilongwe City") 
      ~"Lilongwe, Lilongwe City",
      birth_region_name %in% c("Mwanza", "Neno") ~"Mwanza, Neno",
      birth_region_name %in% c("Mzimba", "Mzuzu City") 
      ~"Mzimba, Mzuzu City",
      birth_region_name == "Nkhata Bay And Likoma" ~"Nkhata Bay, Likoma",
      birth_region_name == "Nthisi" ~"Ntchisi",
      birth_region_name %in% c("Zomba", "Zomba City") ~"Zomba, Zomba City",
             TRUE ~ birth_region_name))

district <- district_codes(ctry_df)

prev_region <- district %>% select(contains("region")) %>% unique() %>%
  rename_with(~paste0("prev_", .x)) %>%
  full_join(ctry_df %>% select(prev_region_name) %>% unique()) %>%
  mutate(
    prev_region = case_when(
      prev_region_name == "Abroad"~ 100,
      prev_region_name == "Niu (Not In Universe)"~ 98,
      prev_region_name == "Unknown"~ 99,
      TRUE ~ as.numeric(prev_region)
    )
  )

birth_region <- prev_region %>% rename_with(~sub("prev_", "birth_", .x)) %>%
  full_join(ctry_df %>% select(birth_region_name) %>% unique())

ctry_df %<>% full_join(district) %>%
  left_join(prev_region) %>%
  left_join(birth_region) 

mwi98_df <- ctry_df %>% filter(year == 1998)
mwi08_df <- ctry_df %>% filter(year == 2008)

saveRDS(mwi98_df, here(build.dir, ctries_dir, cname, cens_dir, "census98.rds"))
saveRDS(mwi08_df, here(build.dir, ctries_dir, cname, cens_dir, "census08.rds"))

rm(cname, ctry_df, d, r, pr, br, pc, district, prev_region, birth_region)
rm(mwi98_df, mwi08_df)


#*******************************************************************************
# Mali ----
#*******************************************************************************

cname <- "MLI"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent98_09.dta")) %>%
  rename(duration = migyrs1)

d <- geo_labels(ctry_df, "geo2_ml", district_name)
r <- geo_labels(ctry_df, "geo1_ml", region_name) 

pd <- geo_labels(ctry_df, "mig2_p_ml", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_p_ml", prev_region_name)
bd <- geo_labels(ctry_df, "bplml", birth_district_name)
pc <- geo_labels(ctry_df, "migctryp", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, bd, pc, dur = TRUE) %>%
  mutate(
    birth_district_name = case_when(
      substr(birth_district_name, 1, 8) == "District"~"District Of Bamako",
      substr(birth_district_name, 1, 7) == "Commune"~"District Of Bamako",
      birth_district_name == "Gourma Rharous" ~"Gourma-Rharous",
      birth_district_name == "Tin Essako" ~"Tin-Essako",
      birth_district_name == "Youvarou" ~"Youwarou",
      birth_district_name == "Baraoueli" ~"Baroueli",
      birth_district_name %in% c("Gao Region, Unknown Circle", 
                                 "Kidal Region, Unknown Circle")
      ~"Gao, Kidal Regions, Unknown Circle",
      TRUE~birth_district_name)) %>%
  combine_bl() %>%
  urban_status(1998)

# create codes
district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  full_join(ctry_df %>% select(prev_district_name, prev_region_name) %>% 
              unique()) %>%
  code_prev_dist(10, 101, unk_r = 10) %>%
  mutate(
    prev_region = ifelse(prev_region_name == "Mali, Province Unspecified",
                         9, prev_region),
    prev_district = ifelse(prev_region_name == "Mali, Province Unspecified",
                           97, prev_district))

birth_district <- prev_district %>% rename_with(~sub("prev_", "birth_", .x)) 

ctry_df %<>% full_join(district) %>%
  left_join(prev_district) %>%
  left_join(birth_district)

mli98_df <- ctry_df %>% filter(year == 1998)
mli09_df <- ctry_df %>% filter(year == 2009)

saveRDS(mli98_df, here(build.dir, ctries_dir, cname, cens_dir, "census98.rds"))
saveRDS(mli09_df, here(build.dir, ctries_dir, cname, cens_dir, "census09.rds"))

rm(cname, ctry_df, d, pd, r, pr, bd, pc, district, prev_district,birth_district)
rm(mli98_df, mli09_df)

#*******************************************************************************
# Mauritius ----
# can possibly do MCVA / ward, but will require some work, for now do region
#*******************************************************************************

cname <- "MUS"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir,"consistent00_11.dta")
  )

d <- geo_labels(ctry_df, "geo2_mu", district_name) 
r <- geo_labels(ctry_df, "geo1_mu", region_name) 

pd <- geo_labels(ctry_df, "migmu", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_5_mu", prev_region_name)
pc <- geo_labels(ctry_df, "migctry5", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, pc) %>%
  mutate(
    # one district missing last combo
    district_name = ifelse(substr(district_name, 1, 4) == "Nouv",
                           paste0(district_name, 
                                  ", St. Julien (Haut De Flacq) West"),
                           district_name),
    prev_district_name = gsub(" Vca", "", prev_district_name) %>%
      gsub("Municipality Unknown", "Unknown Municipality", .) %>%
      gsub("  ", " ", .) %>%
      gsub("Hill Ward", "Hill-Ward", .) %>%
      gsub("Curepipe Ward", "Curepipe-Ward", .) %>%
      gsub("Bornes Ward", "Bornes-Ward", .) %>%
      gsub("Phoenix Ward", "Phoenix-Ward", .) %>%
      gsub("Port-Louis - Ward", "Port Louis-Ward", .) %>%
      gsub("St. ", "St ", .),
    prev_district_name = case_when(
      prev_district_name == "Bamboo Virieux" ~"Bambous Virieux",
      prev_district_name == "Beneres" ~"Benares",
      prev_district_name == "Bel Air - Riviere Seche" ~"Bel Air Riviere Seche",
      prev_district_name == "Belle Vue Maurel South" ~"Belle Vue Maurel",
      prev_district_name == "Bois Cherie" ~"Bois Cheri",
      prev_district_name == "Bon Acceuil" ~"Bon Accueil",
      prev_district_name == "Clementia" ~"Clemencia",
      prev_district_name == "Creve Coeur" ~"Creve Ceur",
      prev_district_name == "Grande River South East" ~"Grand River South-East",
      str_detect(prev_district_name, "La Ferme") == TRUE ~"La Ferme",
      prev_district_name == "Laventure South" ~"Laventure",
      prev_district_name == "Montagne Blanches East" ~"Montagne Blanche East",
      prev_district_name == "Nouvelle Decouvert" ~"Nouvelle Decouverte",
      prev_district_name == "Plaines Wilhems District, Unknown Municipality" 
      ~"Plaine Wilhems District, Unknown Municipality",
      prev_district_name == "Quartier Militairevca" ~"Quartier Militaire",
      prev_district_name == "Quatre Soeurs" ~"Quatre Seurs",
      str_detect(prev_district_name, "Region") == TRUE 
      ~str_sub(prev_district_name, 10),
      prev_district_name == "Riviere Des Creole" ~"Riviere Des Creoles",
      prev_district_name == "Riviere Du Rempart" ~"Riviere Du Rempart ",
      prev_district_name == "Roche Noires" ~"Roches Noires",
      prev_district_name == "Roches Terre" ~"Roche Terre",
      prev_district_name == "St Aubin" ~"Saint Aubin",
      prev_district_name == "St.julien D'hotman West" ~"St Julien D'hotman West",
      prev_district_name == "Tombeau Bay" ~"Baie Du Tombeau",
      prev_district_name == "Town Of Port-Louis Ward 1 West" 
      ~"Town Of Port Louis-Ward 1 West",
      prev_district_name == "Town Of Quatre-Bornes-Ward 3 West" 
      ~"Town Of Quatre-Bornes-Ward 3",
      prev_district_name == "Town Of Vacoas/Phoenix-Ward 4 East" 
      ~"Town Of Vac/Phoenix-Ward 4 East",
      prev_district_name == "Town Of Vacoas/Phoenix-Ward 4 West" 
      ~"Town Of Vacoas/Phoenix-Ward 4",
      prev_district_name == "Villebague East" ~"Ville Bague East",
      prev_district_name == "Unknown" & prev_region_name == "Black River" 
      ~"Black River District, Unknown Municipality",
      # these are all in the same region for which there is one district
      str_sub(prev_district_name, 1, 4) == "Zone" ~"Baie Aux Huitres",
      prev_district_name == "Rodrigues District, Unknown Municipality" 
      ~"Baie Aux Huitres",
      prev_district_name == "Niu" ~"Niu (Not In Universe)",
      TRUE ~prev_district_name)) %>%
  combine_pd() %>%
  filter(str_detect(district_name, "Unknown Municipality") == FALSE) %>%
  urban_status(2000)


# create codes
district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  full_join(ctry_df %>% select(prev_district_name, prev_region_name) %>% 
              unique()) %>%
  code_prev_dist(10000, 10001, unk_r = 100)

ctry_df %<>% full_join(district) %>%
  left_join(prev_district)

mus00_df <- ctry_df %>% filter(year == 2000)
mus11_df <- ctry_df %>% filter(year == 2011)

saveRDS(mus00_df, here(build.dir, ctries_dir, cname, cens_dir, "census00.rds"))
saveRDS(mus11_df, here(build.dir, ctries_dir, cname, cens_dir, "census11.rds"))

rm(cname, ctry_df, d, pd, r, pr, pc, district, prev_district)
rm(mus00_df, mus11_df)

#*******************************************************************************
# Mozambique ----
#*******************************************************************************

cname <- "MOZ"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent97_07.dta")
  )

d <- geo_labels(ctry_df, "geo2_mz", district_name) 
r <- geo_labels(ctry_df, "geo1_mz", region_name)

pd <- geo_labels(ctry_df, "mig2_5_mz", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_5_mz", prev_region_name) 

bd <- geo_labels(ctry_df, "bplmz2", birth_district_name)
br <- geo_labels(ctry_df, "bplmz1", birth_region_name) 
#pc <- geo_labels(ctry_df, "migctry5", prev_country_name)
pc1 <- geo_labels(ctry_df, "migctry1", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, bd, br, pc1) %>%
  mutate(
    across(contains("district"), ~case_when(
      substr(.x, 1, 15) == "Distrito Urbano"~ "Maputo City",
      .x == "Cidade De Maputo" ~"Maputo City",
      # previous district combinations
      .x %in% c("Morrumbene", "Maxixe")~  "Morrumbene, Maxixe",
      .x %in% c("Nacala-Porto", "Nacala-Velha") ~"Nacala-Porto, Nacala-Velha",
      substr(.x,1,8) == "Zambezia" ~"Zambezia Province, Unknown District",
      .x == "Niu" ~"Niu (Not In Universe)",
      .x == "Mocamboa Da Praia" ~"Mocimboa Da Praia",
      TRUE~.x)) ) %>%
  combine_bl() %>%
  urban_status(1997)

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name, prev_region_name) %>% 
              unique() ) %>%
  code_prev_dist(100, 10001, unk_r = 100)

birth_district <- prev_district %>% rename_with(~sub("prev_", "birth_", .x))

ctry_df %<>% full_join(district) %>%
  left_join(prev_district) %>%
  left_join(birth_district)

id_cons <- ctry_df %>% select(district_name, geo2_mz) %>% distinct() %>%
  group_by(district_name) %>%
  mutate(id_cons = min(geo2_mz)) %>%
  ungroup() %>%
  select(-district_name)

ctry_df <- full_join(ctry_df, id_cons) %>%
  mutate(geo2_mz = id_cons) %>%
  select(-id_cons) %>%
  left_join(id_cons %>% rename(mig2_1_mz = geo2_mz)) %>%
  mutate(mig2_1_mz = ifelse(is.na(id_cons) == FALSE, id_cons, mig2_1_mz)) %>%
  select(-id_cons) %>%
  left_join(id_cons %>% rename(mig2_5_mz = geo2_mz)) %>%
  mutate(mig2_5_mz = ifelse(is.na(id_cons) == FALSE, id_cons, mig2_5_mz)) %>%
  select(-id_cons)

moz97_df <- ctry_df %>% filter(year == 1997)
moz07_df <- ctry_df %>% filter(year == 2007)

saveRDS(moz97_df, here(build.dir, ctries_dir, cname, cens_dir, "census97.rds"))
saveRDS(moz07_df, here(build.dir, ctries_dir, cname, cens_dir, "census07.rds"))

rm(cname, ctry_df, d, pd, r, pr, br, bd, pc1, district, prev_district,
   birth_district, id_cons)
rm(moz97_df, moz07_df)

#*******************************************************************************
# Senegal ----
#* SEN - 5 years ago and birth district
#*******************************************************************************

cname <- "SEN"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir,"consistent02_13.dta")
  )

d <- geo_labels(ctry_df, "geo2_sn", district_name)
r <- geo_labels(ctry_df, "geo1_sn", region_name)

pd <- geo_labels(ctry_df, "mig2_5_sn", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_5_sn", prev_region_name)
bd <- geo_labels(ctry_df, "bplsn", birth_district_name)
pc <- geo_labels(ctry_df, "migctry5", prev_country_name)

ctry_df <- clean_census(ctry_df, "sn2002a_occ3", d, r, pd, pr, bd, pc) %>%
  mutate(
    birth_district_name = gsub("-", " ", birth_district_name),
    birth_district_name = case_when(
      birth_district_name == "Mbour" ~"M'bour",
      birth_district_name == "Mbacke" ~"M'backe",
      birth_district_name == "Tivaoune" ~"Tivaouane",
      birth_district_name == "Foreign Country" ~"Abroad",
      birth_district_name %in% 
        c("Kaolack Region, Unknown Department", 
          "Fatick Region, Unknown Department",
          "Kaffrine Region, Unknown Department")
      ~"Kaolack, Fatick, And Kaffrine Regions, Unknown Department",
      birth_district_name %in% 
        c("Saint Louis Region, Unknown Department", 
          "Louga Region, Unknown Department",
          "Matam Region, Unknown Department")
      ~"Saint Louis, Louga, And Matam Regions, Unknown Department",
      birth_district_name %in% 
        c("Tambacounda Region, Unknown Department", 
          "Kedougou Region, Unknown Department")
      ~"Tambacounda And Kedougou Regions, Unknown Department",
      birth_district_name %in% 
        c("Kolda Region, Unknown Department", 
          "Sedhiou Region, Unknown Department")
      ~"Kolda And Sedihou Regions, Unknown Department",
      TRUE ~birth_district_name)) %>%
  combine_bl() %>%
  urban_status(2002)


# create region and district codes
district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name, prev_region_name) %>% 
              unique() ) %>%
  code_prev_dist(100, 1001, unk_r = 10)

birth_district <- prev_district %>% rename_with(~sub("prev_", "birth_", .x)) %>%
  full_join(ctry_df %>% select(birth_district_name) %>% unique()) %>%
  mutate(birth_region_name = ifelse(birth_district_name == "Ferlo", "Ferlo", 
                                    birth_region_name),
         birth_region = ifelse(birth_district_name == "Ferlo", 9, birth_region),
         birth_district = ifelse(birth_district_name == "Ferlo", 91, 
                                 birth_district))
  
ctry_df %<>% full_join(district) %>%
  left_join(prev_district) %>%
  left_join(birth_district)

sen02_df <- ctry_df %>% filter(year == 2002)
sen13_df <- ctry_df %>% filter(year == 2013)

saveRDS(sen02_df, here(build.dir, ctries_dir, cname, cens_dir, "census02.rds"))
saveRDS(sen13_df, here(build.dir, ctries_dir, cname, cens_dir, "census13.rds"))

rm(cname, ctry_df, d, r, pd, pr, bd, pc, district, prev_district, birth_district)
rm(sen02_df, sen13_df)

#*******************************************************************************
# Rwanda ----
#*******************************************************************************

cname <- "RWA"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent12.dta")
  ) %>%
  rename(duration = rw2012a_resdur)

d <- geo_labels(ctry_df, "geo2_rw", district_name) 
r <- geo_labels(ctry_df, "geo1_rw2012", region_name) 

pd <- geo_labels(ctry_df, "migrw2", prev_district_name)
pr <- geo_labels(ctry_df, "mig1alt_p_rw", prev_region_name)
pc <- geo_labels(ctry_df, "migctryp", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, pc, dur = TRUE) %>%
  urban_status(2012) %>%
  mutate(
    across(contains("name"), ~ifelse(str_sub(.x, 1, 3) == "Niu",
                                     "Niu (Not In Universe)", .x)),
    prev_district_name = case_when(
      prev_district_name == "Gakenye" ~ "Gakenke",
      prev_district_name == "Nyarugenge" ~ "Nyarugenge District",
      TRUE ~ prev_district_name
    ))

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name, prev_region_name) %>% 
              distinct() ) %>%
  code_prev_dist(100, 101) %>%
  distinct()

rwa12_df <- ctry_df %>% full_join(district) %>%
  left_join(prev_district) %>%
  rename(geo1_rw = geo1_rw2012)

saveRDS(rwa12_df, here(build.dir, ctries_dir, cname, cens_dir, "census12.rds"))

rm(cname, ctry_df, d, r, pd, pr, pc, district, prev_district)
rm(rwa12_df)

#*******************************************************************************
# Sierra Leone ----
#*******************************************************************************

cname <- "SLE"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent04_15.dta")
  )

d <- geo_labels(ctry_df, "geo2_sl", district_name) 
r <- geo_labels(ctry_df, "geo1_sl", region_name) 

pd <- geo_labels(ctry_df, "mig2_5_sl", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_5_sl", prev_region_name)
pd1 <- geo_labels(ctry_df, "mig2_14_sl", prev_district_name1)
pr1 <- geo_labels(ctry_df, "mig1_14_sl", prev_region_name1)
bd <- geo_labels(ctry_df, "bplsl", birth_district_name)
pc <- geo_labels(ctry_df, "migctry5", prev_country_name)

ctry_df <- clean_census(ctry_df, "sl2004a_ind", d, r, pd, pr, pd1, pr1, bd, pc) %>%
  mutate(
    across(c(prev_district_name, prev_region_name),
           ~ifelse(is.na(.x) == TRUE, get(paste0(cur_column(),"1")), .x ))) %>%
  select(-prev_district_name1, -prev_region_name1) %>%
  combine_bl() %>%
  urban_status(2004)

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name) %>% unique() ) %>%
  code_prev_dist(100, 10001) 

birth_district <- prev_district %>% rename_with(~sub("prev_", "birth_", .x)) 

ctry_df %<>% full_join(district) %>%
  left_join(prev_district) %>%
  left_join(birth_district)

sle04_df <- ctry_df %>% filter(year == 2004)
sle15_df <- ctry_df %>% filter(year == 2015)

saveRDS(sle04_df, here(build.dir, ctries_dir, cname, cens_dir, "census04.rds"))
saveRDS(sle15_df, here(build.dir, ctries_dir, cname, cens_dir, "census15.rds"))

rm(cname, ctry_df, d, r, pd, pd1, pr, pr1, bd, pc, district, prev_district, 
   birth_district)
rm(sle04_df, sle15_df)

#*******************************************************************************
# Sudan & S. Sudan ----
#* The census happened in 08 before independence so it is as one country
#* and I will use Sudan as if it were a single country. 
#*******************************************************************************

cname1 <- "SSD"
ssd_df <- read_dta(
  here(raw.dir, ctries_dir, cname1, cens_dir, "consistent_08.dta")
  ) %>%
  rename(duration = migyrs1)

d <- geo_labels(ssd_df, "geo2_ss", district_name) 
r <- geo_labels(ssd_df, "geo1_ss", region_name) 

pr <- geo_labels(ssd_df, "mig1_1_ss", prev_region_name)

ssd_df <- clean_census(ssd_df, "indgen", d, r, pr, dur = TRUE) %>%
  urban_status(2008) 

cname2 <- "SDN"
sdn_df <- read_dta(
  here(raw.dir, ctries_dir, cname2, cens_dir, "consistent_08.dta")
  ) %>% 
  rename(duration = migyrs1)

d <- geo_labels(sdn_df, "geo2_sd", district_name) 
r <- geo_labels(sdn_df, "geo1_sd", region_name) 

pr <- geo_labels(sdn_df, "mig1_1_sd", prev_region_name)

sdn_df <- clean_census(sdn_df, "indgen", d, r, pr, dur = TRUE) %>%
  urban_status(2008) 

ctry_df <- bind_rows(
  ssd_df %>% mutate(country_name = "South Sudan"), 
  sdn_df %>% mutate(country_name = "Sudan")) %>%
  mutate(geo1_sd = ifelse(country_name == "Sudan", geo1_sd, geo1_ss),
         geo2_sd = ifelse(country_name == "Sudan", geo2_sd, geo2_ss)) %>%
  select(-c(geo1_ss, geo2_ss))

district <- district_codes(ctry_df)

prev_region <- district %>% select(contains("region")) %>% unique() %>%
  rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_region_name) %>% 
              unique() ) %>%
  mutate(
    prev_region = case_when(
      prev_region_name == "Abroad"~ 100,
      prev_region_name == "Niu (Not In Universe)"~ 98,
      prev_region_name == "Unknown"~ 99,
      TRUE ~ as.numeric(prev_region)
    )
  ) %>%
  unique()

ctry_df %<>% full_join(district) %>%
  left_join(prev_region) 

ssd08_df <- ctry_df %>% filter(country_name == "South Sudan")
sdn08_df <- ctry_df %>% filter(country_name == "Sudan")

saveRDS(ssd08_df, here(build.dir, ctries_dir, cname1, cens_dir, "census08.rds"))
saveRDS(sdn08_df, here(build.dir, ctries_dir, cname2, cens_dir, "census08_NS.rds"))
saveRDS(ctry_df, here(build.dir, ctries_dir, cname2, cens_dir, "census08.rds"))

rm(cname1, cname2, d, r, pr, district, prev_region)
rm( ctry_df, ssd08_df, sdn08_df, sdn_df, ssd_df)

#*******************************************************************************
# Tanzania ----
#*******************************************************************************

cname <- "TZA"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent02_12.dta")
  )

r <- geo_labels(ctry_df, "geo1_tz", region_name)
pr <- geo_labels(ctry_df, "mig1_1_tz", prev_region_name) 
br <- geo_labels(ctry_df, "bpltz", birth_region_name) 
pc <- geo_labels(ctry_df, "migctry1", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", r, pr, br, pc) %>%
  mutate(
    op = str_locate(birth_region_name, "\\("),
    op1 = op[,1],
    birth_region_name = ifelse(is.na(op1) == FALSE & op1 > 5,
                               str_sub(birth_region_name, op1 + 1, -2),
                               birth_region_name) %>%
      ifelse(. == "Ruvumba", "Ruvuma", .),
    ) %>%
  select(-starts_with("op")) %>%
  combine_br() %>%
  urban_status(2002, geo = region_name)


# create region codes
region <- ctry_df %>% select(region_name) %>% unique() %>%
  arrange(region_name) %>%
  mutate(region = row_number())

prev_region <- region %>% rename_with(~paste0("prev_", .x)) %>%
  full_join(ctry_df %>% select(prev_region_name) %>% unique()) %>%
  mutate(
    prev_region = case_when(
      prev_region_name == "Abroad"~ 100,
      prev_region_name == "Niu (Not In Universe)"~ 98,
      prev_region_name == "Unknown"~ 99,
      TRUE ~ as.numeric(prev_region)
    )
  )

birth_region <- prev_region %>% rename_with(~sub("prev_", "birth_", .x)) 

ctry_df %<>% full_join(region) %>%
  left_join(prev_region) %>%
  left_join(birth_region)

tza02_df <- ctry_df %>% filter(year == 2002)
tza12_df <- ctry_df %>% filter(year == 2012)

saveRDS(tza02_df, here(build.dir, ctries_dir, cname, cens_dir, "census02.rds"))
saveRDS(tza12_df, here(build.dir, ctries_dir, cname, cens_dir, "census12.rds"))

rm(cname, ctry_df, r, pr, br, pc, region, prev_region, birth_region)
rm(tza02_df, tza12_df)

#*******************************************************************************
# Togo ----
#*******************************************************************************

cname <- "TGO"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent_10.dta")
  ) %>%
  rename(duration = migyrs1)

d <- geo_labels(ctry_df, "geo2_tg", district_name) 
r <- geo_labels(ctry_df, "geo1_tg", region_name) 

pd <- geo_labels(ctry_df, "mig2_p_tg", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_p_tg", prev_region_name)
pc <- geo_labels(ctry_df, "migctryp", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, pc, dur = TRUE) %>%
  urban_status(2010)

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name, prev_region_name) %>% 
              unique() ) %>%
  code_prev_dist(10, 1001) %>%
  mutate(
    prev_district = case_when(
      prev_district_name == "Region Maritime, Lome Unknown Prefecture" ~ 100,
      prev_district_name == "Region Plateaux, Centrale, Kara Unknown Prefecture" ~ 200,
      prev_district_name == "Region Savanes Unknown Prefecture" ~ 300,
      TRUE ~ prev_district
    ),
    prev_region = case_when(
      prev_district_name == "Region Maritime, Lome Unknown Prefecture" ~ 1,
      prev_district_name == "Region Plateaux, Centrale, Kara Unknown Prefecture" ~ 2,
      prev_district_name == "Region Savanes Unknown Prefecture" ~ 3,
      TRUE ~ prev_region
    )
  )

tgo10_df <- ctry_df %>% full_join(district) %>%
  left_join(prev_district) 

saveRDS(tgo10_df, here(build.dir, ctries_dir, cname, cens_dir, "census10.rds"))

rm(cname, ctry_df, d, r, pd, pr, pc, district, prev_district)
rm(tgo10_df)

#*******************************************************************************
# Uganda ----
#* district of birth only in 2002
#*******************************************************************************

cname <- "UGA"
ctry_df <-  read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent02_14.dta")
  ) %>%
  rename(duration = migyrs1)

r <- geo_labels(ctry_df, "geo1_ug", region_name)
#r <- geo_labels(ctry_df, "regnug", region_name) 

pr <- geo_labels(ctry_df, "mig1_p_ug", prev_region_name)
br <- geo_labels(ctry_df, "bplug", birth_region_name)
pc <- geo_labels(ctry_df, "migctryp", prev_country_name)

ctry_df <- clean_census(ctry_df, "ug2002a_occ", r, pr, br, pc, dur = TRUE) %>%
  mutate(id_base = str_sub(as.character(geo1_ug),1, -3),
         bpl_sfx = as.character(bplug) %>%
           ifelse(str_length(.) == 1, paste0("0", .), .),
         bpl1_ug = paste0(id_base, bpl_sfx),
    birth_region_name = ifelse(birth_region_name == "Sembabule",
                                 "Ssembabule", birth_region_name)
    ) %>%
  combine_br() %>%
  urban_status(2002, geo = region_name) %>%
  select(-id_base, -bpl_sfx)

# create region codes
region <- ctry_df %>% select(region_name) %>% unique() %>%
  arrange(region_name) %>%
  mutate(region = row_number())

prev_region <- region %>% rename_with(~paste0("prev_", .x)) %>%
  full_join(ctry_df %>% select(prev_region_name) %>% distinct()) %>%
  mutate(
    prev_region = case_when(
      prev_region_name == "Abroad"~ 100,
      prev_region_name == "Visitor"~ 97,
      prev_region_name == "Unknown"~ 99,
      TRUE ~ as.numeric(prev_region)
    )
  )

birth_region <- region %>% rename_with(~paste0("birth_", .x)) %>%
  full_join(ctry_df %>% select(birth_region_name) %>% distinct()) %>%
  mutate(
    birth_region = case_when(
      birth_region_name == "Abroad"~ 100,
      birth_region_name == "Response Suppressed"~ 96,
      birth_region_name == "Unknown"~ 99,
      TRUE ~ as.numeric(birth_region)
    )
  )

ctry_df %<>% full_join(region) %>%
  left_join(prev_region) %>%
  left_join(birth_region)

uga02_df <- ctry_df %>% filter(year == 2002)
uga14_df <- ctry_df %>% filter(year == 2014)

saveRDS(uga02_df, here(build.dir, ctries_dir, cname, cens_dir, "census02.rds"))
saveRDS(uga14_df, here(build.dir, ctries_dir, cname, cens_dir, "census14.rds"))

rm(cname, ctry_df, r, pr, br, pc, region, prev_region, birth_region)
rm(uga02_df, uga14_df)

#*******************************************************************************
# Zambia ----
#*******************************************************************************

cname <- "ZMB"
ctry_df <-  read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent00_10.dta")
  )

d <- geo_labels(ctry_df, "geo2_zm", district_name) 
r <- geo_labels(ctry_df, "geo1_zm", region_name) 

pd <- geo_labels(ctry_df, "mig2_1_zm", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_1_zm", prev_region_name)
bd <- geo_labels(ctry_df, "bplzm", birth_district_name)
pc <- geo_labels(ctry_df, "migctry1", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, bd, pc) %>%
  mutate(
    birth_district_name = case_when(
      birth_district_name == "Itezhi-Tezhi" ~"Itezhi Tezhi",
      birth_district_name == "Kapiri-Posh"  ~"Kapiri Mposhi", 
      birth_district_name == "Mufumbwe" ~ "Mufumbwe (Chizera)",
      TRUE ~ birth_district_name)) %>%
  combine_bl() %>%
  urban_status(2000)

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name, prev_region_name) %>% 
              unique() ) %>%
  code_prev_dist(100, 1001)

birth_district <- prev_district %>% rename_with(~sub("prev_", "birth_", .x)) %>%
  full_join(ctry_df %>% select(birth_district_name) %>% 
              unique() ) %>%
  mutate(birth_region_name = ifelse(
    is.na(birth_region_name) == TRUE,
    sub("Unknown District In ", "", birth_district_name),
    birth_region_name),
    birth_region_name = sub(" Province", "", birth_region_name),
    birth_region_name = case_when(
      birth_region_name %in% c("Eastern", "Muchinga", "Northern")
      ~"Eastern, Muchinga, Northern",
      birth_region_name == "North-Western" ~ "North Western",
      birth_region_name == "Luapala" ~ "Luapula",
      birth_district_name == "Unknown (Zambia)" ~ "Unknown",
      TRUE~birth_region_name)) %>%
  group_by(birth_region_name) %>%
  mutate(prev_r_mean = mean(birth_region, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    birth_region = ifelse(is.na(birth_region), prev_r_mean, birth_region),
    birth_district = ifelse(is.na(birth_district), 100*prev_r_mean, 
                            birth_district)
    ) %>%
  select(-prev_r_mean)

ctry_df %<>% full_join(district) %>%
  left_join(prev_district) %>%
  left_join(birth_district)

zmb00_df <- ctry_df %>% filter(year == 2000)
zmb10_df <- ctry_df %>% filter(year == 2010)

saveRDS(zmb00_df, here(build.dir, ctries_dir, cname, cens_dir, "census00.rds"))
saveRDS(zmb10_df, here(build.dir, ctries_dir, cname, cens_dir, "census10.rds"))

rm(cname, ctry_df, d, r, pd, pr, bd, pc, district, prev_district, birth_district)
rm(zmb00_df, zmb10_df)

#*******************************************************************************
# Zimbabwe ----
#*******************************************************************************

cname <- "ZWE"
ctry_df <-  read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent_12.dta")
  ) 

# clean labeling of two districts that have the wrong number
ctry_df <- ctry_df %>% 
  mutate(
    mig2_10_zw = case_when(
      geo2_zw %in% c("716003023", "716007025") ~ 
        as.numeric(
          paste0(
            "7160",
            sprintf("%02d", zw2012a_res10yr),
            sprintf(
              "%03d",
              as.numeric(substr(sprintf("%03d", zw2012a_res10yr3), 2, 3))
            )
          )
        ),
      TRUE ~ mig2_10_zw
    )
  )

d <- geo_labels(ctry_df, "geo2_zw", district_name) 
r <- geo_labels(ctry_df, "geo1_zw", region_name) 

pd <- geo_labels(ctry_df, "mig2_10_zw", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_10_zw", prev_region_name)

ctry_df <- clean_census(ctry_df, "occ", d, r, pd, pr) %>%
  urban_status(2012)

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name, prev_region_name) %>% 
              unique() ) %>%
  code_prev_dist(100, 10001) %>%
  unique()

zwe12_df <- ctry_df %>% full_join(district) %>%
  left_join(prev_district) 

saveRDS(zwe12_df, here(build.dir, ctries_dir, cname, cens_dir, "census12.rds"))

rm(cname, ctry_df, d, r, pd, pr, district, prev_district)
rm(zwe12_df)


#*******************************************************************************
# South Africa ----
#*******************************************************************************
# ZAF - has two files that are needed later as we're not using those exact files

cname <- "ZAF"

ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent01_16.dta")
  ) %>% 
  rename(duration = migyrs2)
table(ctry_df$urban, ctry_df$year, useNA = 'ifany')
d <- geo_labels(ctry_df, "geo2_za", district_name)
r <- geo_labels(ctry_df, "geo1_za", region_name)

pd <- geo_labels(ctry_df, "mig2_p_za", prev_district_name)
pr <- geo_labels(ctry_df, "mig1_p_za", prev_region_name)
pr1 <- geo_labels(ctry_df, "mig1_5_za", prev_region1_name)
br <- geo_labels(ctry_df, "bplza", birth_region_name)
#pc <- geo_labels(ctry_df, "migctryp", prev_country_name)

ctry_df <- clean_census(ctry_df, "indgen", d, r, pd, pr, pr1, br, dur = TRUE) %>%
  mutate(
    prev_region_name = ifelse(is.na(prev_region_name) == TRUE,
                              prev_region1_name,
                              prev_region_name)) %>%
  select(-prev_region1_name) %>%
  filter(district_name != "Unknown") %>%
  combine_br() %>%
  mutate(
    across(contains("region_name"),
           ~ifelse(.x == "Eastern Cape, Kwazulu-Natal",
                   "Kwazulu-Natal, Eastern Cape", .x)),
    prev_region_name = ifelse(prev_region_name == "Niu (Not In Universe)",
    birth_region_name, prev_region_name)) %>%
  urban_status(2011)

district <- district_codes(ctry_df)

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  # create codes for abroad and people who only know the region
  full_join(ctry_df %>% select(prev_district_name) %>% unique() ) %>%
  code_prev_dist(10, 101) %>%
  drop_na()

prev_region <- prev_district %>% 
  select(contains("region")) %>% unique() %>%
  full_join(ctry_df %>% select(prev_region_name) %>% unique() )

birth_region <- prev_district %>% rename_with(~sub("prev_", "birth_", .x)) %>%
  select(contains("region")) %>% unique() %>%
  full_join(ctry_df %>% select(birth_region_name) %>% unique() )

ctry_df %<>% full_join(district) %>%
  left_join(prev_region) %>%
  left_join(birth_region) %>%
  left_join(prev_district)
  

zaf01_df <- ctry_df %>% filter(year == 2001)
zaf07_df <- ctry_df %>% filter(year == 2007)
zaf11_df <- ctry_df %>% filter(year == 2011)
zaf16_df <- ctry_df %>% filter(year == 2016)

#saveRDS( zaf01_df, paste0(build.dir, cname, "/Census/census01.rds"))
saveRDS(zaf07_df, here(build.dir, ctries_dir, cname, cens_dir, "census07.rds"))
saveRDS(zaf11_df, here(build.dir, ctries_dir, cname, cens_dir, "census11-1.rds"))
saveRDS(zaf16_df, here(build.dir, ctries_dir, cname, cens_dir, "census16.rds"))

rm(cname, ctry_df, d, r, pd, pr, pr1, br, district, prev_district, birth_region)
rm(zaf01_df, zaf07_df, zaf11_df, zaf16_df)


# Matching ----

# dist_df <- readRDS( paste0(build.dir, cname, "/Support/cons_l2_dist.rds")) %>%
#   select(district_name) %>% unique() %>% mutate(dist = 1)
# 
# fl_df <- readRDS( paste0(build.dir,cname, "/Forest Cover/l2_loss30c.rds")) %>%
#   select(district_name) %>% unique() %>% mutate(tree = 1)
# 
# cens_df <- ctry_df %>% select(district_name) %>% unique() %>% mutate(curr = 1)
# 
# prev_df <- ctry_df %>% select(prev_district_name) %>% unique() %>%
#   mutate(prev = 1) %>% rename(district_name = prev_district_name)
# 
# birth_df <- ctry_df %>% select(birth_district_name) %>% unique() %>%
#   mutate(birth = 1) %>% rename(district_name = birth_district_name)
# 
# m <- full_join(cens_df, prev_df)
# m <- full_join(cens_df, birth_df)
# m <- full_join(cens_df, dist_df)
# m <- full_join(cens_df, fl_df)
# 
# ## region ---- 
# 
# dist_df <- readRDS( paste0(build.dir, cname, "/Support/cons_l1_dist.rds")) %>%
#   select(region_name) %>% unique() %>% mutate(dist = 1)
# 
# fl_df <- readRDS( paste0(build.dir,cname, "/Forest Cover/l1_loss30c.rds")) %>%
#   select(region_name) %>% unique() %>% mutate(tree = 1)
# 
# cens_df <- ctry_df %>% select(region_name) %>% unique() %>% mutate(curr = 1)
# 
# prev_df <- ctry_df %>% select(prev_region_name) %>% unique() %>%
#   mutate(prev = 1) %>% rename(region_name = prev_region_name)
# 
# birth_df <- ctry_df %>% select(birth_region_name) %>% unique() %>%
#   mutate(birth = 1) %>% rename(region_name = birth_region_name)
# 
# m <- full_join(cens_df, prev_df)
# m <- full_join(cens_df, birth_df)
# m <- full_join(cens_df, dist_df)
# m <- full_join(cens_df, fl_df)

#*******************************************************************************
# Madagascar ----
#*******************************************************************************
cname <- "MDG"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, cens_dir, "consistent18.dta")
)   
r <- geo_labels(ctry_df, "geo1_mg", region_name) 
pr <- geo_labels(ctry_df, "mig1_10_mg", prev_region_name)
br <- geo_labels(ctry_df, "bplmg1", birth_region_name)
d <- geo_labels(ctry_df, "geo2_mg", district_name) 
pd <- geo_labels(ctry_df, "mig2_10_mg", prev_district_name)
bd <- geo_labels(ctry_df, "bplmg2", birth_district_name)

ctry_df <- clean_census(ctry_df, "indgen", r, pr, br, d, pd, bd) %>% 
  urban_status(2018, geo = district_name) %>% 
  mutate(across(contains("name"), 
                ~ifelse(str_sub(.x, 1, 3) == "Niu","Niu (Not In Universe)", .x)))

region <- ctry_df %>% 
  select(region_name) %>%
  unique() %>%
  arrange(region_name) %>%
  mutate(region = row_number())

district <- ctry_df %>% 
  select(district_name) %>%
  unique() %>%
  arrange(district_name) %>%
  mutate(district = row_number())

prev_region <- region %>% rename_with(~paste0("prev_", .x)) %>%
  full_join(ctry_df %>% select(prev_region_name) %>% distinct()) 

prev_district <- district %>% rename_with(~paste0("prev_", .x)) %>%
  full_join(ctry_df %>% select(prev_district_name) %>% distinct()) 

birth_region <- region %>% rename_with(~paste0("birth_", .x)) %>%
  full_join(ctry_df %>% select(birth_region_name) %>% distinct()) %>%
  mutate(
    birth_region = case_when(
      birth_region_name == "Abroad"~ 100,
      birth_region_name == "Visitor"~ 97,
      birth_region_name == "Unknown"~ 99,
      TRUE ~ as.numeric(birth_region)
    )
  )

birth_district <- district %>% rename_with(~paste0("birth_", .x)) %>%
  full_join(ctry_df %>% select(birth_district_name) %>% distinct()) %>%
  mutate(
    birth_district = case_when(
      birth_district_name == "Abroad"~ 100,
      birth_district_name == "Visitor"~ 97,
      birth_district_name == "Unknown"~ 99,
      TRUE ~ as.numeric(birth_district)
    )
  )

mdg18 <- ctry_df %<>% 
  full_join(region) %>%
  left_join(prev_region) %>% 
  left_join(birth_region) %>% 
  left_join(district) %>% 
  left_join(prev_district) %>% 
  left_join(birth_district)

saveRDS(mdg18, here(build.dir, ctries_dir, cname, cens_dir, "census18.rds")) 
# not actually a census... naming as some functions need it
# excluding those is better handled by excluding country names

rm(cname, ctry_df, r, pr, br, region, prev_region, birth_region)
rm(mdg18)

#*******************************************************************************
#*From the LSMS -------------
#*processing of LSMS into 'census style' df in Build/Census/Clean non-IPUMS
# note that the final dfs of ethiopia and nigeria are called census(yr).rds 
# calling it like this as it is called with the name census after 
# e.g., in Final/Population.R
# although this is NOT actually based on the census, of course

#*******************************************************************************
# Ethiopia ----
#*******************************************************************************
cname <- "ETH"
ctry_df <- readRDS(
  here(raw.dir, ctries_dir, cname, "consistent14.rds")
)   

r <- geo_labels(ctry_df, "geo1_et", region_name) 

ctry_df <- clean_census(ctry_df, "indgen", r) %>%
  urban_status(2016, geo = region_name) %>% 
  mutate(across(contains("name"), 
                ~ifelse(str_sub(.x, 1, 3) == "Niu","Niu (Not In Universe)", .x)))

region <- ctry_df %>% 
  transmute(
    region_name, 
    region = as.integer(str_sub(geo1_et, start = -3, end = -1))
  ) %>% 
  unique()

eth14 <- ctry_df %<>% 
  full_join(region) 

saveRDS(eth14, here(build.dir, ctries_dir, cname, "census14.rds"))

rm(cname, ctry_df, r, region)
rm(eth14)

#*******************************************************************************
# Nigeria ----
#*******************************************************************************

cname <- "NGA"
ctry_df <- read_dta(
  here(raw.dir, ctries_dir, cname, "consistent12.dta")
)   
r <- geo_labels(ctry_df, "geo1_ng", region_name) 

ctry_df <- clean_census(ctry_df, "indgen", r) %>% 
  urban_status(2012, geo = region_name) %>% 
  mutate(across(contains("name"), 
                ~ifelse(str_sub(.x, 1, 3) == "Niu","Niu (Not In Universe)", .x)))

region <- ctry_df %>% 
  select(region_name) %>%
  unique() %>%
  arrange(region_name) %>%
  mutate(region = row_number())

nga12 <- ctry_df %<>% 
  full_join(region)

saveRDS(nga12, here(build.dir, ctries_dir, cname, cens_dir, "census12.rds")) 

rm(cname, ctry_df, r, region)
rm(nga12)

