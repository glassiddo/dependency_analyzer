# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Jan 19, 2022
# Title:   Build Forest Loss Panels
# Desc:    Create Forest Loss Panels, 5-year aggregates, and cover for each country
# Output:  
#*******************************************************************************
# Set Up ----
source("code/SSA_env_SetUp.R")
#source("code/Functions/F-Forest.R")

l1_loss30 <- readRDS(file.path(build.dir,"Forest Cover/lossbyyear30_l1.rds"))
l2_loss30 <- readRDS(file.path(build.dir,"Forest Cover/lossbyyear30_l2.rds"))

forestloss30_df <- readRDS(file.path(build.dir,"Forest Cover/lossbyyear30.rds"))

#*******************************************************************************
# Benin ----
c_name <- n_ctry("BEN")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code) %>% ren_ben(lvl = 2)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code) %>% ren_ben(lvl = 1)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#ben30_df <- get_country30("Benin")
#saveRDS( ben30_df, paste0(ben.dir, "Forest Cover/loss30c.rds"))

#*******************************************************************************
# Botswana ----
c_name <- n_ctry("BWA")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Burkina Faso ----
c_name <- n_ctry("BFA")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Cameroon ----
c_name <- n_ctry("CMR")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Egypt ----
c_name <- n_ctry("EGY")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Ghana ----
c_name <- n_ctry("GHA")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Guinea ----
c_name <- n_ctry("GIN")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Ivory Coast ----
c_name <- n_ctry("CIV")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Kenya ----
c_name <- n_ctry("KEN")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code) 
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code) 
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Lesotho ----
c_name <- n_ctry("LSO")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l1_df <- l1_loss30 %>% filter(cntry_code == c_code) 
saveRDS( l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

l2_df <- l2_loss30 %>% filter(cntry_code == c_code) 
saveRDS( l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

#*******************************************************************************
# Malawi ----
c_name <- n_ctry("MWI")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS( l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS( l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Mali ----
c_name <- n_ctry("MLI")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS( l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS( l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Mauritius ----
c_name <- n_ctry("MUS")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code) %>%
  filter(str_detect(district_name, "Unknown Municipality") == FALSE)
saveRDS( l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code) 
saveRDS( l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))


#*******************************************************************************
# Mozambique* ----
c_name <- n_ctry("MOZ")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code) %>%
  ren_moz(lake = FALSE) %>% moz_id(district_name, geolevel2) %>%
  group_by(country, cntry_code, geolevel2, district_name, year) %>%
  summarise(across(c(treecover, loss, treecover_simple, loss_simple),
                   ~sum(.x)), .groups = 'drop')

# l2_df <- l2_loss30 %>% filter(cntry_code == c_code) %>%
#   filter(district_name %!in% c("Aeroporto", "Lake Malawi")) %>%
#   mutate(district_name = case_when(
#     district_name %in% c("Morrumbene", "Maxixe")~  "Morrumbene, Maxixe",
#     district_name %in% c("Nacala-Porto", "Nacala-Velha") 
#     ~"Nacala-Porto, Nacala-Velha",
#     substr(district_name, 1, 15) == "Distrito Urbano"~ "Maputo City",
#     TRUE~district_name)) %>%
#   group_by(district_name, year) %>%
#   summarise(
#     across(c(treecover, treecover_simple, loss, loss_simple), ~sum(.x)), 
#     geolevel2 = min(geolevel2),
#     .groups = 'drop')

saveRDS( l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS( l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))


#*******************************************************************************
# Rwanda ----
c_name <- n_ctry("RWA")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Senegal ----
c_name <- n_ctry("SEN")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Sierra Leone ----

c_name <- n_ctry("SLE")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# South Africa ----
#*******************************************************************************

c_name <- n_ctry("SAF")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

# zaf30_df <- get_country30("South Africa") %>%
#   mutate(
#     district_name = sub(" District Municipality", "", district_name),
#     district_name = sub(" Metropolitan Municipality", "", district_name),
#     district_name = ifelse(district_name == "Sekhukhune", 
#                            "Greater Sekhukhune", district_name)) %>%
#   combine_districts(Country = "South Africa")
# 
# saveRDS(zaf30_df, paste0(build.dir, "South Africa/Forest Cover/loss30c.rds"))

# muni level N = 234
# IDK where this file went
# zaf30_df <- read_dta(
#   "data/Hansen Forest Loss/Output/lossbyyear30_SA.dta") %>%
#   remove_all_labels() %>%
#   loss_long(c(mn_name, mn_code, mn_mdb_c)) %>%
#   mutate(
#     pr_code = substr(mn_mdb_c, 1, 2),
#     mn_name = case_when(
#       mn_name == "//Khara Hais" ~"Khara Hais",
#       mn_name == "KhÃ¯Â¿Â½i-Ma" ~"Khâi-Ma",
#       mn_name == "Emalahleni" & pr_code == "EC" ~"Emalahleni-EC",
#       mn_name == "Emalahleni" & pr_code == "MP" ~"Emalahleni-MP",
#       mn_name == "Naledi" & pr_code == "FS" ~"Naledi-FS",
#       mn_name == "Naledi" & pr_code == "NW" ~"Naledi-NW",
#       TRUE~mn_name)) %>%
#   select(-mn_mdb_c, -pr_code)  %>%
#   rename(district_name = mn_name,
#          district = mn_code)
# 
# saveRDS(zaf30_df, paste0(build.dir, "South Africa/Forest Cover/l3_loss30c.rds"))

#*******************************************************************************
# South Sudan ----

c_name <- n_ctry("SSD")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Sudan & S. Sudan----
c_name1 <- n_ctry("SSD")
c_name2 <- n_ctry("SDN")

c_code1 <- ipums_country_codes[c_name1] %>% as.numeric()
c_code2 <- ipums_country_codes[c_name2] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code %in% c(c_code1, c_code2))
saveRDS(l2_df, paste0(build.dir, c_name2, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code %in% c(c_code1, c_code2))
saveRDS(l1_df, paste0(build.dir, c_name2, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Togo ----
c_name <- n_ctry("TGO")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Tanzania ----
c_name <- n_ctry("TZA")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Uganda* ----
c_name <- n_ctry("UGA")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Zambia ----
c_name <- n_ctry("ZMB")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

#*******************************************************************************
# Zimbabwe ----
c_name <- n_ctry("ZWE")
c_code <- ipums_country_codes[c_name] %>% as.numeric()

l2_df <- l2_loss30 %>% filter(cntry_code == c_code)
saveRDS(l2_df, paste0(build.dir, c_name, "/Forest Cover/l2_loss30c.rds"))

l1_df <- l1_loss30 %>% filter(cntry_code == c_code)
saveRDS(l1_df, paste0(build.dir, c_name, "/Forest Cover/l1_loss30c.rds"))

