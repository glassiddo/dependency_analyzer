#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    Dec 6, 2022
#* Title:   Build Nightlight datasets for each country
#* Desc:    
#* NB:      Years = 1992-2013
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

l1_nl_df <- readRDS(paste0(build.dir, "Africa/Night Light/l1_nightlight.rds"))
l2_nl_df <- readRDS(paste0(build.dir, "Africa/Night Light/l2_nightlight.rds"))

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
# Cameroon ----
c_name <- n_ctry("CMR")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

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
# Guinea----
c_name <- n_ctry("GIN")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

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
# Malawi ----
c_name <- n_ctry("MWI")
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
  ren_moz(lake = FALSE) %>% moz_id(district_name, geolevel2) %>%
  group_by(country, geolevel2, district_name, year) %>%
  summarise(
    ntlv = weighted.mean(ntlv, w = ntla, na.rm = TRUE),
    ntlvcc = weighted.mean(ntlvcc, w = ntlacc, na.rm = TRUE),
    ntlvh = weighted.mean(ntlvh, w = ntlah, na.rm = TRUE),
    nlvda = weighted.mean(nlvda, w = nlada, na.rm = TRUE),
    nlv = weighted.mean(nlv, w = nla, na.rm = TRUE),
    across(c(ntla, ntlacc, ntlah, nlada, nla), ~mean(.x)), .groups = 'drop')

saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))


# l2_df <- l2_nl_df %>% filter(country == c_name) %>%
#   filter(district_name %!in% c("Aeroporto", "Lake Malawi")) %>%
#   mutate(district_name = case_when(
#     district_name %in% c("Morrumbene", "Maxixe")~  "Morrumbene, Maxixe",
#     district_name %in% c("Nacala-Porto", "Nacala-Velha") 
#     ~"Nacala-Porto, Nacala-Velha",
#     substr(district_name, 1, 15) == "Distrito Urbano"~ "Maputo City",
#     TRUE~district_name)) %>%
#   group_by(district_name, year) %>%
#   #TODO to be precise I should be weighting by the size of each location
#   summarise(across(starts_with("ntl"), ~mean(.x)),
#             geolevel2 = min(geolevel2),
#             .groups = 'drop')
# saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Rwanda ----
c_name <- n_ctry("RWA")
l2_df <- l2_nl_df %>% filter(country == c_name)
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
# South Sudan ----
c_name <- n_ctry("SSD")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Sudan ----
c_name <- n_ctry("SDN")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight_NS.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight_NS.rds"))

## Sudan, S. Sudan ----
c_name1 <- n_ctry("SSD")
c_name2 <- n_ctry("SDN")

l2_df <- l2_nl_df %>% filter(country %in% c(c_name1, c_name2))
saveRDS(l2_df, paste0(build.dir, c_name2, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country %in% c(c_name1, c_name2))
saveRDS(l1_df, paste0(build.dir, c_name2, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Tanzania ----
c_name <- n_ctry("TZA")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Togo ----
c_name <- n_ctry("TGO")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Uganda ----
c_name <- n_ctry("UGA")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Zambia ----
c_name <- n_ctry("ZMB")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))

#*******************************************************************************
# Zimbabwe ----
c_name <- n_ctry("ZWE")
l2_df <- l2_nl_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_nightlight.rds"))

l1_df <- l1_nl_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_nightlight.rds"))


