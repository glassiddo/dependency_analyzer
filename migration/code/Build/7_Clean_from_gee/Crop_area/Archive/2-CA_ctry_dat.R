#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    Feb 2, 2023
#* Title:   Build crop area datasets for each country
#* Desc:    
#* NB:      Years: 2003, 2007, 2011, 2015, 2019
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

l1_ca_df <- readRDS(paste0(build.dir, "Africa/Crop Area/l1_croparea.rds"))
l2_ca_df <- readRDS(paste0(build.dir, "Africa/Crop Area/l2_croparea.rds"))

#*******************************************************************************
# Benin ----
c_name <- n_ctry("BEN")
l2_df <- l2_ca_df %>% filter(country == c_name) %>% ren_ben(lvl = 2)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name) %>% ren_ben(lvl = 1)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Botswana ----
c_name <- n_ctry("BWA")
l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Burkina Faso ----
c_name <- n_ctry("BFA")
l2_df <- l2_ca_df %>% filter(country == c_name)

# urb_df <- read_urb(c_name, 2)
# ca_df <- l2_df %>% group_by(district_name) %>% summarise(ntla = mean(ntla))
# match_df <- full_join(ca_df, urb_df) %>% arrange(district_name)

saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

# urb_df <- read_urb(c_name, 1)
# ca_df <- l1_df %>% group_by(region_name) %>% summarise(ntla = mean(ntla))
# match_df <- full_join(ca_df, urb_df)

#*******************************************************************************
# Cameroon ----
c_name <- n_ctry("CMR")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Egypt ----
c_name <- n_ctry("EGY")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Ghana ----
c_name <- n_ctry("GHA")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Guinea ----
c_name <- n_ctry("GIN")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Kenya ----
c_name <- n_ctry("KEN")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Lesotho ----
c_name <- n_ctry("LSO")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Malawi ----
c_name <- n_ctry("MWI")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Mali ----
c_name <- n_ctry("MLI")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Mauritius ----
c_name <- n_ctry("MUS")
l2_df <- l2_ca_df %>% filter(country == c_name) %>%
  filter(str_detect(district_name, "Unknown Municipality") == FALSE)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Mozambique ----
c_name <- n_ctry("MOZ")
l2_df <- l2_ca_df %>% filter(country == c_name) %>%
  ren_moz(lake = FALSE) %>% moz_id(district_name, geolevel2) %>%
  group_by(country, geolevel2, district_name, year) %>%
  summarise(sharecrop = weighted.mean(sharecrop, w = npix),
            npix = sum(npix),
            .groups = 'drop')
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

# l2_df <- l2_ca_df %>% filter(country == c_name) %>%
#   filter(district_name %!in% c("Aeroporto", "Lake Malawi")) %>%
#   mutate(district_name = case_when(
#     district_name %in% c("Morrumbene", "Maxixe")~  "Morrumbene, Maxixe",
#     district_name %in% c("Nacala-Porto", "Nacala-Velha") 
#     ~"Nacala-Porto, Nacala-Velha",
#     substr(district_name, 1, 15) == "Distrito Urbano"~ "Maputo City",
#     TRUE~district_name)) %>%
#   group_by(country, district_name, geolevel2, year) %>%
#   #TODO to be precise I should be weighting by the size of each location
#   summarise(sharecrop = weighted.mean(sharecrop, w = npix),
#             npix = sum(npix), 
#             .groups = 'drop')
# saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Rwanda ----
c_name <- n_ctry("RWA")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Senegal ----
c_name <- n_ctry("SEN")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Sierra Leone ----
c_name <- n_ctry("SLE")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))


#*******************************************************************************
# South Africa ----
c_name <- n_ctry("SAF")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# South Sudan ----
c_name <- n_ctry("SSD")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Sudan ----
c_name <- n_ctry("SDN")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea_NS.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea_NS.rds"))

# Sudan & S. Sudan ----
c_name1 <- n_ctry("SSD")
c_name2 <- n_ctry("SDN")

l2_df <- l2_ca_df %>% filter(country %in% c(c_name1, c_name2))
saveRDS(l2_df, paste0(build.dir, c_name2, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country %in% c(c_name1, c_name2))
saveRDS(l1_df, paste0(build.dir, c_name2, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Tanzania ----
c_name <- n_ctry("TZA")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Togo ----
c_name <- n_ctry("TGO")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Uganda ----
c_name <- n_ctry("UGA")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Zambia ----
c_name <- n_ctry("ZMB")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))

#*******************************************************************************
# Zimbabwe ----
c_name <- n_ctry("ZWE")
l2_df <- l2_ca_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_croparea.rds"))

l1_df <- l1_ca_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_croparea.rds"))


