#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    Feb 6, 2023
#* Title:   Build GAEZ datasets for each country
#* Desc:    
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

l1_gaez_df <- readRDS(paste0(build.dir, "Africa/GAEZ data/l1_GAEZ.rds"))
l2_gaez_df <- readRDS(paste0(build.dir, "Africa/GAEZ data/l2_GAEZ.rds")) %>%
  mutate(geolevel2 = as.character(geolevel2))

#*******************************************************************************
# Benin ----
c_name <- n_ctry("BEN")
l2_df <- l2_gaez_df %>% filter(country == c_name) %>% ren_ben(lvl = 2)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name) %>% ren_ben(lvl = 1)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

# create a DF with the data for each country we use at the most disagg level
samp_df <- l2_df

#*******************************************************************************
# Botswana ----
c_name <- n_ctry("BWA")
l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l1_df)

#*******************************************************************************
# Burkina Faso ----
c_name <- n_ctry("BFA")
l2_df <- l2_gaez_df %>% filter(country == c_name)

# urb_df <- read_urb(c_name, 2)
# ca_df <- l2_df %>% group_by(district_name) %>% summarise(ntla = mean(ntla))
# match_df <- full_join(ca_df, urb_df) %>% arrange(district_name)

saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l2_df)

# urb_df <- read_urb(c_name, 1)
# ca_df <- l1_df %>% group_by(region_name) %>% summarise(ntla = mean(ntla))
# match_df <- full_join(ca_df, urb_df)

#*******************************************************************************
# Egypt ----
c_name <- n_ctry("EGY")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

#*******************************************************************************
# Ghana ----
c_name <- n_ctry("GHA")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l2_df)

#*******************************************************************************
# Guinea ----
c_name <- n_ctry("GIN")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l1_df)

#*******************************************************************************
# Kenya ----
c_name <- n_ctry("KEN")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l2_df)

#*******************************************************************************
# Lesotho ----
c_name <- n_ctry("LSO")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l1_df)

#*******************************************************************************
# Mali ----
c_name <- n_ctry("MLI")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l2_df)

#*******************************************************************************
# Mauritius ----
c_name <- n_ctry("MUS")
l2_df <- l2_gaez_df %>% filter(country == c_name) %>%
  filter(str_detect(district_name, "Unknown Municipality") == FALSE)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l1_df)

#*******************************************************************************
# Mozambique* ----
c_name <- n_ctry("MOZ")
l2_df <- l2_gaez_df %>% filter(country == c_name) %>%
  filter(district_name %!in% c("Aeroporto", "Lake Malawi"))
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

# l2_df <- l2_gaez_df %>% filter(country == c_name) %>%
#   filter(district_name %!in% c("Aeroporto", "Lake Malawi")) %>%
#   mutate(district_name = case_when(
#     district_name %in% c("Morrumbene", "Maxixe")~  "Morrumbene, Maxixe",
#     district_name %in% c("Nacala-Porto", "Nacala-Velha") 
#     ~"Nacala-Porto, Nacala-Velha",
#     substr(district_name, 1, 15) == "Distrito Urbano"~ "Maputo City",
#     TRUE~district_name)) %>%
#   group_by(district_name, year, country, country_code) %>%
#   #TODO to be precise I should be weighting by the size of each location
#   summarise(
#     across(c(ends_with("har"), ends_with("prd"), ends_with("yld")), ~mean(.x)), 
#     geolevel2 = min(geolevel2),
#             .groups = 'drop')
# saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l2_df)

#*******************************************************************************
# Senegal ----
c_name <- n_ctry("SEN")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l2_df)

#*******************************************************************************
# Sierra Leone ----
c_name <- n_ctry("SLE")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l2_df)

#*******************************************************************************
# South Africa ----
c_name <- n_ctry("SAF")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l1_df)

#*******************************************************************************
# Tanzania ----
c_name <- n_ctry("TZA")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l1_df)

#*******************************************************************************
# Togo ----
c_name <- n_ctry("TGO")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

#*******************************************************************************
# Uganda----
c_name <- n_ctry("UGA")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l1_df)

#*******************************************************************************
# Zambia ----
c_name <- n_ctry("ZMB")
l2_df <- l2_gaez_df %>% filter(country == c_name)
saveRDS(l2_df, paste0(build.dir, c_name, "/Support/l2_gaez.rds"))

l1_df <- l1_gaez_df %>% filter(country == c_name)
saveRDS(l1_df, paste0(build.dir, c_name, "/Support/l1_gaez.rds"))

samp_df <- bind_rows(samp_df, l2_df) %>%
  mutate(
    across(c(geolevel1, geolevel2), ~as.character(.x)),
    ipums_id = ifelse(is.na(geolevel2) == FALSE,geolevel2, geolevel1)) %>%
  relocate(c(country_code, country, year, ipums_id, geolevel2, district_name,
             geolevel1, region_name))

write_dta(samp_df, file.path(build.dir, "Africa/GAEZ data/sample_ts.dta"))