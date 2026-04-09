# Author:  Sam Marshall
# Date:    July 12, 2022
# Title:   Build distance urban
# Output:  
# Desc:    create distance between centroids of geometry and share of area urban         
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

ctries_dir <- "Countries"

get_dist <- function(ctry, lvl) {
  
  # read world consistent boundary centroids for that level
  centroids <- readRDS(here(
    build.dir, "Africa", "Maps", paste0("l", lvl,"_centriods.rds")
  )) %>% 
  filter(
    country %in% ctry
  )
  #ipums_id != "888888" # lake malawi, in two countries
  
  # get distance between centroids
  dist_long <- st_distance(centroids$geometry, centroids$geometry) %>% 
    as.data.frame()
  
  dist_long %<>%
    `colnames<-`(centroids$ipums_id) %>%
    `rownames<-`(centroids$ipums_id) %>% 
    rownames_to_column("ipums_id_d") %>% 
    pivot_longer(cols = -ipums_id_d, 
                 names_to = "ipums_id_o",
                 values_to = "distance") %>%
    mutate(distance = as.numeric(distance)) 
  
}


urban_share <- function(ctry, lvl) {

  # use planar geometry so that it doesnt return error
  sf_use_s2(FALSE)
  ctry_sf <- readRDS(here(
    build.dir, "Africa", "Maps", paste0("l", lvl,"_sf.rds")
  )
  ) %>% 
    filter(country == ctry) %>% 
    st_make_valid() 
  
  if (lvl == 2) {geo_name <- sym("district_name")}
  if (lvl == 1) {geo_name <- sym("region_name")}
  
  d_area <- ctry_sf %>%
    mutate(total_area = st_area(geometry)) %>% 
    as.data.frame() %>% 
    select(-geometry)
  
  # this isn't attaching when the file reads out, must be a bug
  #units::set_units(d_area$total_area,"km^2")
  
  ctry_urban <- urban_sf %>%
    filter(ISO3 == ctry) %>% 
    st_make_valid()
  
  urban_area <- st_intersection(ctry_sf, ctry_urban) %>%
    mutate(overlap = st_area(geometry)) %>%
    as.data.frame() %>% 
    select(-geometry) %>%
    group_by(ipums_id) %>%
    summarise(urban_area = sum(overlap), .groups = 'drop') 
  
  share_df <- full_join(d_area, urban_area) %>%
    mutate(
      total_area = as.numeric(total_area),
      urban_area = as.numeric(urban_area) %>%
        ifelse(is.na(.), 0, .),
      urban_share = urban_area / total_area,
      #make area in hectares
      total_area = total_area / 10^4,
      urban_area = urban_area / 10^4
    ) %>%
    select({{geo_name}}, ipums_id, total_area, urban_area, urban_share) %>%
    filter(str_sub({{geo_name}}, 1, 6) != "Waterb") %>%
    filter(str_sub({{geo_name}}, 1, 3) != "Unk") %>%
    filter({{geo_name}} != "Special Region")
  
  #units::set_units(share_df$urban_area,km^2)
  #units::set_units(share_df$total_area,"km^2")
  
  sf_use_s2(TRUE)
  
  return(share_df)
}

# funtion to do the easy ones
write_du_files <- function(ctry) {
  
  lvl <- census_info %>% filter(country == c_name) %>% pull(lvl)
  
  ctry_dist <- get_dist(ctry, lvl) 
  ctry_urb <- urban_share(ctry, lvl)
  
  saveRDS(ctry_dist, 
          here(build.dir, ctries_dir, ctry, "Support", 
               paste0("cons_l", lvl,"_dist.rds")
          )
  )
  saveRDS(ctry_urb, 
          here(build.dir, ctries_dir, ctry, "Support", 
               paste0("cons_l", lvl,"_urban.rds")
          )
  )
  
}

# read Africapolis data
urban_sf <- read_sf(
  here(raw.dir, "Africa", "africapolis", "AFRICAPOLIS2020", 
  "AFRICAPOLIS2020.shp")) 


#*******************************************************************************
# Angola ----
#*******************************************************************************

c_name <- "AGO"
write_du_files(c_name)

#*******************************************************************************
# Benin ----
#*******************************************************************************

c_name <- "BEN"
lvl_i <- census_info %>% filter(country == c_name) %>% pull(lvl)

ctry_dist <- get_dist(c_name, lvl_i)  
ctry_urb <- urban_share(c_name, lvl_i) %>% ren_ben(lvl = lvl_i)

saveRDS(ctry_dist, here(
  build.dir, ctries_dir, c_name, "Support", "cons_l2_dist.rds")
)
saveRDS(ctry_urb, here(
  build.dir, ctries_dir, c_name, "Support", "cons_l2_urban.rds")
)

#*******************************************************************************
# Botswana ----
#*******************************************************************************
c_name <- "BWA"
write_du_files(c_name)

# ctry_dist <- get_dist(c_name, 1)%>%
#   mutate(
#     across(contains("name"), 
#            ~ifelse(substr(.x,1,6) == "Ghanzi", 
#                    "Ghanzi, Central Kgalagadi Game Reserve (Ckgr)", .x)))
# 
# # total area urban area
# ctry_urb <- urban_share(c_name, 1) %>%
#   mutate(
#     across(contains("name"), 
#            ~ifelse(substr(.x,1,6) == "Ghanzi", 
#                    "Ghanzi, Central Kgalagadi Game Reserve (Ckgr)", .x)))
# 
# saveRDS(ctry_dist, paste0(build.dir, c_name, "/Support/cons_l1_dist.rds"))
# saveRDS(ctry_urb, paste0(build.dir, c_name, "/Support/cons_l1_urban.rds"))

rm(c_name, ctry_dist, ctry_urb)

#*******************************************************************************
# Burkina Faso ----

c_name <- "BFA"
write_du_files(c_name)

#*******************************************************************************
# Cameroon ----

c_name <- "CMR"
write_du_files(c_name)

#*******************************************************************************
# Ghana ----
c_name <- "GHA"
write_du_files(c_name)

#*******************************************************************************
# Guinea ----
#*******************************************************************************
c_name <- "GIN"
write_du_files(c_name)

# ctry_dist <- get_dist(c_name, 1) %>%
#   # this is level one geogrphy, but level two in the data
#   rename_with(~sub("region", "district", .x))
# 
# ctry_urb <- urban_share(c_name, 1) %>%
#   rename(district_name = region_name)
# 
# saveRDS(ctry_dist, paste0(build.dir, c_name, "/Support/cons_l2_dist.rds"))
# saveRDS(ctry_urb, paste0(build.dir, c_name, "/Support/cons_l2_urban.rds"))
# 
# rm(c_name, ctry_dist, ctry_urb)

#*******************************************************************************
# Ivory Coast ----
c_name <- "CIV"
write_du_files(c_name)

#*******************************************************************************
# Kenya ----
c_name <- "KEN"
write_du_files(c_name)

#*******************************************************************************
# Lesotho ----
c_name <- "LSO"
write_du_files(c_name)

#*******************************************************************************
# Malawi ----
c_name <- "MWI"
write_du_files(c_name)

#*******************************************************************************
# Mali ----
c_name <- "MLI"
write_du_files(c_name)

#*******************************************************************************
# Mauritius ----
c_name <- "MUS"
write_du_files(c_name)

#*******************************************************************************
# Mozambique ----

c_name <- "MOZ"
write_du_files(c_name)

# ctry_dist <- get_dist(c_name, 2)  %>%
#   ren_moz(lake = FALSE) %>%
#   group_by(district_name, prev_district_name) %>%
#   summarise(distance = mean(distance), .groups = 'drop') %>%
#   mutate(distance = ifelse(district_name == prev_district_name, 0, distance))
# 
# ctry_urb <- urban_share(c_name, 2) %>%
#   ren_moz(lake = FALSE) %>%
#   group_by(district_name) %>%
#   summarise(across(c(total_area, urban_area), ~sum(.x)), .groups = 'drop') %>%
#   mutate(urban_share = urban_area / total_area)
# 
# saveRDS(ctry_dist, paste0(build.dir, c_name, "/Support/cons_l2_dist.rds"))
# saveRDS(ctry_urb, paste0(build.dir, c_name, "/Support/cons_l2_urban.rds"))
# 
# rm(c_name, ctry_dist, ctry_urb)

#*******************************************************************************
# Rwanda ----
c_name <- "RWA"
write_du_files(c_name)

#*******************************************************************************
# Senegal ----
c_name <- "SEN"
write_du_files(c_name)

#*******************************************************************************
# Sierra Leone ----
c_name <- "SLE"
write_du_files(c_name)

#*******************************************************************************
# South Africa ----
c_name <- "ZAF"
write_du_files(c_name)

## level 3 ----
# use simon data to get level 3

# import shape file data
zaf_sf <- st_read( 
  dsn = here(
    raw.dir, ctries_dir, c_name, "census 2011", 
    "sa-census-2011-ssa-spatial-data1", "MN_SA_2011"
    ), 
  layer="MN_SA_2011"
  ) %>%
  rename_with(~str_to_lower(.)) %>%
  mutate(mn_name = case_when(
    mn_name == "//Khara Hais" ~"Khara Hais",
    mn_name == "Emalahleni" & pr_name == "Eastern Cape" ~"Emalahleni-EC",
    mn_name == "Emalahleni" & pr_name == "Mpumalanga" ~"Emalahleni-MP",
    mn_name == "Naledi" & pr_name == "Free State" ~"Naledi-FS",
    mn_name == "Naledi" & pr_name == "North West" ~"Naledi-NW",
    TRUE ~mn_name
  )) %>%
  rename(district_name = mn_name)

# get centriods of each district
trueCentroids = st_point_on_surface(zaf_sf)

# get distance between centroids
zaf_dist <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
  as.data.frame()  %>%
  `colnames<-`(trueCentroids$district_name) %>%
  `rownames<-`(trueCentroids$district_name) %>% 
  rownames_to_column("district_name") %>% 
  pivot_longer(cols = -district_name, 
               names_to = "prev_district_name",
               values_to = "distance") %>%
  mutate(distance = as.numeric(distance)) 

## share urban ----
sf_use_s2(FALSE)
d_area <- zaf_sf %>% 
  mutate(total_area = st_area(geometry)) %>% 
  as.data.frame() %>% 
  select(-geometry)

urban_area <- st_intersection(zaf_sf, urban_sf %>% filter(ISO3 == "ZAF")) %>%
  mutate(overlap = st_area(geometry)) %>%
  as.data.frame() %>% 
  select(-geometry) %>%
  group_by(district_name) %>%
  summarise(urban_area = sum(overlap), .groups = 'drop') 


zaf_urb_df <- full_join(d_area, urban_area) %>%
  mutate(total_area = as.numeric(total_area),
         urban_area = as.numeric(urban_area) %>%
           ifelse(is.na(.), 0, .),
         urban_share = urban_area / total_area,
         #make area in square kilometers
         total_area = total_area / 1000,
         urban_area = urban_area / 1000) %>%
  select(district_name, total_area, urban_area, urban_share)

sf_use_s2(TRUE)

saveRDS(zaf_dist, here(build.dir, ctries_dir, c_name, "Support", "cons_l3_dist.rds"))
saveRDS(zaf_urb_df, here(build.dir, ctries_dir, c_name, "Support", "cons_l3_urban.rds"))


rm(zaf_sf, trueCentroids, zaf_dist, d_area, urban_area, zaf_urb_df)

# 
# #*******************************************************************************
# # South Sudan ----
# c_name <- "SSD"
# write_du_files(c_name)
# write_du_files(c_name)

#*******************************************************************************
# Sudan ----
# c_name2 <- "SDN"
# 
# urb_sdn <- urban_share(c_name2, 1)
# 
# dist_sdn <- get_dist(c_name2, 1)
# 
# saveRDS(dist_sdn, here(
#   build.dir, ctries_dir, c_name2, "Support", "cons_l1_dist_NS.rds")
# )
# saveRDS(urb_sdn, here(
#   build.dir, ctries_dir, c_name2, "Support", "cons_l1_urban_NS.rds")
# )

## Sudan, S. Sudan ----
c_name <- "SDN"

urb_sudan <- urban_share(c_name, 1)

dist_sudan <- get_dist(c_name, 1)

saveRDS(dist_sudan, here(
  build.dir, ctries_dir, c_name, "Support", "cons_l1_dist.rds")
)
saveRDS(urb_sudan, here(
  build.dir, ctries_dir, c_name, "Support", "cons_l1_urban.rds")
)

rm(c_name1, c_name2, dist_sdn, dist_sudan, urb_sdn, urb_sudan)

#*******************************************************************************
# Tanzania ----
c_name <- "TZA"
write_du_files(c_name)

#*******************************************************************************
# Togo ----
c_name <- "TGO"
write_du_files(c_name)

#*******************************************************************************
# Uganda ----
#*******************************************************************************
c_name <- "UGA"
write_du_files(c_name)

# ctry_dist <- get_dist(c_name, 1) %>%
#   # this is level one geogrphy, but level two in the data
#   rename_with(~sub("region", "district", .x))
# 
# ctry_urb <- urban_share(c_name, 1) %>%
#   rename(district_name = region_name)
# 
# saveRDS(ctry_dist, paste0(build.dir, c_name, "/Support/cons_l2_dist.rds"))
# saveRDS(ctry_urb, paste0(build.dir, c_name, "/Support/cons_l2_urban.rds"))
# 
# rm(c_name, ctry_dist, ctry_urb)


#*******************************************************************************
# Zambia ----
c_name <- "ZMB"
write_du_files(c_name)

#*******************************************************************************
# Zimbabwe ----
c_name <- "ZWE"
write_du_files(c_name)

#*******************************************************************************
# Madagascar ----
c_name <- "MDG"
write_du_files(c_name)

#*******************************************************************************
# Gabon ----
c_name <- "GAB"
write_du_files(c_name)

#*******************************************************************************
# Ethiopia ----
c_name <- "ETH"
write_du_files(c_name)

#*******************************************************************************
# Nigeria ----
c_name <- "NGA"
write_du_files(c_name)
