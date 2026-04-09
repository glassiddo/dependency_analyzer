# Author:  Sam Marshall
# Date:    July 12, 2022
# Title:   Build distance
# Output:  
# Desc:    Build district distance from IPUMS data         
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")
library(sf)
library(broom)

get_dist <- function(ctry, lvl = 2) {
  # read world consistent boundary centroids for that level
  centroids <- readRDS(paste0(build.dir, "Africa/Maps/l", lvl,"_centriods.rds"))
  
  trueCentroids <- centroids %>% filter(.$country == ctry)
  
  # get distance between centroids
  dist_long <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
    as.data.frame()
  
  if (lvl == 2) {
    dist_long %<>%
      `colnames<-`(trueCentroids$district_name) %>%
      `rownames<-`(trueCentroids$district_name) %>% 
      rownames_to_column("district_name") %>% 
      pivot_longer(cols = -district_name, 
                   names_to = "prev_district_name",
                   values_to = "distance") %>%
      mutate(distance = as.numeric(distance)) 
  }
  else {
    dist_long %<>%
      `colnames<-`(trueCentroids$region_name) %>%
      `rownames<-`(trueCentroids$region_name) %>% 
      rownames_to_column("region_name") %>% 
      pivot_longer(cols = -region_name, 
                   names_to = "prev_region_name",
                   values_to = "distance") %>%
      mutate(distance = as.numeric(distance)) 
  }
  return(dist_long)
}

urban_share <- function(ctry_name, ccode, lvl = 2) {
  
  # use planar geometry so that it doesnt return error
  sf_use_s2(FALSE)
  ctry_sf <- readRDS(paste0(build.dir, "Africa/Maps/l", lvl,"_sf.rds")) %>%
    filter(country == ctry_name)
  
  if (lvl == 2) {geo_name <- sym("district_name")}
  if (lvl == 1) {geo_name <- sym("region_name")}
  
  d_area <- ctry_sf %>%
    mutate(total_area = st_area(geometry)) %>% 
    as.data.frame() %>% 
    select(-geometry)
  
  ctry_urban <- urban_sf %>% filter(ISO3 == ccode)
  
  
  urban_area <- st_intersection(ctry_sf, ctry_urban) %>%
    mutate(overlap = st_area(geometry)) %>%
    as.data.frame() %>% 
    select(-geometry) %>%
    group_by({{geo_name}}) %>%
    summarise(urban_area = sum(overlap), .groups = 'drop') 
  
  
  share_df <- full_join(d_area, urban_area) %>%
    mutate(total_area = as.numeric(total_area),
           urban_area = as.numeric(urban_area) %>%
             ifelse(is.na(.), 0, .),
           urban_share = urban_area / total_area,
           #make area in square kilometers
           total_area = total_area / 1000,
           urban_area = urban_area / 1000) %>%
    select({{geo_name}}, total_area, urban_area, urban_share) %>%
    filter(str_sub({{geo_name}}, 1, 5) != "Water") %>%
    filter(str_sub({{geo_name}}, 1, 3) != "Unk") %>%
    filter({{geo_name}} != "Special Region")
  
  sf_use_s2(TRUE)
  
  return(share_df)
}

# funtion to do the easy ones
write_du_files <- function(ctry_name, lvl) {
  ctry3 <- g_ctry3(ctry_name)
  
  ctry_dist <- get_dist(ctry_name, lvl) 
  ctry_urb <- urban_share(ctry_name, ctry3, lvl)
  
  saveRDS(ctry_dist, 
          paste0(build.dir, ctry_name, "/Support/cons_l", lvl,"_dist.rds"))
  saveRDS(ctry_urb, 
          paste0(build.dir, ctry_name, "/Support/cons_l", lvl,"_urban.rds"))
  
}

# read Africapolis data
urban_sf <- st_read(dsn = "data/Raw/Africa/africapolis/AFRICAPOLIS2020/", 
                    layer ="AFRICAPOLIS2020") 

#*******************************************************************************
# World ----
#*******************************************************************************

ctry_list <- c("Benin", "Botswana", "Burkina Faso", "Cameroon",
               "Egypt", "Ethiopia", "Ghana", "Guinea", "Kenya",
               "Lesotho", "Liberia", "Malawi", "Mali", "Mauritius",
               "Morocco", "Mozambique", "Rwanda", "Senegal", 
               "Sierra Leone", "South Africa", "South Sudan",
               "Sudan", "Tanzania", "Togo", "Uganda", "Zambia",
               "Zimbabwe")

## level 1 ----
# import shape file data
africa_sf <- st_read(dsn= paste0(raw.dir, "Africa/Shapefiles/IPUMS/level 1/"), 
                     layer="world_geolev1_2021") %>%
  filter(CNTRY_NAME %in% ctry_list) %>%
  rename(country = CNTRY_NAME,
         region_name = ADMIN_NAME,
         region = GEOLEVEL1) %>%
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



# get centriods of each region (level 1 geometry)
#NB st_centroid doesn't work for Senegal because point doesn't fall on surface
sf_use_s2(FALSE)
centroids_sf <- africa_sf %>%
  filter(str_sub(region_name, 1, 7) != "Waterbo") %>%
  filter(str_sub(region_name, 1, 3) != "Unk") %>%
  filter(region_name != "Special Region") %>%
  st_crop(c(xmin = -180, xmax = 180, ymax = 180, ymin = -40)) %>%
  st_point_on_surface()

sf_use_s2(TRUE)
saveRDS(centroids_sf, paste0(build.dir, "Africa/Maps/l1_centriods.rds"))
saveRDS(africa_sf, paste0(build.dir, "Africa/Maps/l1_sf.rds"))

rm(africa_sf, centroids_sf)


## level 2 ----
# import shape file data
africa_sf <- st_read(dsn= paste0(raw.dir, "Africa/Shapefiles/IPUMS/level 2/"), 
                     layer="world_geolev2_2021") %>%
  filter(CNTRY_NAME %in% ctry_list) %>%
  rename(country = CNTRY_NAME,
         district_name = ADMIN_NAME,
         district = GEOLEVEL2) %>%
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

sf_use_s2(FALSE)
centroids_sf <- africa_sf %>%
  filter(str_sub(district_name, 1, 7) != "Waterbo") %>%
  filter(str_sub(district_name, 1, 3) != "Unk") %>%
  filter(district_name != "Special Region") %>%
  st_crop(c(xmin = -180, xmax = 180, ymax = 180, ymin = -40)) %>%
  st_point_on_surface()

sf_use_s2(TRUE)

saveRDS(centroids_sf, paste0(build.dir, "Africa/Maps/l2_centriods.rds"))
saveRDS(africa_sf, paste0(build.dir, "Africa/Maps/l2_sf.rds"))

rm(africa_sf, centroids_sf)

#*******************************************************************************
# Benin ----
#*******************************************************************************

# import shape file data
# ben_sf <- st_read( 
#   dsn= paste0(raw.dir, "Benin/shapefiles/ben_adm_1m_salb_2019_shapes/") , 
#   layer="ben_admbnda_adm2_1m_salb_20190816") %>%
#   rename(district_name = adm2_name,
#          region_name = adm1_name,
#          country_name = adm0_name) %>%
#   mutate(district_name = case_when(
#     district_name == "Aguegues" ~"Aguegue",
#     district_name == "Pehunco" ~"Pehonko",
#     district_name == "Zogbodomey" ~"Zogbodome",
#     TRUE ~district_name
#   ))
# 
# # get centriods of each district
# trueCentroids = st_centroid(ben_sf)
# 
# # get distance between centroids
# dist_long <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
#   as.data.frame() %>% 
#   `colnames<-`(trueCentroids$district_name) %>%
#   `rownames<-`(trueCentroids$district_name) %>% 
#   rownames_to_column("district_name") %>% 
#   pivot_longer(cols = - district_name, 
#                names_to = "prev_district_name",
#                values_to = "distance")
# 
# ## share urban ----
# d_area <- ben_sf %>% 
#   mutate(total_area = st_area(geometry)) %>% 
#   as.data.frame() %>% 
#   select(-geometry)
# 
# urban_area <- st_intersection(ben_sf, urban_sf %>% filter(ISO3 == "BEN")) %>%
#   mutate(overlap = st_area(geometry)) %>%
#   group_by(district_name, region_name) %>%
#   summarise(urban_area = sum(overlap), .groups = 'drop') %>% 
#   as.data.frame() %>% 
#   select(-geometry)
# 
# share_urb_df <- full_join(d_area, urban_area) %>%
#   mutate(total_area = as.numeric(total_area),
#          urban_area = as.numeric(urban_area) %>%
#            ifelse(is.na(.), 0, .),
#          urban_share = urban_area / total_area,
#          #make area in square kilometers
#          total_area = total_area / 1000,
#          urban_area = urban_area / 1000) %>%
#   select(district_name, region_name, total_area, urban_area, urban_share)
# 
# saveRDS( dist_long, paste0(build.dir, "Benin/Support/cons_l2_dist.rds"))
# saveRDS( share_urb_df, paste0(build.dir, "Benin/Support/cons_l2_urban.rds"))
# 
# rm(ben_sf, trueCentroids, dist_long, d_area, urban_area, share_urb_df)


c_name <- "Benin"
ctry3 <- "BEN"
ctry_dist <- get_dist(c_name, 1) %>% ren_ben(lvl = 1)  
ctry_urb <- urban_share(c_name, ctry3, 1) %>% ren_ben(lvl = 1)

saveRDS(ctry_dist, paste0(build.dir, c_name, "/Support/cons_l1_dist.rds"))
saveRDS(ctry_urb, paste0(build.dir, c_name, "/Support/cons_l1_urban.rds"))

ctry_dist <- get_dist(c_name, 2) %>% ren_ben(lvl = 2)  
ctry_urb <- urban_share(c_name, ctry3, 2) %>% ren_ben(lvl = 2)

saveRDS(ctry_dist, paste0(build.dir, c_name, "/Support/cons_l2_dist.rds"))
saveRDS(ctry_urb, paste0(build.dir, c_name, "/Support/cons_l2_urban.rds"))

#*******************************************************************************
# Botswana ----
#*******************************************************************************
c_name <- "Botswana"
ctry3 <- "BWA"

ctry_dist <- get_dist(c_name, 1)%>%
  mutate(
    across(contains("name"), 
           ~ifelse(substr(.x,1,6) == "Ghanzi", 
                   "Ghanzi, Central Kgalagadi Game Reserve (Ckgr)", .x)))

# total area urban area
ctry_urb <- urban_share(c_name, ctry3, 1) %>%
  mutate(
    across(contains("name"), 
           ~ifelse(substr(.x,1,6) == "Ghanzi", 
                   "Ghanzi, Central Kgalagadi Game Reserve (Ckgr)", .x)))

saveRDS(ctry_dist, paste0(build.dir, c_name, "/Support/cons_l1_dist.rds"))
saveRDS(ctry_urb, paste0(build.dir, c_name, "/Support/cons_l1_urban.rds"))

rm(c_name, ctry3, ctry_dist, ctry_urb)

#*******************************************************************************
# Burkina Faso ----

c_name <- "Burkina Faso"
write_du_files(c_name, 2)
write_du_files(c_name, 1)

#*******************************************************************************
# Ghana ----
c_name <- "Ghana"
write_du_files(c_name, 2)
write_du_files(c_name, 1)

#*******************************************************************************
# Guinea ----
#*******************************************************************************
c_name <- "Guinea"
ctry3 <- "GIN"

ctry_dist <- get_dist(c_name, 1) %>%
  # this is level one geogrphy, but level two in the data
  rename_with(~sub("region", "district", .x))

ctry_urb <- urban_share(c_name, ctry3, 1) %>%
  rename(district_name = region_name)

saveRDS(ctry_dist, paste0(build.dir, c_name, "/Support/cons_l2_dist.rds"))
saveRDS(ctry_urb, paste0(build.dir, c_name, "/Support/cons_l2_urban.rds"))

rm(c_name, ctry3, ctry_dist, ctry_urb)

#*******************************************************************************
# Kenya ----
c_name <- "Kenya"
write_du_files(c_name, 2)
write_du_files(c_name, 1)

#*******************************************************************************
# Lesotho ----
c_name <- "Lesotho"
write_du_files(c_name, 1)

#*******************************************************************************
# Mali ----
c_name <- "Mali"
write_du_files(c_name, 2)
write_du_files(c_name, 1)

#*******************************************************************************
# Mauritius ----
c_name <- "Mauritius"
write_du_files(c_name, 2)
write_du_files(c_name, 1)

#*******************************************************************************
# Mozambique ----

c_name <- "Mozambique"
ctry3 <- "MOZ"
write_du_files(c_name, 1)

ctry_dist <- get_dist(c_name, 2)  %>%
  ren_moz(lake = FALSE) %>%
  group_by(district_name, prev_district_name) %>%
  summarise(distance = mean(distance), .groups = 'drop') %>%
  mutate(distance = ifelse(district_name == prev_district_name, 0, distance))

ctry_urb <- urban_share(c_name, ctry3, 2) %>%
  ren_moz(lake = FALSE) %>%
  group_by(district_name) %>%
  summarise(across(c(total_area, urban_area), ~sum(.x)), .groups = 'drop') %>%
  mutate(urban_share = urban_area / total_area)

saveRDS(ctry_dist, paste0(build.dir, c_name, "/Support/cons_l2_dist.rds"))
saveRDS(ctry_urb, paste0(build.dir, c_name, "/Support/cons_l2_urban.rds"))

rm(c_name, ctry3, ctry_dist, ctry_urb)

#*******************************************************************************
# Senegal ----
c_name <- "Senegal"
write_du_files(c_name, 2)
write_du_files(c_name, 1)


#*******************************************************************************
# Sierra Leone ----
c_name <- "Sierra Leone"
write_du_files(c_name, 2)
write_du_files(c_name, 1)

#*******************************************************************************
# South Africa ----
c_name <- "South Africa"
write_du_files(c_name, 2)
write_du_files(c_name, 1)

## level 3 ----
# use simon data to get level 3

# import shape file data
zaf_sf <- st_read( 
  dsn= paste0(
    raw.dir, 
    "South Africa/census 2011/sa-census-2011-ssa-spatial-data1/MN_SA_2011/") , 
  layer="MN_SA_2011") %>%
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

saveRDS(zaf_dist, paste0(build.dir, "South Africa/Support/cons_l3_dist.rds"))
saveRDS(zaf_urb_df, paste0(build.dir, "South Africa/Support/cons_l3_urban.rds"))


rm(zaf_sf, trueCentroids, zaf_dist, d_area, urban_area, zaf_urb_df)


#*******************************************************************************
# Tanzania ----
c_name <- "Tanzania"
write_du_files(c_name, 2)
write_du_files(c_name, 1)

#*******************************************************************************
# Uganda ----
#*******************************************************************************
c_name <- "Uganda"
ctry3 <- "UGA"

ctry_dist <- get_dist(c_name, 1) %>%
  # this is level one geogrphy, but level two in the data
  rename_with(~sub("region", "district", .x))

ctry_urb <- urban_share(c_name, ctry3, 1) %>%
  rename(district_name = region_name)

saveRDS(ctry_dist, paste0(build.dir, c_name, "/Support/cons_l2_dist.rds"))
saveRDS(ctry_urb, paste0(build.dir, c_name, "/Support/cons_l2_urban.rds"))

rm(c_name, ctry3, ctry_dist, ctry_urb)


#*******************************************************************************
# Zambia ----
#*******************************************************************************
c_name <- "Zambia"
write_du_files(c_name, 2)
write_du_files(c_name, 1)


