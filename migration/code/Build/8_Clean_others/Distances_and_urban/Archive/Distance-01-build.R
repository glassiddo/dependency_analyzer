# Author:  Sam Marshall
# Date:    July 12, 2022
# Title:   Build distance
# Output:  
# Desc:    Build district distance from IPUMS data         
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

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

#*******************************************************************************
# World ----
#*******************************************************************************

## level 1 ----
# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Africa/Shapefiles/IPUMS/level 1/") , 
  layer="world_geolev1_2021",
  verbose=FALSE )

names_df <- tibble(country = spdf@data$CNTRY_NAME,
                   region_name = spdf@data$ADMIN_NAME,
                   region = spdf@data$GEOLEVEL1) %>%
  mutate(id = as.character(row_number() - 1),
         region_name = iconv(region_name, "latin1", "ASCII", "byte") %>%
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
trueCentroids = gCentroid(spdf,byid=TRUE) %>%
  st_as_sf() %>% 
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(names_df) %>%
  filter(str_sub(region_name, 1, 5) != "Water") %>%
  filter(str_sub(region_name, 1, 3) != "Unk")

saveRDS(trueCentroids, paste0(build.dir, "Africa/Maps/l1_centriods.rds"))

rm(spdf, names_df, trueCentroids)

## level 2 ----
# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Africa/Shapefiles/IPUMS/level 2/") , 
  layer="world_geolev2_2021",
  verbose=FALSE )

names_df <- tibble(country = spdf@data$CNTRY_NAME,
                   district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL2) %>%
  mutate(
    id = as.character(row_number() - 1),
    district_name = iconv(district_name, "latin1", "ASCII", "byte") %>%
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

# get centriods of each district (level 2 geometry)
trueCentroids = gCentroid(spdf,byid=TRUE) %>%
  st_as_sf() %>% 
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(names_df) %>%
  filter(str_sub(district_name, 1, 5) != "Water") %>%
  filter(str_sub(district_name, 1, 3) != "Unk")

saveRDS(trueCentroids, paste0(build.dir, "Africa/Maps/l2_centriods.rds"))

#*******************************************************************************
# Benin ----
#*******************************************************************************

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Benin/shapefiles/ben_adm_1m_salb_2019_shapes/") , 
  layer="ben_admbnda_adm2_1m_salb_20190816",
  verbose=FALSE )

names_df <- tibble(district_name = spdf@data$adm2_name) %>%
  mutate(id = as.character(row_number() - 1),
         district_name = case_when(
           district_name == "Aguegues" ~"Aguegue",
           district_name == "Pehunco" ~"Pehonko",
           district_name == "Zogbodomey" ~"Zogbodome",
           TRUE~district_name))

# get centriods of each district
trueCentroids = gCentroid(spdf,byid=TRUE) %>%
  st_as_sf() %>% 
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(names_df) 

# get distance between centroids
dist_long <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
  as.data.frame() %>% 
  `colnames<-`(trueCentroids$id) %>%
  `rownames<-`(trueCentroids$id) %>% 
  rownames_to_column("id") %>% 
  pivot_longer(cols = -id, 
               names_to = "prev_id",
               values_to = "distance") %>%
  full_join(names_df) %>%
  full_join(names_df %>% rename_with(~paste0("prev_", .x))) %>%
  mutate(distance = as.numeric(distance)) %>%
  select(-id, -prev_id)

saveRDS( dist_long, paste0(build.dir, "Benin/Support/cons_l2_dist.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

# ben_dist <- get_dist("Benin", 2) %>%
#   select(district_name) %>% unique() %>% mutate(map = 1)
# 
# ben_names <- ben13_df %>% select(district_name) %>% unique() %>% mutate(cens = 1)
# 
# m <- full_join(ben_dist, ben_names)

#*******************************************************************************
# Botswana ----
#*******************************************************************************

bwa_dist <- get_dist("Botswana", 1) %>%
  # some escape characters here need to get rid of
  mutate(
    across(contains("name"), 
           ~ifelse(substr(.x,1,6) == "Ghanzi", 
                   "Ghanzi, Central Kgalagadi Game Reserve (Ckgr)", .x)))

saveRDS(bwa_dist, paste0(build.dir, "Botswana/Support/cons_l1_dist.rds"))

#*******************************************************************************
# Burkina Faso ----
#*******************************************************************************

bfa_dist <- get_dist("Burkina Faso", 2) 
saveRDS(bfa_dist, paste0(build.dir, "Burkina Faso/Support/cons_l2_dist.rds"))

#*******************************************************************************
# Ghana ----
#*******************************************************************************

gha_dist <- get_dist("Ghana", 2) 
saveRDS( gha_dist, paste0(build.dir, "Ghana/Support/cons_l2_dist.rds"))

#*******************************************************************************
# Guinea ----
#*******************************************************************************

gin_dist <- get_dist("Guinea", 1) %>%
  # this is level one geogrphy, but level two in the data
  rename_with(~sub("region", "district", .x))
saveRDS( gin_dist, paste0(build.dir, "Guinea/Support/cons_l2_dist.rds"))

#*******************************************************************************
# Kenya ----
#*******************************************************************************

ken_dist <- get_dist("Kenya", 2) 
saveRDS(ken_dist, paste0(build.dir, "Kenya/Support/cons_l2_dist.rds"))

#*******************************************************************************
# Lesotho ----

lso_dist <- get_dist("Lesotho", 1)
saveRDS(lso_dist, paste0(build.dir, "Lesotho/Support/cons_l1_dist.rds"))

#*******************************************************************************
# Mali ----

mli_dist <- get_dist("Mali", 2) 
saveRDS(mli_dist, paste0(build.dir, "Mali/Support/cons_l2_dist.rds"))

#*******************************************************************************
# Mauritius ----
#*******************************************************************************

# district level
mus_dist <- get_dist("Mauritius", 2) 
saveRDS(mus_dist, paste0(build.dir, "Mauritius/Support/cons_l2_dist.rds"))

# region level
mus_dist <- get_dist("Mauritius", 1) 
saveRDS(mus_dist, paste0(build.dir, "Mauritius/Support/cons_l1_dist.rds"))

#*******************************************************************************
# Mozambique ----
#*******************************************************************************

moz_dist <- get_dist("Mozambique", 2) %>%
  mutate(
    district_name = case_when(
      substr(district_name, 1, 15) == "Distrito Urbano"~ "Maputo City",
      district_name == "Matutuíne" ~"Matutuane",
      district_name %in% c("Morrumbene", "Maxixe")~  "Morrumbene, Maxixe",
      district_name %in% c("Nacala-Porto", "Nacala-Velha") 
      ~"Nacala-Porto, Nacala-Velha",
      TRUE~district_name)) %>%
  filter(!(district_name %in% c("Aeroporto", "Lake Malawi") | 
             prev_district_name %in% c("Aeroporto", "Lake Malawi")))%>%
  group_by(district_name, prev_district_name) %>%
  summarise(distance = mean(distance), .groups = 'drop') %>%
  mutate(distance = ifelse(district_name == prev_district_name, 0, distance))

saveRDS(moz_dist, paste0(build.dir, "Mozambique/Support/cons_l2_dist.rds"))

#*******************************************************************************
# Senegal ----
#*******************************************************************************

sen_dist <- get_dist("Senegal", 2) 
saveRDS(sen_dist, paste0(build.dir, "Senegal/Support/cons_l2_dist.rds"))

#*******************************************************************************
# Sierra Leone ----
#*******************************************************************************

sle_dist <- get_dist("Sierra Leone", 2) 
saveRDS(sle_dist, paste0(build.dir, "Sierra Leone/Support/cons_l2_dist.rds"))

#*******************************************************************************
# South Africa ----
#*******************************************************************************

# zaf_dist <- get_dist("South Africa", 2)
# saveRDS(zaf_dist, paste0(build.dir, "South Africa/Support/cons_l2_dist.rds"))

## level 3 ----
# use simon data to get level 3

# import shape file data
spdf <- readOGR( 
  dsn= paste0(
    raw.dir, 
    "South Africa/census 2011/sa-census-2011-ssa-spatial-data1/MN_SA_2011/") , 
  layer="MN_SA_2011",
  verbose=FALSE )


names_df <- tibble(mn_name = spdf@data$MN_NAME,
                   mn_code = spdf@data$MN_CODE,
                   pr_name = spdf@data$PR_NAME) %>%
  mutate(id = as.character(row_number() - 1),
         mn_name = case_when(
           mn_name == "//Khara Hais" ~"Khara Hais",
           mn_name == "Emalahleni" & pr_name == "Eastern Cape" ~"Emalahleni-EC",
           mn_name == "Emalahleni" & pr_name == "Mpumalanga" ~"Emalahleni-MP",
           mn_name == "Naledi" & pr_name == "Free State" ~"Naledi-FS",
           mn_name == "Naledi" & pr_name == "North West" ~"Naledi-NW",
           TRUE ~mn_name
         )) 

trueCentroids = gCentroid(spdf,byid=TRUE) %>%
  st_as_sf() %>% 
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(names_df) 

zaf_dist <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
  as.data.frame()  %>%
  `colnames<-`(trueCentroids$mn_name) %>%
  `rownames<-`(trueCentroids$mn_name) %>% 
  rownames_to_column("mn_name") %>% 
  pivot_longer(cols = -mn_name, 
               names_to = "prev_mn_name",
               values_to = "distance") %>%
  mutate(distance = as.numeric(distance)) %>%
  rename(district_name = mn_name,
         prev_district_name = prev_mn_name)

saveRDS(zaf_dist, paste0(build.dir, "South Africa/Support/cons_l3_dist.rds"))

rm(spdf, trueCentroids, names_df)

#*******************************************************************************
# Tanzania ----
#*******************************************************************************

tza_dist <- get_dist("Tanzania", 1) 
saveRDS(tza_dist, paste0(build.dir, "Tanzania/Support/cons_l1_dist.rds"))

#*******************************************************************************
# Uganda ----
#*******************************************************************************

uga_dist <- get_dist("Uganda", 1) %>%
  # this is level one geogrphy, but level two in the data
  rename_with(~sub("region", "district", .x))
saveRDS(uga_dist, paste0(build.dir, "Uganda/Support/cons_l2_dist.rds"))

#*******************************************************************************
# Zambia ----
#*******************************************************************************

zmb_dist <- get_dist("Zambia", 2) 
saveRDS(zmb_dist, paste0(build.dir, "Zambia/Support/cons_l2_dist.rds"))



