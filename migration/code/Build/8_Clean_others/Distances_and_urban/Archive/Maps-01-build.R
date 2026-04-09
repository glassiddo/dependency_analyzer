# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Feb 4, 2022
# Title:   Map Files
# Output:  
# Desc:    Build map files and save as RDS for quicker access         
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")
library(rgdal)
library(sf)
library(broom)
library(rgeos)

#*******************************************************************************
# Benin ----
#*******************************************************************************

# import shape file data
ben_spdf <- readOGR( 
  dsn= paste0(ben_raw.dir, "shapefiles/ben_adm_1m_salb_2019_shapes/") , 
  layer="ben_admbnda_adm2_1m_salb_20190816",
  verbose=FALSE )

# create df for plotting
spdf_fortified <- broom::tidy(gSimplify(ben_spdf, tol=0.01, topologyPreserve=TRUE))

# get district names from shape data
dist_names_df <- data.frame(ben_spdf@data$adm2_name) 
names(dist_names_df) <- "district_name"
dist_names_df$id <- seq(0,nrow(dist_names_df)-1)
dist_names_df %<>% mutate( id = as.character(id )) %>%
  mutate(
    district_name = ifelse(district_name == "Aguegues", "Aguegue", district_name),
    district_name = ifelse(district_name == "Pehunco", "Pehonko", district_name),
    district_name = ifelse(district_name == "Zogbodomey", "Zogbodome", district_name) 
  )

spdf_fortified <- full_join(spdf_fortified, dist_names_df, by = "id") 

saveRDS( spdf_fortified, paste0(ben.dir, "Maps/District_shapefile.rds"))

# get centriods of each district
trueCentroids = gCentroid(ben_spdf,byid=TRUE)
ben_centriods <- tibble(lat = trueCentroids@coords[, 2],
                        long = trueCentroids@coords[, 1]) %>%
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

saveRDS( ben_centriods, paste0(ben.dir, "Maps/District_centriods.rds"))

# get distance between centroids
trueCentroids %<>% st_as_sf() %>% 
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

dist_long <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
  as.data.frame() %>% 
  `colnames<-`(trueCentroids$district_name) %>%
  `rownames<-`(trueCentroids$district_name) %>% 
  rownames_to_column("district_name") %>% 
  pivot_longer(cols = - district_name, 
               names_to = "prev_district_name",
               values_to = "distance")

saveRDS( dist_long, paste0(ben.dir, "Support/district_distance.rds"))

rm(ben_spdf, dist_names_df, spdf_fortified, trueCentroids, ben_centriods, dist_long)

## Region ----
# import shape file data
ben_spdf <- readOGR( 
  dsn= paste0(ben_raw.dir, "shapefiles/ben_adm_1m_salb_2019_shapes/") , 
  layer="ben_admbnda_adm1_1m_salb_20190816",
  verbose=FALSE )

# create df for plotting
ben_reg <- broom::tidy(
  gSimplify(ben_spdf, tol=0.01, topologyPreserve=TRUE)
)

# get region names from shape data
reg_names_df <- data.frame(ben_spdf@data$adm1_name) 
names(reg_names_df) <- "region_name"
reg_names_df$id <- seq(0,nrow(reg_names_df)-1)
reg_names_df %<>% mutate( id = as.character(id )) 

ben_reg <- full_join(ben_reg, reg_names_df, by = "id") 

saveRDS( ben_reg, paste0(ben.dir, "Maps/Region_shapefile.rds"))

rm(ben_reg, reg_names_df, ben_spdf)

#*******************************************************************************
# Botswana ----
#*******************************************************************************

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Botswana/IPUMS Shapefiles/level 1/") , 
  layer="geo1_bw1981_2011",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL1) %>%
  mutate(id = as.character(row_number() - 1),
         district_name = ifelse(
           district_name == "Ghanzi, Central Kgalagadi Game Reserve (CKGR)\r\n",
           "Ghanzi, Central Kgalagadi Game Reserve (CKGR)",
           district_name))

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
  select(-id, -prev_id, -district, -prev_district)

saveRDS( dist_long, paste0(build.dir, "Botswana/Support/district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

#*******************************************************************************
# Burkina Faso ----
#*******************************************************************************

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Burkina Faso/IPUMS Shapefiles/level 2/") , 
  layer="geo2_bf1996_2006",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL2) %>%
  mutate(id = as.character(row_number() - 1),
         district_name = gsub("<e9>", "e", district_name, perl = TRUE),
         district_name = gsub("<e8>", "e", district_name, perl = TRUE),)

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
  select(-id, -prev_id, -district, -prev_district)

saveRDS( dist_long, 
         paste0(build.dir, "Burkina Faso/Support/district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

#*******************************************************************************
# Ghana ----
#*******************************************************************************
# Consistent Region from Ipums. Make distance between prefectures in census

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Ghana/IPUMS Shapefiles/level 2/") , 
  layer="geo2_gh2000_2010",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL2) %>%
  mutate(id = as.character(row_number() - 1),
         district_name = sub(" or ", " Or ", district_name),
         district_name = sub("KMA", "Kma", district_name))

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
  select(-id, -prev_id, -district, -prev_district)

saveRDS( dist_long, paste0(build.dir, "Ghana/Support/district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

#*******************************************************************************
# Guinea ----
#*******************************************************************************
# Consistent Region from Ipums. Make distance between prefectures in census

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Guinea/IPUMS Shapefiles/level 1/") , 
  layer="geo1_gn1983_2014",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL1) %>%
  mutate(id = as.character(row_number() - 1))

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
  filter(district_name != "Waterbodies") %>%
  filter(prev_district_name != "Waterbodies") %>%
  select(-id, -prev_id, -district, -prev_district)

saveRDS( dist_long, paste0(build.dir, "Guinea/Support/district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

#*******************************************************************************
# Kenya ----
#*******************************************************************************
# Consistent Region from Ipums. Make distance between prefectures in census

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Kenya/IPUMS Shapefiles/level 2/") , 
  layer="geo2_ke1969_2009",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL2) %>%
  mutate(id = as.character(row_number() - 1))

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
  select(-id, -prev_id, -district, -prev_district)

saveRDS( dist_long, paste0(build.dir, "Kenya/Support/district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

#*******************************************************************************
# Mali ----
#*******************************************************************************
# Consistent cercle from Ipums. Make distance between prefectures in census

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Mali/IPUMS Shapefiles/level 2/") , 
  layer="geo2_ml1987_2009",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL2) %>%
  mutate(id = as.character(row_number() - 1),
         district_name = gsub("<e9>", "e", district_name, perl = TRUE),
         district_name = sub("<ef>", "i", district_name, perl = TRUE),
         district_name = ifelse(district_name == "District of Bamako", 
                                "Bamako", district_name))

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
  select(-id, -prev_id, -district, -prev_district)

saveRDS( dist_long, paste0(build.dir, "Mali/Support/district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

#*******************************************************************************
# Mozambique ----
#*******************************************************************************

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Mozambique/IPUMS shapefiles/") , 
  layer="geo2_mz1997_2007",
  verbose=FALSE )

# create df for plotting
spdf_fortified <- broom::tidy(gSimplify(spdf, tol=0.01, topologyPreserve=TRUE))

# get district names from shape data
dist_names_df <- tibble(district_name = spdf@data$ADMIN_NAME) %>%
  mutate(
    id = as.character(row_number() - 1),
    district_name = sub("<e1>", "a", district_name, perl = TRUE),
    district_name = sub("<e7>", "c", district_name, perl = TRUE),
    district_name = sub("<e9>", "e", district_name, perl = TRUE),
    district_name = sub("<e8>", "e", district_name, perl = TRUE),
    district_name = sub("<ed>", "i", district_name, perl = TRUE),
    district_name = sub("<f3>", "o", district_name, perl = TRUE),
    district_name = sub("<fa>", "u", district_name, perl = TRUE),
    district_name = case_when(
      district_name == "Cahora-Bassa"~ "Cahora Bassa",
      district_name == "Beira Cidade"~ "Cidade Da Beira",
      district_name == "Matola Cidade"~ "Cidade Da Matola",
      district_name == "Chimoio Cidade"~ "Cidade De Chimoio",
      district_name == "Inhambane (Cidade)"~ "Cidade De Inhambane",
      district_name == "Pemba Cidade"~ "Cidade De Pemba",
      district_name == "Zumbo"~ "Zumbu",
      district_name == "Nicodala"~ "Nicoadala",
      district_name == "Mandlacaze"~ "Mandlakaze", 
      district_name == "N'Gauma"~ "Ngauma",
      district_name == "Namapa-Erati"~ "Erati", 
      district_name == "Bilene Macia"~ "Bilene",
      district_name == "Pemba-Metuge"~ "Metuge", 
      district_name == "Lake Malawi"~ "Lago Niassa", 
      TRUE~district_name
    ),
    district_name_o = district_name,
    district_name = case_when(
      substr(district_name, 1, 15) == "Distrito Urbano"~ "Cidade De Maputo",
      substr(district_name, 1, 6) == "Nacala"~ "Nacala",
      substr(district_name, 1, 4) == "Lago"~ "Lago",
      district_name %in% c("Chigubo", "Massangena")~ "Chigubo, Massangena",
      district_name %in% c("Ibo", "Quissanga")~ "Ibo, Quissanga",
      district_name %in% c("Mavago", "Mecula")~ "Mavago, Mecula",
      district_name %in% c("Morrumbene", "Maxixe")~ "Morrumbene, Maxixe",
      TRUE~district_name
    )) 

spdf_fortified <- full_join(spdf_fortified, dist_names_df, by = "id") 

saveRDS( spdf_fortified, paste0(build.dir, "Senegal/Maps/District_shapefile.rds"))

# get centriods of each district
trueCentroids = gCentroid(spdf,byid=TRUE)
centriods <- tibble(lat = trueCentroids@coords[, 2],
                    long = trueCentroids@coords[, 1]) %>%
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

saveRDS( centriods, paste0(build.dir, "Mozambique/Maps/District_centriods.rds"))

# get distance between centroids of combined districts
trueCentroids %<>% st_as_sf() %>% 
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

dist_long <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
  as.data.frame() %>% 
  `colnames<-`(trueCentroids$district_name_o) %>%
  `rownames<-`(trueCentroids$district_name_o) %>% 
  rownames_to_column("district_name_o") %>% 
  pivot_longer(cols = - district_name_o, 
               names_to = "prev_district_name_o",
               values_to = "distance") %>%
  full_join(dist_names_df %>% select(-id)) %>%
  full_join(dist_names_df %>% select(-id) %>%
              rename(prev_district_name = district_name,
                     prev_district_name_o = district_name_o)) %>%
  mutate(distance = as.numeric(distance)) %>%
  group_by(district_name, prev_district_name) %>%
  summarise(distance = mean(distance), .groups = 'drop') %>%
  mutate(distance = ifelse(district_name == prev_district_name, 0, distance)) %>%
  filter(district_name != "Aeroporto") %>%
  filter(prev_district_name != "Aeroporto")

saveRDS( dist_long, paste0(build.dir, "Mozambique/Support/district_distance.rds"))

rm(spdf, dist_names_df, spdf_fortified, trueCentroids, centriods, dist_long)

#*******************************************************************************
# Mauritius ----
#*******************************************************************************
# Consistent shapefiles from Ipums

## District ----
# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Mauritius/IPUMS Shapefiles/level 2/") , 
  layer="geo2_mu1990_2011",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL2) %>%
  mutate(id = as.character(row_number() - 1))

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
  select(-id, -prev_id, -district, -prev_district)

saveRDS( dist_long, paste0(build.dir, "Mauritius/Support/district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

## Region ----
# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Mauritius/IPUMS Shapefiles/level 1/") , 
  layer="geo1_mu1990_2011",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL1) %>%
  mutate(id = as.character(row_number() - 1),
         district_name = gsub("è", "e", district_name),
         district_name = gsub("é", "e", district_name),
         district_name = str_to_title(district_name))

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
  select(-id, -prev_id, -district, -prev_district) %>%
  rename_with(~sub("district", "region", .x))

saveRDS( dist_long, paste0(build.dir, "Mauritius/Support/region_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

#*******************************************************************************
# Senegal ----
#*******************************************************************************

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Senegal/department shapefiles/") , 
  layer="geoBoundaries-SEN-ADM2_simplified",
  verbose=FALSE )

# create df for plotting
spdf_fortified <- broom::tidy(gSimplify(spdf, tol=0.01, topologyPreserve=TRUE))

# get district names from shape data
dist_names_df <- tibble(department_name = spdf@data$shapeName) %>%
  mutate(
    id = as.character(row_number() - 1),
    department_name = ifelse(department_name == "Mbacke", "M'backe", department_name),
    department_name = ifelse(department_name == "Mbour", "M'bour", department_name),
    department_name = ifelse(department_name == "Medina Yoroufoula", 
                             "Medina Yoro Foulah", department_name),
    department_name = ifelse(department_name == "Tivaoune", "Tivaouane", department_name),
    department_name = ifelse(department_name == "Koumpentoum", "Koupentoum", department_name),
    d = department_name,
    d = ifelse(d %in% c("Bakel", "Goudiry"), "Bakel, Goudiry", d),
    d = ifelse(d %in% c("Goudomp", "Sedhiou", "Bounkiling"), 
               "Goudomp, Sedhiou, Bounkiling", d),
    d = ifelse(d %in% c("Kaffrine", "Koungheul", "Guinguineo", "Birkelane", 
                        "Gossas", "Malem Hodar"), 
               "Kaffrine, Koungheul, Guinguineo, Birkelane, Gossas, Malem Hoddar", d),
    d = ifelse(d %in% c("Kedougou", "Salemata", "Saraya"), 
               "Kedougou, Salemata, Saraya", d),
    d = ifelse(d %in% c("Kolda", "Medina Yoro Foulah"), "Kolda, Medina Yoro Foulah", d),
    d = ifelse(d %in% c("Pikine", "Guediawaye"), "Pikine, Guediawaye", d),
    d = ifelse(d %in% c("Podor", "Matam", "Kanel", "Linguere", "Ranerou"), 
               "Podor, Matam, Kanel, Linguere, Ranerou", d),
    d = ifelse(d %in% c("Tambacounda", "Koupentoum"), "Tambacounda, Koupentoum", d),
    d = ifelse(d %in% c("Saint Louis", "Dagana"), "Saint Louis, Dagana", d),
    district_name = d) %>%
  select(-d)

spdf_fortified <- full_join(spdf_fortified, dist_names_df, by = "id") 

saveRDS( spdf_fortified, paste0(build.dir, "Senegal/Maps/District_shapefile.rds"))

# get centriods of each district
trueCentroids = gCentroid(spdf,byid=TRUE)
centriods <- tibble(lat = trueCentroids@coords[, 2],
                        long = trueCentroids@coords[, 1]) %>%
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

saveRDS( centriods, paste0(build.dir, "Senegal/Maps/District_centriods.rds"))

# get distance between centroids of combined districts
trueCentroids %<>% st_as_sf() %>% 
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

dist_long <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
  as.data.frame() %>% 
  `colnames<-`(trueCentroids$department_name) %>%
  `rownames<-`(trueCentroids$department_name) %>% 
  rownames_to_column("department_name") %>% 
  pivot_longer(cols = - department_name, 
               names_to = "prev_department_name",
               values_to = "distance") %>%
  full_join(dist_names_df %>% select(-id)) %>%
  full_join(dist_names_df %>% select(-id) %>%
              rename(prev_district_name = district_name,
                     prev_department_name = department_name)) %>%
  mutate(distance = as.numeric(distance)) %>%
  group_by(district_name, prev_district_name) %>%
  summarise(distance = mean(distance), .groups = 'drop') %>%
  mutate(distance = ifelse(district_name == prev_district_name, 0, distance))

saveRDS( dist_long, paste0(build.dir, "Senegal/Support/district_distance.rds"))

rm(spdf, dist_names_df, spdf_fortified, trueCentroids, centriods, dist_long)

## Consistent ----
# Consistent district from Ipums. Make distance between districts in census

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Senegal/IPUMS Shapefiles/level 2/") , 
  layer="geo2_sn1988_2013",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL2) %>%
  mutate(id = as.character(row_number() - 1),
         district_name = ifelse(district_name == "Nioro du Rip", "Nioro Du Rip", 
                                district_name))

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
  select(-id, -prev_id, -district, -prev_district) 

saveRDS( dist_long, 
         paste0(build.dir, "Senegal/Support/consistent_district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

#*******************************************************************************
# Sierra Leone ----
#*******************************************************************************
# Consistent Region from Ipums. Make distance between prefectures in census

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Sierra Leone/IPUMS Shapefiles/level 2/") , 
  layer="geo2_sl2004_2015",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL2) %>%
  mutate(id = as.character(row_number() - 1))

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
  select(-id, -prev_id, -district, -prev_district)

saveRDS( dist_long, 
         paste0(build.dir, "Sierra Leone/Support/district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)


#*******************************************************************************
# South Africa ----
#*******************************************************************************

# import shape file data
sa_spdf <- readOGR( 
  dsn= paste0(saf_raw.dir, 
              "census 2011/sa-census-2011-ssa-spatial-data1/MN_SA_2011/") , 
  layer="MN_SA_2011",
  verbose=FALSE )

# create df for plotting
spdf_fortified <- broom::tidy(gSimplify(sa_spdf, tol=0.01, topologyPreserve=TRUE))

# get municipality names from shape data
dist_names_df <- data.frame(sa_spdf@data$MN_NAME, sa_spdf@data$MN_NAME_C) 
names(dist_names_df) <- c("district_name", "code")
dist_names_df$id <- seq(0,nrow(dist_names_df)-1)

dist_names_df %<>% mutate( id = as.character(id )) %>%
  mutate(
    district_name = ifelse(code == "Emalahleni(EC136 )", "Emalahleni-EC", district_name),
    district_name = ifelse(code == "Emalahleni(MP312 )", "Emalahleni-MP", district_name),
    district_name = ifelse(id == "106", "Khai-Ma", district_name),
    district_name = ifelse(code == "Naledi(FS164 )", "Naledi-FS", district_name),
    district_name = ifelse(code == "Naledi(NW392 )", "Naledi-NW", district_name),
    #muni_name = ifelse(muni_name == "UMuziwabantu", "Umuziwabantu", muni_name),
    #muni_name = ifelse(muni_name == "UPhongolo", "Uphongolo", muni_name),
    district_name = ifelse(district_name == "!Kheis", "Kheis", district_name),
    district_name = ifelse(district_name == "//Khara Hais", "Khara Hais", district_name),
  )

spdf_fortified <- full_join(spdf_fortified, dist_names_df, by = "id") 

saveRDS( spdf_fortified, paste0(saf.dir, "Maps/District_shapefile.rds"))

# get centriods of each district
trueCentroids = gCentroid(sa_spdf,byid=TRUE)
saf_centriods <- tibble(lat = trueCentroids@coords[, 2],
                        long = trueCentroids@coords[, 1]) %>%
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

saveRDS( saf_centriods, paste0(saf.dir, "Maps/District_centriods.rds"))

# get distance between centroids
trueCentroids %<>% st_as_sf() %>% 
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

dist_long <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
  as.data.frame() %>% 
  `colnames<-`(trueCentroids$district_name) %>%
  `rownames<-`(trueCentroids$district_name) %>% 
  rownames_to_column("district_name") %>% 
  pivot_longer(cols = - district_name, 
               names_to = "prev_district_name",
               values_to = "distance")

saveRDS( dist_long, paste0(saf.dir, "Support/district_distance.rds"))


rm(sa_spdf, dist_names_df, spdf_fortified, trueCentroids, saf_centriods, dist_long)

## Consistent ----
# district shapefiles from IPUMS

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "South Africa/IPUMS/Shapefiles/level 2/") , 
  layer="geo2_za2001_2016",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL2) %>%
  mutate(id = as.character(row_number() - 1))

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
  select(-id, -prev_id, -district, -prev_district)

saveRDS(dist_long, paste0(build.dir, "South Africa/Support/cons_l2_dist.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

## Muni Maps ----

# 2001 from Simon data
# spdf <- readOGR( 
#   dsn= paste0(raw.dir,"South Africa/census 2001/",
#               "sa-census-2001-ssa-spatial-data2/MP_SA"), 
#   layer="MP_SA",
#   verbose=FALSE )
# 
# names_df <- tibble(muni_name = spdf@data$ADMIN_NAME,
#                    muni = spdf@data$MUNI2007) %>%
#   mutate(id = as.character(row_number() - 1))
# 
# # create df for plotting
# spdf_2007 <- broom::tidy(gSimplify(spdf, tol=0.01, topologyPreserve=TRUE)) %>% 
#   full_join(names_df)
# 
# saveRDS(spdf_2001, paste0(build.dir, "South Africa/Maps/Muni_shp2001.rds"))

# Ipums years specific municipality maps

# 2007
spdf <- readOGR( 
  dsn= paste0(raw.dir,"South Africa/IPUMS/Shapefiles/level 3 2007"), 
  layer="geo3_za2007",
  verbose=FALSE )

names_df <- tibble(muni_name = spdf@data$ADMIN_NAME,
                   muni = spdf@data$MUNI2007) %>%
  mutate(id = as.character(row_number() - 1))

# create df for plotting
spdf_2007 <- broom::tidy(gSimplify(spdf, tol=0.01, topologyPreserve=TRUE)) %>% 
  full_join(names_df)

saveRDS(spdf_2007, paste0(build.dir, "South Africa/Maps/Muni_shp2007.rds"))

# 2011
spdf <- readOGR( 
  dsn= paste0(raw.dir,"South Africa/IPUMS/Shapefiles/level 3 2011"), 
  layer="geo3_za2011",
  verbose=FALSE )

names_df <- tibble(muni_name = spdf@data$ADMIN_NAME,
                   muni = spdf@data$MUNI2011) %>%
  mutate(id = as.character(row_number() - 1))

spdf_2011 <- broom::tidy(gSimplify(spdf, tol=0.01, topologyPreserve=TRUE)) %>% 
  full_join(names_df)

saveRDS(spdf_2011, paste0(build.dir, "South Africa/Maps/Muni_shp2011.rds"))

# 2016
spdf <- readOGR( 
  dsn= paste0(raw.dir,"South Africa/IPUMS/Shapefiles/level 3 2016"), 
  layer="geo3_za2016",
  verbose=FALSE )

names_df <- tibble(muni_name = spdf@data$ADMIN_NAME,
                   muni = spdf@data$MUNI2016) %>%
  mutate(id = as.character(row_number() - 1))

spdf_2016 <- broom::tidy(gSimplify(spdf, tol=0.01, topologyPreserve=TRUE)) %>% 
  full_join(names_df)

saveRDS(spdf_2016, paste0(build.dir, "South Africa/Maps/Muni_shp2016.rds"))


#*******************************************************************************
# Tanzania ----
#*******************************************************************************

# import shape file data
tza_spdf <- readOGR( 
  dsn= paste0(tza_raw.dir, "District Shapefiles/") , 
  layer="Districts",
  verbose=FALSE )

# create df for plotting
spdf_fortified <- broom::tidy(gSimplify(tza_spdf, tol=0.01, topologyPreserve=TRUE))

# get district names from shape data
dist_names_df <- data.frame(tza_spdf@data$District_N) 
names(dist_names_df) <- "District"
dist_names_df$id <- seq(0,nrow(dist_names_df)-1)
dist_names_df %<>% mutate( id = as.character(id )) %>%
  mutate(
    District = ifelse(District == "Kaskazini A", "Kaskazini ‘A’", District),
    District = ifelse(District == "Kaskazini B", "Kaskazini ‘B’", District),
    District = ifelse(District == "Butiam", "Butiama", District),
    District = ifelse(District %in% c("Bukoba", "Iringa", "Kigoma", "Lindi",
                                      "Mbeya", "Moshi", "Morogoro", "Mpanda",
                                      "Mtwara", "Musoma", "Njombe", "Shinyanga",
                                      "Singida","Songea", "Sumbawanga"),
                      paste0(District, " Rural"), District),
    District = ifelse(District == "Kigoma  Urban", "Kigoma Urban", District),
    District = ifelse(District == "Chake Chake", "Chakechake", District),
    District = ifelse(District == "Micheweni", "Michweweni", District),
    District = ifelse(District == "Mtwara Urban", "Mtwara Mikindani", District),
    District = ifelse(District == "Urambo", "Uramba", District),
    District = ifelse(District == "Mbarali", "Mbalali", District),
    District = ifelse(District == "Makambako Township Authority", "Makambako", District),
    District = ifelse(District == "Mafinga Township Authority", "Mafinga", District)) %>%
  rename(district_name = District)

spdf_fortified <- full_join(spdf_fortified, dist_names_df, by = "id") 

saveRDS( spdf_fortified, paste0(tza.dir, "Maps/District_shapefile.rds"))

# get centriods of each district
trueCentroids = gCentroid(tza_spdf,byid=TRUE)
tza_centriods <- tibble(lat = trueCentroids@coords[, 2],
                        long = trueCentroids@coords[, 1]) %>%
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

saveRDS( tza_centriods, paste0(tza.dir, "Maps/District_centriods.rds"))

# get distance between centroids
trueCentroids %<>% st_as_sf() %>% 
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

dist_long <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
  as.data.frame() %>% 
  `colnames<-`(trueCentroids$district_name) %>%
  `rownames<-`(trueCentroids$district_name) %>% 
  rownames_to_column("district_name") %>% 
  pivot_longer(cols = - district_name, 
               names_to = "prev_district_name",
               values_to = "distance")

saveRDS( dist_long, paste0(tza.dir, "Support/district_distance.rds"))


rm(tza_spdf, dist_names_df, spdf_fortified, trueCentroids, tza_centriods, dist_long)

## Region ----
# import shape file data
tza_spdf <- readOGR( 
  dsn= paste0(tza_raw.dir, "Region Shapefiles/") , 
  layer="tza_admbnda_adm1_20181019",
  verbose=FALSE )

# create df for plotting
tza_reg <- broom::tidy(
  gSimplify(tza_spdf, tol=0.01, topologyPreserve=TRUE)
)

# get region names from shape data
reg_names_df <- data.frame(tza_spdf@data$ADM1_EN) 
names(reg_names_df) <- "region_name"
reg_names_df$id <- seq(0,nrow(reg_names_df)-1)
reg_names_df %<>% mutate( id = as.character(id )) 

tza_reg <- full_join(tza_reg, reg_names_df, by = "id") 

saveRDS( tza_reg, paste0(tza.dir, "Maps/Region_shapefile.rds"))

rm(tza_reg, tza_spdf, reg_names_df)

## Consistent Region ----
# from Ipums. Make distance between regions in census

# import shape file data
spdf <- readOGR( 
  dsn= paste0(tza_raw.dir, "IPUMS Shapefiles/consistent region/") , 
  layer="geo1_tz1988_2012",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(region_name = spdf@data$ADMIN_NAME,
                   region = spdf@data$GEOLEVEL1) %>%
  mutate(id = as.character(row_number() - 1))

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
  filter(region_name != "Waterbody") %>%
  filter(prev_region_name != "Waterbody") %>%
  mutate(distance = as.numeric(distance)) %>%
  select(-id, -prev_id, -region, -prev_region)

saveRDS( dist_long, paste0(tza.dir, "Support/cons_l1_dist.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

#*******************************************************************************
# Uganda ----
#*******************************************************************************

uga_spdf <- readOGR( 
  dsn= paste0(uga_raw.dir, "Shapefiles/Districts/") , 
  layer="Uganda_Districts-2020---136-wgs84",
  verbose=FALSE )

# create df for plotting
spdf_fortified <- broom::tidy(
  gSimplify(uga_spdf, tol=0.01, topologyPreserve=TRUE)
  )

# get district names from shape data
dist_names_df <- data.frame(uga_spdf@data$d) 
names(dist_names_df) <- "district_name"
dist_names_df$id <- seq(0,nrow(dist_names_df)-1)
dist_names_df %<>% mutate( id = as.character(id ))

spdf_fortified <- full_join(spdf_fortified, dist_names_df, by = "id") 

saveRDS( spdf_fortified, paste0(uga.dir, "Maps/District_shapefile.rds"))

# get centriods of each district
trueCentroids = gCentroid(uga_spdf,byid=TRUE)
uga_centriods <- tibble(lat = trueCentroids@coords[, 2],
                        long = trueCentroids@coords[, 1]) %>%
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

saveRDS( uga_centriods, paste0(uga.dir, "Maps/District_centriods.rds"))

# get distance between centroids
trueCentroids %<>% st_as_sf() %>% 
  mutate(id = as.character(row_number() - 1)) %>% 
  left_join(dist_names_df) 

dist_long <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
  as.data.frame() %>% 
  `colnames<-`(trueCentroids$district_name) %>%
  `rownames<-`(trueCentroids$district_name) %>% 
  rownames_to_column("district_name") %>% 
  pivot_longer(cols = - district_name, 
               names_to = "prev_district_name",
               values_to = "distance")

saveRDS( dist_long, paste0(uga.dir, "Support/district_distance.rds"))


rm(uga_spdf, dist_names_df, spdf_fortified, trueCentroids, uga_centriods, dist_long)

##### Region #####
uga_spdf <- readOGR( 
  dsn= paste0(uga_raw.dir, "Shapefiles/uga_admbnda_ubos_20200824_shp/") , 
  layer="uga_admbnda_adm1_ubos_20200824",
  verbose=FALSE )

# create df for plotting
uga_reg <- broom::tidy(
  gSimplify(uga_spdf, tol=0.01, topologyPreserve=TRUE)
)

# get district names from shape data
reg_names_df <- data.frame(uga_spdf@data$ADM1_EN) 
names(reg_names_df) <- "region_name"
reg_names_df$id <- seq(0,nrow(reg_names_df)-1)
reg_names_df %<>% mutate( id = as.character(id ))

uga_reg <- full_join(uga_reg, reg_names_df, by = "id") 

saveRDS( uga_reg, paste0(uga.dir, "Maps/Region_shapefile.rds"))
rm(uga_reg, uga_spdf, reg_names_df)

## Consistent ----
# Consistent district from Ipums. Make distance between districts in census

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Uganda/Shapefiles/IPUMS/level 1/") , 
  layer="geo1_ug1991_2014",
  verbose=FALSE )

# get district names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL1) %>%
  mutate(id = as.character(row_number() - 1))

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
  filter(district_name != "Waterbodies") %>%
  filter(prev_district_name != "Waterbodies") %>%
  select(-id, -prev_id, -district, -prev_district) 

saveRDS( dist_long, 
         paste0(build.dir, "Uganda/Support/consistent_district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)

#*******************************************************************************
# Zambia ----
#*******************************************************************************
# Consistent district from Ipums. Make distance between prefectures in census

# import shape file data
spdf <- readOGR( 
  dsn= paste0(raw.dir, "Zambia/IPUMS Shapefiles/level 2/") , 
  layer="geo2_zm1990_2010",
  verbose=FALSE )

# get region names and codes from shape data
names_df <- tibble(district_name = spdf@data$ADMIN_NAME,
                   district = spdf@data$GEOLEVEL2) %>%
  mutate(id = as.character(row_number() - 1))

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
  select(-id, -prev_id, -district, -prev_district)

saveRDS( dist_long, paste0(build.dir, "Zambia/Support/district_distance.rds"))

rm(spdf, names_df, trueCentroids, dist_long)


#*******************************************************************************
# Africa ----
#*******************************************************************************

# import shape file data
af_spdf <- readOGR( 
  dsn= "data/Raw/Africa/Shapefiles/afr_g2014_2013_0/" , 
  layer="afr_g2014_2013_0",
  verbose=FALSE )

# create df for plotting
spdf_fortified <- broom::tidy(gSimplify(af_spdf, tol=0.01, topologyPreserve=TRUE))

# get district names from shape data
dist_names_df <- tibble(country = af_spdf@data$ADM0_NAME,
                        ISO3 = af_spdf@data$ISO3,
                        ISO2 = af_spdf@data$ISO2) %>%
  mutate(id = as.character(row_number() -1))

spdf_fortified <- full_join(spdf_fortified, dist_names_df, by = "id") 

saveRDS( spdf_fortified, "Data/Build/Africa/Maps/Country_shapefile.rds")
