#* Project: Migration Africa
#* Author:  Flavia Grasso
#* Date:    February 2, 2024
#* Title:   Including distance for countries in different regions
#* Desc:    Build distance Dataset for each admin unit that we use
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

# get distance ----
countries_l1 <- census_info %>% 
  filter(lvl == 1) %>% 
  select(country) %>% 
  pull()

countries_l2 <- census_info %>% 
  filter(lvl == 2) %>% 
  select(country) %>% 
  pull()

# read world consistent boundary centroids for levels 1 and 2
centroids1 <- readRDS(here(build.dir, "Africa", "Maps", "l1_centriods.rds")) %>% 
  mutate(country=ifelse(country=="SSD", 'SDN', country)) 
centroids2 <- readRDS(here(build.dir, "Africa", "Maps", "l2_centriods.rds")) %>% 
  mutate(country=ifelse(country=="SSD", 'SDN', country)) 

trueCentroids1 <- centroids1 %>% filter(country %in% countries_l1) 
trueCentroids2 <- centroids2 %>% filter(country %in% countries_l2) 

trueCentroids <- bind_rows(trueCentroids1, trueCentroids2)

# get distance between centroids
dist_long <- st_distance(trueCentroids$geometry, trueCentroids$geometry) %>% 
  as.data.frame()

dist_long %<>%
  `colnames<-`(trueCentroids$ipums_id) %>%
  `rownames<-`(trueCentroids$ipums_id) %>% 
  rownames_to_column("ipums_id_d") %>% 
  pivot_longer(cols = -ipums_id_d, 
               names_to = "ipums_id_o",
               values_to = "distance") %>%
  mutate(distance = as.numeric(distance)) 

rm(trueCentroids, trueCentroids1, trueCentroids2, centroids1, centroids2)
#saveRDS(dist_long, paste0(build.dir,"Support/cons_dist.rds"))


#read distance ----
# read id data
id_df <- readRDS(here(out.dir, "R", "id.rds")) %>%
  select(country, ipums_id, admin_name) %>%
  rename(ipums_id_d = ipums_id)

pid_df <- id_df %>%
  rename(
    prev_country = country, 
    ipums_id_o = ipums_id_d, 
    prev_admin_name = admin_name
    )

#read pairs for census 1 and 2
mig_pairs1 <- readRDS(here(out.dir, "R", "full_migration_census1.rds")) %>%
  select(ipums_id_d, ipums_id_o)
mig_pairs2 <- readRDS(here(out.dir, "R", "full_migration_census2.rds")) %>%
  select(ipums_id_d, ipums_id_o)


dist_df1 <- dist_long %>%
  mutate(distance = as.numeric(distance),
         distance = distance / 1000) %>%
  inner_join(mig_pairs1)

dist_df2 <- dist_long %>%
  mutate(distance = as.numeric(distance),
         distance = distance / 1000) %>%
  inner_join(mig_pairs2)


avg_ctry_dist_df1 <- dist_df1 %>%
  inner_join(id_df) %>% 
  inner_join(pid_df) %>%
  filter(ipums_id_o != ipums_id_d)  %>%
  group_by(ipums_id_o, country) %>%
  summarize(mean_dist_ctry = mean(distance), 
            mean_dist_inv_ctry = mean(1/distance),
            total_dist_inv_ctry = sum(1/distance),
            .groups='drop') 

avg_ctry_dist_df2 <- dist_df2 %>%
  inner_join(id_df) %>% 
  inner_join(pid_df) %>%
  filter(ipums_id_o != ipums_id_d)  %>%
  group_by(ipums_id_o, country) %>%
  summarize(mean_dist_ctry = mean(distance), 
            mean_dist_inv_ctry = mean(1/distance),
            total_dist_inv_ctry = sum(1/distance),
            .groups='drop') 


avg_dist_df1 <- dist_df1 %>% 
  inner_join(id_df) %>% 
  inner_join(pid_df) %>%
  filter(ipums_id_o != ipums_id_d)  %>%
  group_by(ipums_id_o) %>%
  summarise(mean_dist = mean(distance),
            mean_dist_inv = mean(1/distance),
            total_dist_inv = sum(1/distance),
            .groups = 'drop') 

avg_dist_df2 <- dist_df2 %>% 
  inner_join(id_df) %>% 
  inner_join(pid_df) %>%
  filter(ipums_id_o != ipums_id_d)  %>%
  group_by(ipums_id_o) %>%
  summarise(mean_dist = mean(distance),
            mean_dist_inv = mean(1/distance),
            total_dist_inv = sum(1/distance),
            .groups = 'drop') 


dist_df1 <- dist_df1 %>% 
  left_join(
    avg_ctry_dist_df1, 
    by = "ipums_id_o", 
    relationship = "many-to-many" # few dest countries for each origin ipums_id
  ) %>% 
  inner_join(avg_ctry_dist_df1) %>%
  inner_join(avg_dist_df1) %>%
  mutate(inv_dist_shares = ifelse(ipums_id_o != ipums_id_d,
                                  (1 / distance), NA),
         avg_dist_shares = ifelse(ipums_id_o != ipums_id_d,
                                  (1 / distance) / (total_dist_inv), NA)) %>%
  select(-total_dist_inv)  %>% 
  inner_join(id_df) %>% 
  inner_join(pid_df) %>%
  relocate(country, prev_country, ipums_id_d, ipums_id_o, admin_name, prev_admin_name) %>%
  var_labels(
    distance = "distance (km) between centroid of o-d",
    mean_dist = "avg dist from o, km",
    mean_dist_ctry = "avg dist from o in country, km",
    inv_dist_shares = "1 / distance o-d",
    avg_dist_shares = "1/dist / sum d (1 / dist od)")

dist_df2 <- inner_join(
  dist_df2, avg_ctry_dist_df2,
  by = "ipums_id_o", 
  relationship = "many-to-many" # few dest countries for each origin ipums_id
  ) %>%
  inner_join(avg_dist_df1) %>%
  mutate(inv_dist_shares = ifelse(ipums_id_o != ipums_id_d,
                                  (1 / distance), NA),
         avg_dist_shares = ifelse(ipums_id_o != ipums_id_d,
                                  (1 / distance) / (total_dist_inv), NA)) %>%
  select(-total_dist_inv)  %>% 
  inner_join(id_df) %>% 
  inner_join(pid_df) %>%
  relocate(country, prev_country, ipums_id_d, ipums_id_o, admin_name, prev_admin_name) %>%
  var_labels(
    distance = "distance (km) between centroid of o-d",
    mean_dist = "avg dist from o, km",
    mean_dist_ctry = "avg dist from o in country, km",
    inv_dist_shares = "1 / distance o-d",
    avg_dist_shares = "1/dist / sum d (1 / dist od)")

rm(id_df, pid_df, dist_long, avg_ctry_dist_df1, avg_dist_df1, mig_pairs1, 
   avg_ctry_dist_df2, avg_dist_df2, mig_pairs2)


saveRDS(dist_df1, here(out.dir, "R", "full_distance_census1.rds"))
write_dta(dist_df1, here(out.dir, "full_distance_census1.dta"))

saveRDS(dist_df2, here(out.dir, "R", "full_distance_census2.rds"))
write_dta(dist_df2, here(out.dir, "full_distance_census2.dta"))

