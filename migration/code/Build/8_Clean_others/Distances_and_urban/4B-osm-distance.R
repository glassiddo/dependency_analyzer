#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    April 2, 2023
#* Title:   Travel time/distance
#* Desc:    Build distance Dataset for each admin unit that we use
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

# read data
dist_df <- readRDS(here(out.dir, "R", "distance.rds"))

osm_dist_df <- readRDS(here(
  build.dir, "Africa", "Travel Distance", "osm_dist.rds")
  ) %>%
  filter(admin_name %!in% c('Lake Malawi', 'Lake Kivu')) %>%
  filter(prev_admin_name %!in% c('Lake Malawi', 'Lake Kivu'))

# create vars a la distance function
avg_dist_df <- osm_dist_df %>% 
  filter(ipums_id_o != ipums_id_d)  %>%
  group_by(ipums_id_o) %>%
  summarise(mean_osm_dist = mean(dist),
            mean_osm_dist_inv = mean(1/dist),
            total_osm_dist_inv = sum(1/dist),
            mean_osm_dur = mean(dur),
            mean_osm_dur_inv = mean(1/dur),
            total_osm_dur_inv = sum(1/dur),
            .groups = 'drop') 

osm_df <- inner_join(osm_dist_df, avg_dist_df) %>%
  filter(ipums_id_o != ipums_id_d) %>%
  mutate(inv_osm_dist_shares = 1 / dist,
         avg_osm_dist_shares = (1 / dist) / total_osm_dist_inv,
         inv_osm_dur_shares = 1 / dur,
         avg_osm_dur_shares = (1 / dur) / total_osm_dur_inv,
  ) %>%
  select(-total_osm_dist_inv, total_osm_dur_inv) 


#*******************************************************************************
# Distance shares ----
shares_df <- dist_df %>%
  filter(ipums_id_d != ipums_id_o) %>%
  full_join(osm_df %>% select(-c(admin_name, prev_admin_name))) %>%
  group_by(ipums_id_o, country_name) %>%
  summarise(dist_shares_sum = sum(inv_dist_shares, na.rm = TRUE),
            osm_dist_shares_sum = sum(inv_osm_dist_shares, na.rm = TRUE),
            osm_dur_shares_sum = sum(inv_osm_dur_shares, na.rm = TRUE),
            .groups = 'drop') %>%
  rename(ipums_id = ipums_id_o) %>%
  var_labels(
    dist_shares_sum = "sum strait-line inv-dist shares",
    osm_dist_shares_sum = "sum osm inv-dist shares",
    osm_dur_shares_sum = "sum osm inv-duration shares")

# distance matrix ----
dist_mat <- dist_df %>%
  filter(ipums_id_d != ipums_id_o) %>%
  full_join(osm_df %>% select(-c(admin_name, prev_admin_name))) %>% 
  relocate(country_name, ipums_id_d, ipums_id_o, admin_name, prev_admin_name) %>%
  var_labels(
    distance = "strait-line distance (km) between centroid of o-d",
    mean_dist = "avg strate-line dist from o, km",
    inv_dist_shares = "1 / strait distance o-d",
    avg_dist_shares = "1/dist / sum d (1 / dist od)",
    dist = "osm distance (km) between centroid of o-d",
    mean_osm_dist = "avg osm dist from o, km",
    inv_osm_dist_shares = "1 / osm distance o-d",
    avg_osm_dist_shares = "1/dist / sum d (1 / dist od)",
    dur = "osm travel time (hours) between centroid of o-d",
    mean_osm_dur = "avg osm duration from o, km",
    inv_osm_dur_shares = "1 / osm duration o-d",
    avg_osm_dur_shares = "1/duration / sum d (1 / duration od)")

saveRDS(shares_df, here(out.dir, "R", "dist_shares.rds"))
write_dta(shares_df, here(out.dir, "dist_shares.dta"))

saveRDS(dist_mat, here(out.dir, "R", "dist_mat.rds"))
write_dta(dist_mat, here(out.dir, "dist_mat.dta"))
