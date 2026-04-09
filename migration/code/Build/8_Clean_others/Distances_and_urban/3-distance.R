#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    April 2, 2023
#* Title:   Distance
#* Desc:    Build distance Dataset for each admin unit that we use
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

# read id data
id_df <- readRDS(here(out.dir, "R", "id.rds")) %>%
  select(country_name, country, ipums_id, admin_name) %>%
  rename(ipums_id_d = ipums_id)

pid_df <- id_df %>%
  rename(ipums_id_o = ipums_id_d, prev_admin_name = admin_name)

#*******************************************************************************
read_dist <- function(ctry_str) {
  
  lvl <- census_info %>% filter(country == ctry_str) %>% pull(lvl)
  
  geo_name <- sym(paste0(g_sym(lvl), "_name"))
  prev_geo_name <- sym(paste0(g_sym(lvl, r = "p"), "_name"))
  
  dist_df <- readRDS(
    here(build.dir, "Countries", ctry_str, "Support", 
           paste0("cons_l", lvl, "_dist.rds")
    )) %>%
    mutate(distance = as.numeric(distance),
           distance = distance / 1000) 
  
  avg_dist_df <- dist_df %>% 
    inner_join(id_df) %>% 
    inner_join(pid_df) %>%
    filter(ipums_id_o != ipums_id_d)  %>%
    group_by(ipums_id_o) %>%
    summarise(mean_dist = mean(distance),
              mean_dist_inv = mean(1/distance),
              total_dist_inv = sum(1/distance),
              .groups = 'drop') 
  
  out_df <- inner_join(dist_df, avg_dist_df) %>%
    mutate(inv_dist_shares = ifelse(ipums_id_o != ipums_id_d,
                                    (1 / distance), NA),
           avg_dist_shares = ifelse(ipums_id_o != ipums_id_d,
                                    (1 / distance) / (total_dist_inv), NA),
    ) %>%
    select(-total_dist_inv)  %>% 
    inner_join(id_df) %>% 
    inner_join(pid_df)
}

#*******************************************************************************
countries <- census_info %>% pull(country)

dist_df <- map_dfr(countries, read_dist)

# save ----
dist_df %<>%
  relocate(country_name, country, ipums_id_d, ipums_id_o, admin_name, prev_admin_name) %>%
  var_labels(
    distance = "distance (km) between centroid of o-d",
    mean_dist = "avg dist from o, km",
    inv_dist_shares = "1 / distance o-d",
    avg_dist_shares = "1/dist / sum d (1 / dist od)")

saveRDS(dist_df, here(out.dir, "R", "distance.rds"))
write_dta(dist_df, here(out.dir, "distance.dta"))
