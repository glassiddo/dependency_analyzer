#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    March 28, 2024
#* Title:   Sample Nightlights
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

id_df <- readRDS(file.path(out.dir, "R/id.rds")) %>%
 # mutate(country_name = ifelse(str_sub(ipums_id, 1, 3) == '729', 
#                               'South Sudan', country_name)) %>%
  {.}

samp_list <- unique(id_df$country_name)
#samp_list <- c("South Sudan", samp_list)

# sample geography lists
l2_list <- id_df %>% filter(geo_lvl == 2) 
l2_list <- unique(l2_list$country_name)
l1_list <- id_df %>% filter(geo_lvl == 1) 
l1_list <- unique(l1_list$country_name)

#*******************************************************************************
# grab data ----
#*******************************************************************************
l2_df <- data.frame()
for (c in l2_list) {
  l2_df %<>% bind_rows(
    readRDS(paste0(build.dir, c, "/Support/l2_nightlight.rds")) )
}

l2_df %<>% mutate(geolevel2 = as.character(geolevel2),
                  ipums_id = geolevel2) %>%
  select(-country_code)

l1_df <- data.frame()
for (c in l1_list) {
  l1_df %<>% bind_rows(
    readRDS(paste0(build.dir, c, "/Support/l1_nightlight.rds")) )
}

l1_df %<>% mutate(geolevel1 = as.character(geolevel1),
                  ipums_id = geolevel1) %>%
  select(-country_code)

# always have South Sudan and Sudan be one country now
nl_df <- bind_rows(l2_df, l1_df) %>%
  mutate(country = ifelse(country == 'South Sudan', 'Sudan', country),
         ipums_id = ifelse(country == 'Botswana', paste0('0', ipums_id), ipums_id))

saveRDS(nl_df, paste0(build.dir, 'Africa/Sample Aggregates/nightlight.rds'))
