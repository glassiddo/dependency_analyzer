#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    March 28, 2024
#* Title:   Sample Deforestation
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

id_df <- readRDS(file.path(out.dir, "R/id.rds")) %>%
  mutate(country_name = ifelse(str_sub(ipums_id, 1, 3) == '729', 
                               'South Sudan', country_name))

samp_list <- unique(id_df$country_name)
samp_list <- c("South Sudan", samp_list)

# sample geography lists
l2_list <- id_df %>% filter(geo_lvl == 2) 
l2_list <- unique(l2_list$country_name)
l1_list <- id_df %>% filter(geo_lvl == 1) 
l1_list <- unique(l1_list$country_name)

# remove cases of countries starting with zz
# ideally the filtering should be based on country codes
# but given that the files are saved with country names, use them
l2_list <- str_remove(l2_list, "^zz ")
l1_list <- str_remove(l1_list, "^zz ")

#*******************************************************************************
# grab data ----
#*******************************************************************************
l2_df <- data.frame()
for (c in l2_list) {
  l2_df %<>% bind_rows(
    readRDS(paste0(build.dir, c, "/Forest Cover/l2_loss30c.rds")) )
}

l2_df %<>% mutate(geolevel2 = as.character(geolevel2),
                  ipums_id = geolevel2)

l1_df <- data.frame()
for (c in l1_list) {
  l1_df %<>% bind_rows(
    readRDS(paste0(build.dir, c, "/Forest Cover/l1_loss30c.rds")) )
}

l1_df %<>% mutate(geolevel1 = as.character(geolevel1),
                  ipums_id = geolevel1)

# always have South Sudan and Sudan be one country now
fl_df <- bind_rows(l2_df, l1_df) %>%
  mutate(
    country = case_when(
      country == 'South Sudan' ~ 'Sudan', 
      cntry_code == "384" ~ "Ivory Coast",
      TRUE ~ country),
    cntry_code = case_when(
      cntry_code == 728 ~ 729, 
      TRUE ~ cntry_code),
    ctry_id = 
        str_pad(as.character(cntry_code), 
                width = 3, side = "left", pad = "0") # add zero in the beginning if two digits
  ) %>% 
  select(-cntry_code, -country)

saveRDS(fl_df, paste0(build.dir, 'Africa/Sample Aggregates/loss30c.rds'))
