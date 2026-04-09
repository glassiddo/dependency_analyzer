#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    March 5, 2025
#* Title:   GLOBMAP FTC
#* Desc:    
#* Note:    Merges the yearly datasets and gets deltas 
#*******************************************************************************
#*

source("code/SSA_env_SetUp.R")

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country)

yr_dfs <- list()
for (yr in c(2000:2021)){
  yr_dfs[[as.character(yr)]] <- read_csv(here(
    build.dir, "Africa", "MODIS_VCF", "GAFTC", paste0("gaftc", yr, ".csv")
  ))
}

data_gaftc <- bind_rows(yr_dfs, .id = "year") %>% 
  rename(avg_gaftc = gaftc)

gaftc_waves <- list()

for (w in 1:2) {
  
  # get the fl start and end years
  fl_yrs <- readRDS(here(
    build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", w, ".rds")
  )) 

  forestloss_yrs <- id_df %>% 
    left_join(fl_yrs, by = "country")
  
  summary_gaftc <- inner_join(data_gaftc, forestloss_yrs, by = "ipums_id") %>% 
    filter(year == fl_start_yr | year == fl_end_yr) %>%
    arrange(ipums_id, year) %>% 
    mutate(
      delta_avg_gaftc = ifelse(
        year == fl_end_yr, 
        (avg_gaftc - lag(avg_gaftc)) / 
          (fl_end_yr - fl_start_yr), 
        NA),
      # Log delta calculations
      delta_log_avg_gaftc = ifelse(
        year == fl_end_yr,
        log((avg_gaftc / lag(avg_gaftc)) ^
              (1 / (fl_end_yr - fl_start_yr))),
        NA)
    ) %>% 
    filter(year == fl_end_yr) %>% 
    select(ipums_id, fl_start_yr, fl_end_yr, avg_gaftc, 
           delta_avg_gaftc, delta_log_avg_gaftc)
  
  gaftc_waves[[w]] <- summary_gaftc
  
}

gaftc_wide <- bind_rows(gaftc_waves, .id = "wave")

saveRDS(gaftc_wide, here(out.dir, "R", "GAFTC_wide.rds"))
write_dta(gaftc_wide, here(out.dir, "GAFTC_wide.dta"))
