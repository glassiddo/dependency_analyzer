#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    January 16, 2026
#* Title:   Conflict, topography and climate 
#* Desc:    Get annual mean/median of conflict and climate, and constant topography data
#* Note:    
#*******************************************************************************
#*

source("code/SSA_env_SetUp.R")

full_df <- read_csv(here(
  raw.dir, "Africa", "Geo_controls", "sampleUnitsTopogConflict.csv")
)

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country)

constant_vars <- full_df %>% 
  select(ipums_id, elevation, slope, mean_ruggedness) %>% 
  distinct()

yearly_vars <- full_df %>% 
  select(ipums_id, year, precip, temp, conflict = conflict_count)

df_waves <- list()

for (w in 1:2) {
  # get the fl start and end years
  fl_yrs <- readRDS(here(
    build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", w, ".rds")
  )) 
  
  forestloss_yrs <- id_df %>% 
    left_join(fl_yrs, by = "country")
  
  summary_df <- inner_join(yearly_vars, forestloss_yrs, by = "ipums_id") %>% 
    filter(year >= fl_start_yr & year <= fl_end_yr) %>% # include year 0?
    group_by(ipums_id) %>%
      reframe(
        across(
          c(conflict, temp, precip),
          list(
            mean   = ~ mean(.x, na.rm = TRUE),
            median = ~ median(.x, na.rm = TRUE)
          ), # get annual means and medians
          .names = "{.col}_{.fn}"
        )
      )
    
  df_waves[[w]] <- summary_df %>% 
    left_join(constant_vars, by = "ipums_id")
}

geographic_controls <- bind_rows(df_waves, .id = "wave")

saveRDS(geographic_controls, here(out.dir, "R", "geo_controls.rds"))
write_dta(geographic_controls, here(out.dir, "geo_controls.dta"))
