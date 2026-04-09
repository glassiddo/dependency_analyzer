#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    October 23, 2025
#* Title:   MODIS LC growth
#* Desc:    Process MODIS LC values 
#* Note:    Uses data from cultivableland.do
#*******************************************************************************

source("code/SSA_env_SetUp.R")

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country)

df_waves <- list()

layers <- c("Prop2", "Type1", "Type2", "Type3", "Type4", "Type5")
# to replicate old file (in Archive), need to divide by areaha (and get shrubs, grass)

for (w in 1:2) {
  # read data of all lc types
  df <- map(
    layers, ~read_dta(here(build.dir, "Africa", "MODIS_LC", paste0("sampleUnitslc_", .x, ".dta"))) %>%
      select(-admin_name, -areaha, -country, -geotype)
    ) %>%
    reduce(left_join, by = c("ipums_id", "year")) %>% 
    mutate(
      ipums_id = ifelse(str_length(ipums_id) == 5, paste0('0', ipums_id), ipums_id)
    )
    
  # get first and last year
  fl_yrs <- readRDS(here(
    build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", w, ".rds")
  )) 

  forestloss_yrs <- id_df %>% 
    left_join(fl_yrs, by = "country")
  
  df <- df %>% 
    left_join(forestloss_yrs, by = "ipums_id") %>% 
    filter(year == fl_start_yr | year == fl_end_yr) %>%
    arrange(ipums_id, year)
  
  lc_vars <- grep("lc", names(df), value = TRUE)
  
  # calculate delta variables for all
  df <- df %>%
    group_by(ipums_id) %>%
    mutate(
      across(
        all_of(lc_vars),
        list(
          bl = ~ifelse(year == fl_end_yr, lag(.), NA),
          delta = ~ifelse(year == fl_end_yr, 
                          (. - lag(.)) / (fl_end_yr - fl_start_yr), 
                          NA),
          delta_log = ~ifelse(year == fl_end_yr & lag(.) > 0 & . > 0,
                              log((. / lag(.))^(1 / (fl_end_yr - fl_start_yr))),
                              NA)
        ),
        .names = "{.fn}_{.col}"
      )
    ) %>%
    ungroup()
  
  # keep only end year obs
  df <- df %>%
    filter(year == fl_end_yr) %>%
    select(-contains("year"), -contains("yr"))  
  
  df_waves[[w]] <- df
}

df_combined <- bind_rows(df_waves, .id = "wave") %>%
  mutate(wave = as.numeric(wave))

write_dta(df_combined, here(out.dir, paste0("MODIS_LC_wide.dta")))
saveRDS(df_combined, here(out.dir, "R", paste0("MODIS_LC_wide.rds")))
