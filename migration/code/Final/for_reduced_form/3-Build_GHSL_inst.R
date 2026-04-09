#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    January 23, 2025
#* Title:   Clean GHSL builtup data for the reduced form 
#* Desc:    
#*******************************************************************************

source("code/SSA_env_SetUp.R")

builtup <- readRDS(here(build.dir, "AFRICA", "GHSL", "builtup_cleaned.rds"))

builtup_waves <- list()

for (w in c(1, 2)) {
  
  bu_yrs <- readRDS(here(
    build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", w, ".rds")
  )) 

  builtup_start <- builtup %>% 
    left_join(bu_yrs, by = "country") %>% 
    filter(year == bu_start_yr) %>% 
    rename_with(~paste0("bl_", .x), matches("total|res")) %>% 
    select(ipums_id, country, matches("total|res")) # also include country
  
  builtup_end <- builtup %>% 
    left_join(bu_yrs, by = "country") %>% 
    filter(year == bu_end_yr) %>% 
    rename_with(~paste0("el_", .x), matches("total|res")) %>% 
    select(ipums_id, matches("total|res")) 
  
  builtup_mixed <- full_join(builtup_start, builtup_end, by = "ipums_id") %>%
    left_join(bu_yrs, by = "country") %>% 
    filter(bu_start_yr != bu_end_yr) %>% # removes madagascar as 2018 is attributed to 2020
    mutate(
      delta_buvtotal = (el_buvtotal - bl_buvtotal) / bu_prd_len,
      delta_buvnres = (el_buvnres - bl_buvnres) / bu_prd_len,
      delta_buatotal = (el_buatotal - bl_buatotal) / bu_prd_len,
      delta_buanres = (el_buanres - bl_buanres) / bu_prd_len,
      wave = w
    )
  
  builtup_waves[[w]] <- builtup_mixed
}

builtup_rf <- bind_rows(builtup_waves, .id = "wave") %>% 
  mutate(wave = as.integer(wave))

rm(builtup_mixed, builtup_waves, builtup_end, builtup_start)

write_dta(builtup_rf, here(out.dir, "builtup_rf.dta"))