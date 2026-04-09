#* Project: Migration Africa
#* Author:  Iddo Glass, rewritten from crop_area.do
#* Date:    October 23, 2025
#* Title:   Crop area
#* Desc:    Get crop area in each admin unit
#*******************************************************************************
# Set Up ----
source("code/SSA_env_SetUp.R")

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country)

df_waves <- list()

for (w in 1:2) {
  df <- fread(here(raw.dir, "Africa/Crop Area/IPUMSADMUNITCROPLAND.csv")) %>% 
    mutate(
      ipums_id = ifelse(str_length(ipums_id) == 5, paste0('0', ipums_id), ipums_id)
      ) %>% 
    select(ipums_id, starts_with("npix"), starts_with("shar"))
    
  fl_yrs <- readRDS(here(
    build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", w, ".rds")
  )) 

  forestloss_yrs <- id_df %>% 
    left_join(fl_yrs, by = "country")
  
  df <- df %>% 
    left_join(forestloss_yrs, by = "ipums_id") %>% 
    mutate(
      sharecrop_bl = sharcrop2003,
      start_year = 2003,
      sharecrop_el = sharcrop2003,
      end_year = 2003,
      fl_start_yr = as.numeric(fl_start_yr),
      fl_end_yr = as.numeric(fl_end_yr)
    )
  
  gap <- 1

  for (year in c(2007, 2011, 2015, 2019)) {
    
    sharecrop_col <- paste0("sharcrop", year)
    
    df <- df %>%
      mutate(
        sharecrop_bl = if_else(
          is.na(fl_start_yr) | fl_start_yr >= year - gap,
          .data[[sharecrop_col]],
          sharecrop_bl
        ),
        sharecrop_el = if_else(
          is.na(fl_end_yr) | fl_end_yr >= year - gap,
          .data[[sharecrop_col]],
          sharecrop_el
        ),
        start_year = if_else(fl_start_yr >= (year - gap), year, start_year),
        end_year = if_else(fl_end_yr >= (year - gap), year, end_year)
      )
  }
  
  # Compute growth variables
  df <- df %>%
    mutate(
      delta_sharecrop = (sharecrop_el - sharecrop_bl) / (end_year - start_year),
      gr_sharecrop = safe_divide(delta_sharecrop, sharecrop_bl)
    ) %>%
    select(ipums_id, bl_sharecrop = sharecrop_bl, delta_sharecrop, gr_sharecrop)
  
  df_waves[[w]] <- df
  
}

sharecrop_new <- bind_rows(df_waves, .id = "wave") %>%
  mutate(wave = as.numeric(wave)) 

saveRDS(sharecrop_new, here(out.dir, "R", "sharecrop_new.rds"))
write_dta(sharecrop_new, here(out.dir, "sharecrop_new.dta"))