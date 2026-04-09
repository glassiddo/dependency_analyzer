#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    April 1, 2023
#* Title:   Crop Data
#* Desc:    Build Crop Dataset for each admin unit that we use
#* 1 hectare = 10,000 sq m = 1x10^4 sq m
#*         1 km^2 = 100 hectare = 1x10^6 sq m
#*         New countries: Cameroon, Malawi, Rwanda, South Sudan, Sudan, Togo, Zimbabwe
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

f_croparea <- function(ctry_str, lvl = 2, yr_start, yr_end) {
  
  if (lvl == 2) { geo <- sym("district_name") }
  else { geo <- sym("region_name") }
  
  ipums <- sym(paste0("geolevel", lvl))
  
  period_len <- yr_end - yr_start
  
  dat_df <- readRDS(
    here(build.dir, "Countries", ctry_str, "Support", 
         paste0("l", lvl, "_croparea.rds")
         )
    ) %>%
    mutate(
        # npix is number of 30mx30m units, mult by 900 to get num sq m 
        # make in terms of hectares from m^2 = / 10000
      total_area = npix * 900 / 10000,
      croparea = sharecrop * total_area,
      log_sharecrop = ifelse(sharecrop != 0, log(sharecrop), 
                               log(0.01/total_area)),
      log_croparea = ifelse(croparea != 0, log(croparea), log(0.01)),
      ipums_id = as.character({{ipums}})) %>%
    select({{geo}}, ipums_id, year, total_area, sharecrop, croparea, 
             log_sharecrop, log_croparea) 
  
  crop0_df <- dat_df %>%
    filter(year == yr_start) %>%
    rename(crop_start_yr = year, bl_croparea = croparea, 
           bl_log_croparea = log_croparea, bl_sharecrop = sharecrop, 
           bl_log_sharecrop = log_sharecrop)
  
  crop1_df <- dat_df %>%
    filter(year == yr_end) %>%
    rename(crop_end_yr = year, el_croparea = croparea, 
           el_log_croparea = log_croparea, 
           el_sharecrop = sharecrop, el_log_sharecrop = log_sharecrop) %>%
    select(-c(total_area))
  
  crop_df <- full_join(crop0_df, crop1_df) %>%
    mutate(
      country_name = ctry_str,
      crop_start_yr = yr_start,
      crop_end_yr = yr_end,
      crop_prd_len = period_len,
      # crop area
      delta_croparea = el_croparea - bl_croparea,
      delta_croparea_ann = delta_croparea/ period_len,
      delta_log_croparea = el_log_croparea - bl_log_croparea,
      delta_log_croparea_ann = delta_log_croparea / period_len,
      # share of area cropped
      delta_sharecrop = el_sharecrop - bl_sharecrop,
      delta_sharecrop_ann = delta_sharecrop / period_len,
      delta_log_sharecrop = (el_log_sharecrop - bl_log_sharecrop),
      delta_log_sharecrop_ann = delta_log_sharecrop/period_len) %>%
    rename(total_area_cropdat = total_area) %>%
    select(-c(el_log_sharecrop, bl_log_sharecrop, el_log_croparea, 
              bl_log_croparea))

}


#*******************************************************************************
# Benin 2 ----
ctry_df <- f_croparea("BEN", 2, 2003, 2011)
ca_df <- ctry_df

# Botswana 1 ----
ctry_df <- f_croparea("BWA", 1, 2003, 2011) 
ca_df <- bind_rows(ca_df, ctry_df)

# Burkina Faso ----
ctry_df <- f_croparea("BFA", 2, 2003, 2007) 
ca_df <- bind_rows(ca_df, ctry_df)

# Cameroon ----
ctry_df <- f_croparea("CMR", 2, 2003, 2007)   
ca_df <- bind_rows(ca_df, ctry_df)

# Ghana ----
ctry_df <- f_croparea("GHA", 2, 2003, 2011) 
ca_df <- bind_rows(ca_df, ctry_df)

# Guinea ----
ctry_df <- f_croparea("GIN", 1, 2003, 2015) 
ca_df <- bind_rows(ca_df, ctry_df)

# Ivory Coast ----
ctry_df <- f_croparea("CIV", 2, 1998) 
ca_df <- bind_rows(ca_df, ctry_df)

#ctry_df <- readRDS(
#  paste0(build.dir, "Guinea", "/Support/l", 2, "_croparea.rds"))

# Kenya ----
ctry_df <- f_croparea("KEN", 2, 2003, 2007) 
ca_df <- bind_rows(ca_df, ctry_df)

# Lesotho ----
ctry_df <- f_croparea("LSO", 1, 2003, 2007) 
ca_df <- bind_rows(ca_df, ctry_df)

# Malawi ----
ctry_df <- f_croparea("MWI", 1, 2007, 2015) 
ca_df <- bind_rows(ca_df, ctry_df)

# Mali ----
ctry_df <- f_croparea("MLI", 2, 2003, 2007) 
ca_df <- bind_rows(ca_df, ctry_df)

# Mauritius ----
ctry_df <- f_croparea("MUS", 1, 2003, 2011) 
ca_df <- bind_rows(ca_df, ctry_df)

# Mozambique ----
ctry_df <- f_croparea("MOZ", 2, 2003, 2007) 
ca_df <- bind_rows(ca_df, ctry_df)

# Rwanda ----
ctry_df <- f_croparea("RWA", 2, 2011, 2015) 
ca_df <- bind_rows(ca_df, ctry_df)

# Senegal ----
ctry_df <- f_croparea("SEN", 2, 2003, 2011) 
ca_df <- bind_rows(ca_df, ctry_df)

# Sierra Leone ----
ctry_df <- f_croparea("SLE", 2, 2003, 2015) 
ca_df <- bind_rows(ca_df, ctry_df)

# South Africa ----
ctry_df <- f_croparea("ZAF", 2, 2003, 2011) 
ca_df <- bind_rows(ca_df, ctry_df)

# Sudan & S. Sudan ----
ctry_df <- f_croparea("SDN", 1, 2007, 2015) 
ca_df <- bind_rows(ca_df, ctry_df)

# Tanzania ----
ctry_df <- f_croparea("TZA", 1, 2003, 2011) 
ca_df <- bind_rows(ca_df, ctry_df)

# Togo ----
ctry_df <- f_croparea("TGO", 2, 2011, 2015) 
ca_df <- bind_rows(ca_df, ctry_df)

# Uganda ----
ctry_df <- f_croparea("UGA", 1, 2003, 2015) 
ca_df <- bind_rows(ca_df, ctry_df)

# Zambia ----
ctry_df <- f_croparea("ZMB", 2, 2003, 2011) 
ca_df <- bind_rows(ca_df, ctry_df)

# Zimbabwe ----
ctry_df <- f_croparea("ZWE", 2, 2011, 2015) 
ca_df <- bind_rows(ca_df, ctry_df)

# save ----
id_df <- readRDS(file.path(out.dir, "R", "id.rds"))

ca_df %<>%
  inner_join(id_df) %>%
  relocate(country_name, ipums_id, admin_name, admin_id, geo_lvl, 
           crop_start_yr, crop_end_yr, crop_prd_len) %>%
  relocate(district_name, region_name, .after = last_col()) %>%
  var_labels(
    total_area_cropdat = "Total area, crop data measure",
    bl_sharecrop = "Share of area cropped, baseline",
    bl_croparea = "area cropped, hectares baseline",
    el_sharecrop = "Share of area cropped, endline",
    el_croparea = "area cropped, hectares endline",
    crop_start_yr = "crop data baseline year",
    crop_end_yr = "crop data endline year",
    crop_prd_len = "crop data period length",
    delta_croparea = "change in cropped area, hectares",
    delta_croparea_ann = "change in cropped area, hectares annual",
    delta_log_croparea = "change in log cropped area, log-hectares",
    delta_log_croparea_ann = "change in log cropped area, log-hectares annual",
    delta_sharecrop = "change in share of area cropped",
    delta_sharecrop_ann = "change in share of area cropped, annual",
    delta_log_sharecrop = "change in log share of area cropped",
    delta_log_sharecrop_ann = "change in share of area cropped, annual",
  )

saveRDS(ca_df, file.path(out.dir, "R/croparea.rds"))
write_dta(ca_df, file.path(out.dir, "croparea.dta"))

