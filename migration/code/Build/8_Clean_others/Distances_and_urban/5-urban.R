#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    April 5, 2023
#* Title:   Urban
#* Desc:    Build urban area Dataset for each admin unit that we use
#* 1 hectare = 10,000 sq m = 1x10^4 sq m
#*         1 km^2 = 100 hectare = 1x10^6 sq m
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

#*******************************************************************************

read_urb <- function(ctry_str, lvl = 2) {
  
  lvl <- census_info %>% filter(country == ctry_str) %>% pull(lvl)
  
  out_df <- readRDS(
    here(
      build.dir, "Countries", ctry_str, "Support", 
      paste0("cons_l", lvl,"_urban.rds")
    )
  )
}

# combine ----
countries <- census_info %>% pull(country)

urb_df <- map_dfr(countries, read_urb) %>% 
  select(-region_name, -district_name)

# save ----
id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country) # doesnt include lake kivu+malawi

urb_df %<>%
  inner_join(id_df, by = c("ipums_id")) %>%
  var_labels(
    total_area = "total area, hectares",
    urban_area = "urban area, hectares",
    urban_share = "urban area, share of total"
  )

saveRDS(urb_df, here(out.dir, "R", "urban.rds"))
write_dta(urb_df, here(out.dir, "urban.dta"))

