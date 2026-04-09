# Project: Migration Africa
# Author:  Iddo Glass
# Date:    Sep 23, 2025
# Title:   Merge Forest Loss from GEE
# Desc:    Merge forest loss data downloaded from GEE for each admin unit
# Output:  
#*******************************************************************************


source("code/SSA_env_SetUp.R")

process_hansen_loss <- function(lvl, tc_df, loss_df) {

  ## read files - the tc is separate
  ## they are created in New_HansenTC_IPUMS1 and New_Hansen_IPUMS1 in GEE
  
  ## takes the dataset of the 2000 forest loss in each admin unit
  tc_file <- paste0(
    raw.dir, "Africa/Forest/0925 version/", tc_df, lvl, "_302000.csv"
    )
  
  ## takes the yearly forest loss in each admin unit
  hansen_file <- paste0(
    raw.dir, "Africa/Forest/0925 version/", loss_df, lvl, "_30.csv"
    )
  # pick geolevel column
  geolevel <- ifelse(lvl == 1, "GEOLEVEL1", "GEOLEVEL2")
  
  # read TC
  tc00 <- fread(tc_file, encoding = "Latin-1") %>%
    rename(tc2000 = sum) %>%
    select(CNTRY_NAME, ADMIN_NAME, CNTRY_CODE, !!sym(geolevel), starts_with("TC")) %>%
    arrange(CNTRY_NAME) %>%
    rename_with(str_to_lower)
  
  # read Hansen
  tc_loss <- fread(hansen_file, encoding = "Latin-1") %>%
    select(CNTRY_NAME, ADMIN_NAME, CNTRY_CODE, !!sym(geolevel), starts_with("TC")) %>%
    arrange(CNTRY_NAME) %>%
    rename_with(str_to_lower)
  
  # join based on geolevel (ipums code), cntry code and cntry name. 
  # there are countries (level0) without admin info, then it joins just based on country
  join_key <- ifelse(lvl == 1, "geolevel1", "geolevel2")
  
  # join
  full_join(tc00, tc_loss, by = c("cntry_name", "cntry_code", "admin_name", join_key))
}

hansen_loss_ipums1 <- process_hansen_loss(1, "TC_IPUMS", "Hansen_IPUMS")
hansen_loss_ipums2 <- process_hansen_loss(2, "TC_IPUMS", "Hansen_IPUMS")

simple_hansen_loss_ipums1 <- process_hansen_loss(
  1, "TC_simple_IPUMS", "simple_Hansen_IPUMS"
  )
simple_hansen_loss_ipums2 <- process_hansen_loss(
  2, "TC_simple_IPUMS", "simple_Hansen_IPUMS"
)

write_dta(hansen_loss_ipums1, 
  file.path(build.dir,"Africa/Forest/lossbyyear30_IPUMS1.dta"))
write_dta(hansen_loss_ipums2, 
  file.path(build.dir,"Africa/Forest/lossbyyear30_IPUMS2.dta"))

write_dta(simple_hansen_loss_ipums1, 
          file.path(build.dir,"Africa/Forest/simple_Hansen_IPUMS1.dta"))
write_dta(simple_hansen_loss_ipums2, 
          file.path(build.dir,"Africa/Forest/simple_Hansen_IPUMS2.dta"))
