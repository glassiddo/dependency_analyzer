#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    January 23, 2025
#* Title:   Clean GHSL builtup data for the model
#* Desc:    
#*******************************************************************************

source("code/SSA_env_SetUp.R")

builtup <- readRDS(here(build.dir, "AFRICA", "GHSL", "builtup_cleaned.rds"))

builtup_for_model <- builtup %>% 
  filter(year %in% c(1980, 1990, 2000, 2010)) %>%
  select(country, ipums_id, admin_name, geotype, areaha, year, 
         buanres, buatotal, buvnres, buvtotal) %>%
  labelled::set_variable_labels(
    country = "Country",
    admin_name = "Admin unit",
    ipums_id = "IPUMS ID",
    geotype = "Admin unit shape",
    areaha = "Area (ha)",
    year = "Year",
    buatotal = "Built-up area (Total, ha)",
    buanres = "Built-up area (Non-Residendial, ha)",
    buvtotal = "Built-up volume (Total, m3)",
    buvnres = "Built-up volume (Non-Residendial, m3)"
  )

write_dta(builtup_for_model, 
          here(build.dir, "AFRICA", "GHSL", "builtup.dta"))