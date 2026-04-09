source("code/SSA_env_SetUp.R")

# TODO
# make using gee slightly easier (all files copied into a single folder?)
# verify that no other files are necessary here
# fix stata files:
## agpotentialyield.do, Clean_trade.do, bartiks.do, cleaning_gee_nl_files.do
# later - add descriptives (inc figures), and after that rf regs, modeling etc.

run_r <- function(script) {
  cat("\n--- Running R:", script, "\n")
  source(script, echo = TRUE)
}

run_stata <- function(do_file) {
  
  do_file <- normalizePath(do_file, winslash = "/", mustWork = FALSE)
  if (!file.exists(do_file)) stop(paste("File not found:", do_file)) else{
    cat("\n--- Running Stata:", basename(do_file), "\n")
  }
  stata_exe <- '"C:/Program Files/StataNow19/StataMP-64.exe"' # need to be changed per user
  
  wrapper_content <- c(
    paste0('global projdir  "', normalizePath(getwd(), winslash = "/"), '"'),
    paste0('global rawdata  "', normalizePath(raw.dir, winslash = "/"), '"'),
    paste0('global build    "', normalizePath(build.dir, winslash = "/"), '"'),
    paste0('global final    "', normalizePath(out.dir, winslash = "/"), '"'),
    #paste0('global model    "', normalizePath(model.dir, winslash = "/"), '"'),
    paste0('do "', do_file, '"'),
    # might need to add code
    "exit, clear"  
  )
  
  temp_wrapper <- tempfile(fileext = ".do")
  writeLines(wrapper_content, temp_wrapper)
  
  system(paste(stata_exe, "/e do", shQuote(temp_wrapper)), wait = TRUE)
  if (file.exists(temp_wrapper)) file.remove(temp_wrapper)
}


# 0_LSMS - need to see what's exactly needed there
# run_stata(here(build.code.dir, "0_LSMS", "0_Master.do"))

# 1_Census ----- ideally comment this out unless making a change here ------
# This skips scraping the datasets; it assumes they already exist in your folders

# run_stata(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "AGO", "1-clean_angola_census_part1.do"))
# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "AGO", "2-clean_angola_census_part2.R"))
# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "AGO", "3-ago_clean_sf.R"))

# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "ETH_NGA", "1a-eth_clean_sf.R"))
# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "ETH_NGA", "1b-nga_clean_sf.R"))
# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "ETH_NGA", "2a-eth_clean_to_census_style.R"))
# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "ETH_NGA", "2b-nga_clean_to_census_style.R"))
# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "ETH_NGA", "3-mig_eth_nga.R"))

# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "GAB", "1-gab_clean_sf.R"))

# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "MDG", "1-mdg_clean_sf.R"))
# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "MDG", "2a-mdg_clean_first_census.R"))
# run_r(here(build.code.dir, "1_Census", "1-clean_non_IPUMS_ctries", "MDG", "2b-mdg_clean_second_census.R"))

# run_r(here(build.code.dir, "1_Census", "2-XC_census_year", "0-XC_census_year.R"))

# run_r(here(build.code.dir, "1_Census", "3-zaf_specific", "1-ZAF-IPUMS-xwalk.R"))
# run_r(here(build.code.dir, "1_Census", "3-zaf_specific", "2-ZAF_census.R"))

# run_r(here(build.code.dir, "1_Census", "4-census_ipums_xwalk", "0-census-ipums-xwalk.R"))
# # still not sure exactly what it does and it's not up to date


# 2_Create_unit_IDs
run_r(here(build.code.dir, "2_Create_unit_IDs", "0-Create_unit_IDs.R"))

# 3_Maps - perhaps should be just before the gee as it's a more direct continuation
run_r(here(build.code.dir, "3_Maps", "0-create_new_masters_map.R"))
run_r(here(build.code.dir, "3_Maps", "1-build_centroids.R"))
run_r(here(build.code.dir, "3_Maps", "2-SampleUnit-SF.R"))

# ========================== #
# 1. Upload the sample units to GEE as an asset (from the shapefiles)
# 2. Call it for example like the following, with the current date
# projects/ee-himww/assets/SampleUnits070126
# 3. Then replace the name of var sampleUnits in the first line of this file:
# users/houngbedjiken/afrmigdefo:ADMUNIT/00_MASTER_FILE

# ========================== #

# 4_Set_time_windows
run_r(here(build.code.dir, "4_Set_time_windows", "0-Set_time_windows.R"))

# 5_Emp_pop_mig
run_r(here(build.code.dir, "5_Emp_pop_mig", "1-gabon_get_aggs", "1-gab_clean_census1.R"))
run_r(here(build.code.dir, "5_Emp_pop_mig", "1-gabon_get_aggs", "2-gab_clean_census2.R"))
run_r(here(build.code.dir, "5_Emp_pop_mig", "2-Population.R"))
run_r(here(build.code.dir, "5_Emp_pop_mig", "3-Employment.R"))
run_r(here(build.code.dir, "5_Emp_pop_mig", "4A-full_migration_census1.R"))
run_r(here(build.code.dir, "5_Emp_pop_mig", "4B-full_migration_census2.R"))

# WDI and GDP
# the file generated in gdp.do is used in trade;
# the file generated in the merge_sdn.... is used in nightlight (and in 3-gdp.do)
run_r(here(build.code.dir, "6_Clean_perlim_dfs", "WDI_GDP", "0-Get_wdi_data.R"))
run_stata(here(build.code.dir, "6_Clean_perlim_dfs", "WDI_GDP", "1-Clean_WDI.do"))
run_r(here(build.code.dir, "6_Clean_perlim_dfs", "WDI_GDP", "2-merge_SDN_SSD_WDI.R"))
run_stata(here(build.code.dir, "6_Clean_perlim_dfs", "WDI_GDP", "3-gdp.do"))

# ==========================================================
# GEE - manual export required
# ==========================================================

cat("\n==============================\n")
cat(" GEE INPUT REQUIREMENT\n")
cat("==============================\n")

cat("
The following modules depend on Google Earth Engine exports.

You must run the relevant GEE JavaScript code in the Code Editor
and export outputs into the expected raw data folders BEFORE
running the cleaning scripts below.

The necessary files to run are:
users/houngbedjiken/afrmigdefo:ADMUNIT/00_MASTER_FILE
users/houngbedjiken/afrmigdefo:WORLD/00_MASTER_FILE

Run by following those steps:
1. When located in each of the two files above, press the Enter Key
2. Initiate all the tasks that appear in the Tasks window
3. This will generate the assets (csv files) in AFRMIGDEFO folder in Google Drive
4. Copy those files to the relevant locations in our local folders

Done!
\n")

run_r(here(build.code.dir, "7_Clean_from_gee", "Crop_area", "crop_area.R"))
run_r(here(build.code.dir, "7_Clean_from_gee", "Forest", "gee_cleaning_to_long.R"))
run_r(here(build.code.dir, "7_Clean_from_gee", "Geo_controls", "clean_geo_ctrls.R"))
run_r(here(build.code.dir, "7_Clean_from_gee", "GHSL", "clean_builtup.R"))
run_r(here(build.code.dir, "7_Clean_from_gee", "MODIS_LC", "process_modis_lc.R"))
run_stata(here(build.code.dir, "7_Clean_from_gee", "NightLight", "cleaning_gee_nl_files.do"))
run_r(here(build.code.dir, "7_Clean_from_gee", "TMF", "tmf.R"))

# gaez - the code might needs cleaning
run_stata(here(build.code.dir, "7_Clean_from_gee", "GAEZ", "agvalue.do"))
run_stata(here(build.code.dir, "7_Clean_from_gee", "GAEZ", "agpotentialyield.do"))

### CLEANING FROM OTHER SOURCES ---

run_r(here(build.code.dir, "8_Clean_others", "Africopolis", "add_africapolis_data.R"))

# Mines
run_r(here(build.code.dir, "8_Clean_others", "Mines", "0_clean_artisanal_mines.R"))
run_r(here(build.code.dir, "8_Clean_others", "Mines", "1-clean-mines.R"))
run_r(here(build.code.dir, "8_Clean_others", "Mines", "2-read_prices.R"))
run_r(here(build.code.dir, "8_Clean_others", "Mines", "3-price_AR1.R"))

# Consumption
run_r(here(build.code.dir, "8_Clean_others", "Consumption", "0_cons_by_ctry.R"))

# Distances and urban
run_r(here(build.code.dir, "8_Clean_others", "Distances_and_urban", "1-build_dist_urb.R")) 
run_r(here(build.code.dir, "8_Clean_others", "Distances_and_urban", "2-scrape_travel_time.R"))
run_r(here(build.code.dir, "8_Clean_others", "Distances_and_urban", "3-distance.R"))
run_r(here(build.code.dir, "8_Clean_others", "Distances_and_urban", "4A-full_distance.R"))
run_r(here(build.code.dir, "8_Clean_others", "Distances_and_urban", "4B-osm-distance.R"))
run_r(here(build.code.dir, "8_Clean_others", "Distances_and_urban", "5-urban.R"))

# ESA - files 1 cannot be ran right now cause data isn't located here
# run_r(here(build.code.dir, "8_Clean_others", "ESA", "1a_pft.R"))
# run_r(here(build.code.dir, "8_Clean_others", "ESA", "1b_esa.R"))
run_r(here(build.code.dir, "8_Clean_others", "ESA", "2_clean_esa_pft.R"))

run_stata(here(build.code.dir, "8_Clean_others", "Trade", "Clean_trade.do"))
# port_multiple_cities.csv is now created manually

# ==========================================================
# Final --------
# ==========================================================

run_stata(here(final.code.dir, "0-predicting_gdp_from_nl.do"))

## rf
run_r(here(final.code.dir, "for_reduced_form", "1-ForestLoss_for_rf.R"))
run_r(here(final.code.dir, "for_reduced_form", "2-NightLight_for_rf.R"))
run_r(here(final.code.dir, "for_reduced_form", "3-Build_GHSL_inst.R"))
run_r(here(final.code.dir, "for_reduced_form", "4-Build_mine_inst.R"))
run_stata(here(final.code.dir, "for_reduced_form", "5_bartiks.do"))
run_stata(here(final.code.dir, "for_reduced_form", "6_merge_data.do"))
run_stata(here(final.code.dir, "for_reduced_form", "7_clean_merged_data.do"))

## model
run_r(here(final.code.dir, "for_model", "1-ForestLoss_for_model.R"))
run_stata(here(final.code.dir, "for_model", "2-NightLight_for_model.do"))
run_r(here(final.code.dir, "for_model", "3-Builtup_for_model.R"))