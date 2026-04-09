#* Project: Migration Africa
#* Author:  Sam Marshall, edited by Iddo Glass
#* Date:    Nov 29, 2021, edited on Sep 16, 2025
#* Title:   Project Set-Up
#* Note:    Run this file first. Adjust global data paths          
#*******************************************************************************
pacman::p_load(
  tidyverse, magrittr, ggplot2, ggrepel, haven, readxl, here, purrr, fixest,
  broom, units, sjlabelled, labelled, tidyr, DescTools, sf, stringr, zoo, 
  data.table, jsonlite, tidyjson, janitor
  # , sandwich, lmtest, car, AER, xtable, multiwayvcov, ipumsr, spatstat
)
rm(list=setdiff(ls(), c("run_r","run_stata")))
options(scipen = 999)
#*******************************************************************************

# data paths ----
# set paths to data, build stubs first for easy manipulation
raw.dir <- "data/Raw/"
build.dir <- "data/Build/"
out.dir <- "data/Final/" 
model.dir <-"modeling/program/results/output/tab_final"

build.code.dir <- "code/Build"
final.code.dir <- "code/Final"

#*******************************************************************************
# functions used across files ----
'%!in%' <- function(x,y)!('%in%'(x,y))
select <- dplyr::select
g_sym <- function(lvl, r = "c", l_stub = "prev") {
  if (lvl == 2) {
    geo <- sym("district")
    prev_geo <- sym(paste(l_stub, "district", sep = "_"))
  }
  else {
    geo <- sym("region")
    prev_geo <- sym(paste(l_stub, "region", sep = "_"))
  }
  if (r == "c") {return(geo)}
  else {return(prev_geo)}
}

n_ctry <- function( ctry ) {
  if( ctry == "AGO") {return("Angola")}
  if( ctry == "BEN") {return("Benin")}
  if( ctry == "BWA") {return("Botswana")}
  if( ctry == "BFA") {return("Burkina Faso")}
  if( ctry == "CMR") {return("Cameroon")}
  if( ctry == "CIV") {return("Ivory Coast")}
  if( ctry == "EGY") {return("Egypt")}
  if( ctry == "ETH") {return("Ethiopia")}
  if (ctry == "GAB") {return("Gabon")}
  if( ctry == "GHA") {return("Ghana")}
  if( ctry == "GIN") {return("Guinea")}
  if( ctry == "KEN") {return("Kenya")}
  if( ctry == "LSO") {return("Lesotho")}
  if( ctry == "MDG") {return("Madagascar")}
  if( ctry == "MWI") {return("Malawi")}
  if( ctry == "MLI") {return("Mali")}
  if( ctry == "MUS") {return("Mauritius")}
  if( ctry == "MOZ") {return("Mozambique")}
  if (ctry == "NGA") {return("Nigeria")}
  if( ctry == "RWA") {return("Rwanda")}
  if( ctry == "SEN") {return("Senegal")}
  if( ctry == "SLE") {return("Sierra Leone")}
  if( ctry %in% c("SAF","ZAF")) {return("South Africa")}
  if( ctry == "SSD") {return("South Sudan")}
  if( ctry == "SDN") {return("Sudan")}
  if( ctry == "TZA") {return("Tanzania")}
  if( ctry == "TGO") {return("Togo")}
  if( ctry == "UGA") {return("Uganda")}
  if( ctry == "ZMB") {return("Zambia")}
  if( ctry == "ZWE") {return("Zimbabwe")}
}

g_ctry3 <- function( ctry ) {
  # if adding a new country, make sure that all variations of its name
  # appear in the first part here
  # so this function would work even if data is imported from a diff source
  # and we still want a match (e.g., Ivory Coast vs Cote d'Ivoire)
  # INCLUDES ALSO COUNTRIES NOT IN CENSUS
  if(ctry %in% c("Algeria")) {return("DZA")}
  if(ctry %in% c("Angola")) {return("AGO")}
  if(ctry %in% c("Benin")) {return("BEN")}
  if(ctry %in% c("Botswana")) {return("BWA")}
  if(ctry %in% c("Burkina Faso")) {return("BFA")}
  if(ctry %in% c("Burundi")) {return("BDI")}
  if(ctry %in% c("Cameroon")) {return("CMR")}
  if(ctry %in% c("Central African Republic", "CAR", "Central African Rep.",
                 "Central African Rep")) {return("CAF")}
  if(ctry %in% c("Chad")) {return("TCD")}
  if(ctry %in% c("Congo", "Republic of the Congo",  "Rep. Of the Congo",
                 "Congo Republic", "Congo (Republic)")) {return("COG")}
  if(ctry %in% c("Congo, DRC", "Congo DRC", "DRC", "Congo (Kinshasa)",
                 "Dem. Rep. Congo",
                 "Democratic Republic of the Congo")) {return("COD")}
  if(ctry %in% c("Cote D'Ivoire", "Côte D'Ivoire", "Cote d'Ivoire", 
                 "CÃ´te D'Ivoire", # can be read this way if not encoded properly
                 "Côte d'Ivoire", "Ivory Coast")) {return("CIV")}
  if(ctry %in% c("Djibouti")) {return("DJI")}
  if(ctry %in% c("Egypt")) {return("EGY")}
  if(ctry %in% c("Equatorial Guinea")) {return("GNQ")}
  if(ctry %in% c("Eritrea")) {return("ERI")}
  if(ctry %in% c("Ethiopia")) {return("ETH")}
  if(ctry %in% c("Gabon")) {return("GAB")}
  if(ctry %in% c("Ghana")) {return("GHA")}
  if(ctry %in% c("Guinea")) {return("GIN")}
  if(ctry %in% c("Guinea-Bissau", "Guinea Bissau")) {return("GNB")}
  if(ctry %in% c("Kenya")) {return("KEN")}
  if(ctry %in% c("Lesotho")) {return("LSO")}
  if(ctry %in% c("Liberia")) {return("LBR")}
  if(ctry %in% c("Libya")) {return("LBY")}
  if(ctry %in% c("Madagascar")) {return("MDG")}
  if(ctry %in% c("Malawi")) {return("MWI")}
  if(ctry %in% c("Mali")) {return("MLI")}
  if(ctry %in% c("Mauritania")) {return("MRT")}
  if(ctry %in% c("Mauritius")) {return("MUS")}
  if(ctry %in% c("Morocco")) {return("MAR")}
  if(ctry %in% c("Mozambique")) {return("MOZ")}
  if(ctry %in% c("Namibia")) {return("NAM")}
  if(ctry %in% c("Niger")) {return("NER")}
  if(ctry %in% c("Nigeria")) {return("NGA")}
  if(ctry %in% c("Rwanda")) {return("RWA")}
  if(ctry %in% c("Senegal")) {return("SEN")}
  if(ctry %in% c("Sierra Leone")) {return("SLE")}
  if(ctry %in% c("Somalia")) {return("SOM")}
  if(ctry %in% c("South Africa")) {return("ZAF")}
  if(ctry %in% c("South Sudan", "S. Sudan")) {return("SSD")}
  if(ctry %in% c("Sudan")) {return("SDN")}
  if(ctry %in% c("Swaziland", "Eswatini", "eSwatini")) {return("SWZ")}
  if(ctry %in% c("Tanzania", "United Republic of Tanzania")) {return("TZA")}
  if(ctry %in% c("The Gambia", "Gambia", "The gambia")) {return("GMB")}
  if(ctry %in% c("Togo")) {return("TGO")}
  if(ctry %in% c("Tunisia")) {return("TUN")}
  if(ctry %in% c("Uganda")) {return("UGA")}
  if(ctry %in% c("Zambia")) {return("ZMB")}
  if(ctry %in% c("Zimbabwe")) {return("ZWE")}
  
  return(NA_character_)  # if none, return NA
}

census_info <- fread(here("data", "census_years.csv")) %>% 
  mutate(
    yr1_suffix = str_sub(census_yr1, -2),
    yr2_suffix = str_sub(census_yr2, -2),
  )

# # write it as a csv with countrycode package to match between IPUMS and iso3
# ipums_df <- enframe(ipums_country_codes, name = "country", value = "ipums_code") %>%
#   mutate(
#     iso3 = as.character(
#       countrycode::countrycode(country, origin = "country.name", destination = "iso3c")
#     )
#   ) %>%
#   select(-country) %>%
#   unique()
# fwrite(ipums_df,
#        paste0(raw.dir, "/Africa/Night Light/ipums_iso3_codes.csv"))

# rename district/region for benin files
ren_ben <- function(dat_df, lvl) {
  if (lvl == 1) {
    dat_df %<>%
      mutate(across(contains("region_name"),
                    ~ifelse(.x == "Atacora", "Atakora", .x)))
  }
  if (lvl == 2) {
    dat_df %<>%
      mutate(across(contains("district_name"), ~str_to_sentence(.x)),
             across(contains("district_name"), 
                    ~case_when(
                      .x == "Adjarra" ~ "Adjara",
                      .x == "Abjohoun" ~ "Adjohoun",
                      .x == "Aguegues" ~ "Aguegue",
                      .x == "Banikorara" ~ "Banikoara",
                      .x == "Cobly" ~ "Kobli",
                      .x == "Contonou" ~ "Cotonou",
                      .x == "Copargo" ~ "Kopargo",
                      .x == "Crand-popo" ~ "Grand-popo",
                      .x == "Dassa-zoume" ~ "Dassa",
                      .x == "Dogbo" ~ "Dogbo-tota",
                      .x == "N'dali" ~ "Ndali",
                      .x == "Penhunco" ~ "Pehonko",
                      .x == "Port-novo" ~ "Porto-novo",
                      .x == "Taqnguiete" ~ "Tanguieta",
                      .x == "Toucountouna" ~ "Toukountouna",
                      .x == "Zagnanado" ~ "Zangnanado",
                      .x == "Zogbodomey" ~ "Zogbodome",
                      TRUE ~ .x)))
  }
  return(dat_df)
}


# rename Mozambique
ren_moz <- function(dat_df, lake = TRUE) {
  dat_df %<>%
    mutate(
      across(contains("district_name"), 
             ~case_when(
               substr(.x, 1, 15) == "Distrito Urbano"~ "Maputo City",
               .x == "Matutuíne" ~"Matutuane",
               .x %in% c("Morrumbene", "Maxixe")~  "Morrumbene, Maxixe",
               .x %in% c("Nacala-Porto", "Nacala-Velha") 
               ~"Nacala-Porto, Nacala-Velha",
               TRUE~.x)))
  
    if (lake == FALSE) {
      drop_list <- c("Aeroporto", "Lake Malawi")
      
      if ("prev_district_name" %in% names(dat_df)) {
        dat_df %<>% 
          filter(!(district_name %in% drop_list | prev_district_name %in% drop_list))
      }
      else {
        dat_df %<>% filter(!(district_name %in% drop_list))
      }
    }
  
  return(dat_df)
}

# consistent ipums id for mozambique
moz_id <- function(dat_df, d_id, i_id) {
  dat_df %<>% group_by({{d_id}}) %>%
    mutate({{i_id}} := min({{i_id}})) %>% ungroup() 
}

# divide that wont get infinite 
safe_divide <- function(numerator, denominator, replace_with = NA_real_) {
  # replaces Inf and NaN results of division into NA
  result <- numerator / denominator
  result[is.infinite(result) | is.nan(result)] <- replace_with
  return(result)
} 

## set prime ages - used in population, employment and migration codes
min_pa <- 15 # minimum prime_age - included
max_pa <- 64 # maximum prime_age - included