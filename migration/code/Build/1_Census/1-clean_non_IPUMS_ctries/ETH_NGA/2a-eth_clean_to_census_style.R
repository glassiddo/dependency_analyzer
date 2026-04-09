#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    December, 2025
#* Title:   Create 'census' type structure from Ethiopia LSMS
#* Desc:    Clean employment, population (but not migration) from LSMS for ETH
#*******************************************************************************

source("code/SSA_env_SetUp.R")

ctry_str <- "ETH"
ctries_dir <- "Countries"
lvl <- 1

eth_map <- read_sf(
  here(
    raw.dir, ctries_dir, ctry_str, "Shapefiles", paste0("level ", lvl), 
    paste0(ctry_str, "_adm", lvl, ".shp")
    )
)
  
region_labels <- eth_map %>% 
  st_drop_geometry() %>% 
  select(
    ADMIN_NAME, GEOLEVEL1
    ) %>%
  add_row(
    ADMIN_NAME = "Niu (Not In Universe)",
    GEOLEVEL1 = 231098,
    ) %>% 
  {setNames(.$GEOLEVEL1, .$ADMIN_NAME)}

df_eth <- read_dta(here(build.dir, "Africa", "LSMS", "INPUT_CLEAN", "Ethiopia.dta")) %>%
  filter(
    wave == 2
  ) %>% 
  mutate(
    ## o_region seems to represent the region where survey took place (?)
    ## but d_region the location where the person lives, but often empty
    ## hence, take d_region whenever possible, otherwise o_region
    o_region = case_when(
      nchar(o_region_cd) == 2 ~ paste0(2310, o_region_cd),
      nchar(o_region_cd) == 1 ~ paste0(23100, o_region_cd),
      TRUE ~ NA
    ),
    d_region = case_when(
      nchar(d_region_cd) == 2 ~ paste0(2310, d_region_cd),
      nchar(d_region_cd) == 1 ~ paste0(23100, d_region_cd),
      TRUE ~ NA
    ),
    geo1_et = case_when(
      !is.na(d_region) ~ d_region,
      TRUE ~ o_region
    )
  ) %>% 
  transmute(
    country = ctry_str, 
    year = 2014,
    age,
    ind_id,
    perwt = weight,
    urban = ifelse(o_is_urban == 1, 2, 1), # to align with others 
    empstat = case_when(
      wage_work == 1 | hh_farm_work == 1 ~ 1, # Employed,
      wage_work == 0 & hh_farm_work == 0 ~ 0, # need to make sure - i think it doesn't matter if it's 0/2/3
      TRUE ~ NA
    ),
    indgen = case_when(
      main_sector == 1 | hh_farm_work == 1 ~ 10, # agriculture and forestry
      main_sector == 2 ~ 10, # fishing/fish farms & related
      main_sector == 3 ~ 20, # mining
      main_sector == 4 ~ 30, # manufacturing
      main_sector == 5 ~ 40, # electricity/gas/steam/water supply
      main_sector == 6 ~ 50, # construction
      main_sector == 7 ~ 60, # retail, trade etc
      main_sector == 8 ~ 70, # hotels and resaurants
      main_sector == 9 ~ 80, # transportation/storage/comms
      main_sector == 10 ~ 90, # finances
      main_sector == 11 ~ 111, # real estate
      main_sector == 12 ~ 100, # public admin
      main_sector == 13 ~ 112, # education
      main_sector == 14 ~ 113, # health
      main_sector == 15 ~ 130, # other,
      main_sector == 16 ~ 120, # private hhs with employed persons - into 'private hh services'
      main_sector == 17 ~ 130, # NGOs, into 'others' - doesn't matter much as long as services
      main_sector == 18 ~ 130, # others
      TRUE ~ 0
    ),
    geo1_et = as.numeric(geo1_et)
  )

ctry_df <- df_eth %>%
  mutate(
    urban = haven::labelled(urban, 
                            labels = c(Rural = 1, Urban = 2),
                            label = "Urban vs Rural"),
    geo1_et = haven::labelled(geo1_et, 
                              labels = region_labels,
                              label = "Current region of residence"),
    empstat = haven::labelled(empstat,
                              labels = c(
                                "NIU (not in universe)" = 0,
                                "Employed" = 1,
                                "Unemployed" = 2,
                                "Inactive" = 3
                              ),
                              label = "Activity status (employment status)")
  )

saveRDS(ctry_df, here(raw.dir, ctries_dir, ctry_str, "consistent14.rds"))
