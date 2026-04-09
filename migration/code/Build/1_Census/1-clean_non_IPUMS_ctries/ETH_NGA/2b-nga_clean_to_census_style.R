#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    December, 2025
#* Title:   Create 'census' type structure from Nigeria LSMS
#* Desc:    Clean employment, population (but not migration) from LSMS for NGA
#*******************************************************************************

source("code/SSA_env_SetUp.R")

ctry_str <- "NGA"
ctries_dir <- "Countries"
lvl <- 1

nga_map <- read_sf(
  here(
    raw.dir, "Countries", ctry_str, "Shapefiles", paste0("level ", lvl), 
    paste0(ctry_str, "_adm", lvl, ".shp")
  )
)

region_labels <- nga_map %>% 
  st_drop_geometry() %>%  
  add_row(
    ADMIN_NAME = "Niu (Not In Universe)",
    GEOLEVEL1 = 566098,
  ) %>% 
  {setNames(.$GEOLEVEL1, .$ADMIN_NAME)}

ctry_df <- read_dta(here(build.dir, "Africa/LSMS/INPUT_CLEAN/Nigeria.dta")) %>% 
  filter(wave == 1) %>%
  transmute(
    country = "NGA", 
    year = 2010,
    hh_id, ind_id,
    perwt = weight,
    urban = ifelse(o_is_urban == 1, 2, 1), # to align with others 
    age = ifelse(is.na(age), 999, age), 
    # there are very few unreasonable ages
    # e.g, 103, 110 etc. but just ~30 observations and w/ prime_age=F it's fine
    empstat = case_when(
      any_work == 1 ~ 1, # employed
      any_work == 2 & searching_for_job == 1 & available_for_job == 1 ~ 2, 
      # unemployed
      any_work == 2 & searching_for_job == 2 ~ 3, # not looking - inactive
      any_work == 2 & available_for_job == 2 ~ 3, # unavailable - inactive
      TRUE ~ 0 # NIU
    ),
    indgen = case_when(
      ## missing hotels and restaurants (70), 
      ## Business services and real estate (111), 
      ## has a single category for professional/scientific activities (4)
      ## but given that we're just interested in manufacturing/mining/service/ag,
      ## can just label those as 'others'
      main_sector == 1 ~ 10, # agriculture and forestry
      main_sector == 2 ~ 20, # mining
      main_sector == 3 ~ 30, # manufacturing
      main_sector == 4 ~ 130, # professional/scientific - to other
      main_sector == 5 ~ 40, # electricity
      main_sector == 6 ~ 50, # construction
      main_sector == 7 ~ 80, # transportation
      main_sector == 8 ~ 60, # retail
      main_sector == 9 ~ 90, # finances
      main_sector == 10 ~ 120, # personal services
      main_sector == 11 ~ 112, # education
      main_sector == 12 ~ 113, # health
      main_sector == 13 ~ 100, # public admin
      main_sector == 14 ~ 130, # other
      TRUE ~ 0  # default
    ),
    geo1_ng = as.numeric(paste0(5660, o_state_cd))
  ) %>% 
  mutate(
    urban = haven::labelled(urban, 
                            labels = c(Rural = 1, Urban = 2),
                            label = "Urban vs Rural"),
    geo1_ng = haven::labelled(geo1_ng, 
                              labels = region_labels,
                              label = "Current state of residence"),
    empstat = haven::labelled(empstat,
                              labels = c(
                                "NIU (not in universe)" = 0,
                                "Employed" = 1,
                                "Unemployed" = 2,
                                "Inactive" = 3
                              ),
                              label = "Activity status (employment status)")
  )

write_dta(ctry_df, here(raw.dir, ctries_dir, ctry_str, "consistent12.dta"))
