#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    November 17, 2025
#* Title:   Merge Sudan and South Sudan WDI
#* Desc:    SSDN and SDN are separate in WDI but we treat them both as a SDN
#* Note:    Hence, get weighted averages. Should idelaly be in the stata code
#*******************************************************************************

source("code/SSA_env_SetUp.R")

wdi <- read_dta("data/Build/World/WDI/WDI_Clean_b4_sdn.dta") 

wdi_sdn_ssd <- wdi %>% 
  filter(country %in% c("SDN", "SSD"))

wdi_sdn_merged <- wdi_sdn_ssd %>% 
  group_by(year) %>% 
  mutate(
    w_pop = pop / sum(pop, na.rm = T),
    w_area = ag_lnd_totl_k2 / sum(ag_lnd_totl_k2, na.rm = T),
  ) %>% 
  ungroup() %>% 
  group_by(year) %>%
  summarise(
    # get a weighted average, with the weights being:
    ## area
    across(c(forest, agr), ~ if (all(is.na(.x))) NA_real_ else 
      sum(w_area * .x, na.rm = TRUE)),
    ## population
    across(c(urb_pop, EmpltoPopRatio, EmplShareAg, EmplShareInd, EmplShareServ),
           ~ if (all(is.na(.x))) NA_real_ else sum(w_pop * .x, na.rm = TRUE)), 
    # simply sum both countries
    across(c(pop, aid, gdp, gdp_cd, ag_lnd_totl_k2, gdp_ag_cd, gdp_ind_cd,
             gdp_serv_cd, gdp_ppp, gdp_ag, gdp_ind, gdp_serv),
           ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE))
    ) %>% 
  ungroup() %>% 
  mutate(
    gdp_pc = gdp / pop,
    gdp_pc_cd = gdp_pc / pop,
    gdp_ag_ppp = gdp_ppp*gdp_ag_cd/gdp_cd,
    gdp_serv_ppp=gdp_ppp*gdp_serv_cd/gdp_cd,
    gdp_ind_ppp=gdp_ppp*gdp_ind_cd/gdp_cd
  ) %>% 
  mutate(country = "SDN") 

cols_to_merge <- names(wdi_sdn_merged) %>% 
    setdiff(c("year", "country"))
  
wdi_sdn <- wdi_sdn_ssd %>% 
  filter(country == "SDN") %>% 
  select(-all_of(cols_to_merge)) %>% 
  left_join(wdi_sdn_merged, by = c("year", "country"))
  
wdi_final <- wdi %>% 
  filter(!country %in% c("SDN", "SSD")) %>% 
  copy_labels(wdi) %>%  # preserve labels
  bind_rows(wdi_sdn) %>% # merge with new sudan 
  copy_labels(wdi) %>%  # keep the labels %>% 
  rename( # to match the current version of names in the model
    country_code = country,
    countryname = country_name,
    regionname = region,
    incomelevel = income,
    lendingtype = lending
  ) %>% 
  select(-c(status, lastupdated, capital, longitude, latitude, iso2c))

write_dta(wdi_final, "data/Build/World/WDI/WDI Clean.dta")
