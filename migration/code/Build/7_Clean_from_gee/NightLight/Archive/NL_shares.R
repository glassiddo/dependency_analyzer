#* Project: Migration Africa
#* Author:  Iddo Glass (rewritten from stata, original by Kenneth (?))
#* Date:    October 16, 2025
#* Title:   Night Light Shares
#* Desc:    Get nightlight data from DMSP and VIIRS
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")
nl_raw_dir <- here(raw.dir, "Africa", "Night Light")
nl_build_dir <- here(build.dir, "Africa", "Night Light")

# dmsp
dmsp <- fread(
  here(nl_raw_dir, "sampleUnitsDmspNightLight.csv")
  )

# reshape twice, first for years and then for satellites
dmsp_long <- dmsp %>%
  pivot_longer(
    cols = starts_with("ntlpf") | starts_with("ntllpf") | 
      starts_with("ntllaf") | starts_with("ntlvf"),
    names_to = c(".value", "year"),
    names_pattern = "(.*)y([0-9]{4})"
  ) %>%
  pivot_longer(
    cols = c(ntlpf10, ntlpf12, ntlpf14, ntlpf15, ntlpf16, ntlpf18,
             ntllpf10, ntllpf12, ntllpf14, ntllpf15, ntllpf16, ntllpf18,
             ntllaf10, ntllaf12, ntllaf14, ntllaf15, ntllaf16, ntllaf18,
             ntlvf10, ntlvf12, ntlvf14, ntlvf15, ntlvf16, ntlvf18),
    names_to = c(".value", "satellite"),
    names_pattern = "(ntl[plav]+)f([0-9]{2})"
  ) %>%
  mutate(
    year = as.numeric(year),
    satellite = as.numeric(satellite)
  )

# viirs
viirs <- fread(here(
  nl_raw_dir, "sampleUnitsViirsNightLight.csv")
  )

# reshape
viirs_long <- viirs %>%
  pivot_longer(
    cols = starts_with("ntlp") | starts_with("ntllp") | starts_with("ntlla") | starts_with("ntlv"),
    names_to = c(".value", "year"),
    names_pattern = "(ntl[plav]+)([0-9]{4})"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year != 2012) %>%
  mutate(satellite = 20)

nl_shares <- bind_rows(dmsp_long, viirs_long) %>% 
  rename(country_name = country) %>% 
  mutate(
    country = sapply(country_name, g_ctry3) # get iso3 codes
  ) %>% 
  group_by(country, satellite, year) %>%
  mutate(
    ntlv_tot = sum(ntlv, na.rm = TRUE),
    ntlv_shr = ntlv / ntlv_tot,
    ntllp_tot = sum(ntllp, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  group_by(country, year) %>%
  mutate(ntllp_tot_max = max(ntllp_tot, na.rm = TRUE)) %>%
  arrange(country_name, country, ipums_id, year, ntllp_tot) %>%
  group_by(country_name, country, ipums_id, year) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(country_name, country, admin_name, ipums_id, areaha, 
         year, ntlv_shr, ntlv, ntlla) %>% 
  # for consistency with the 0_build_datasets.do
  rename(country = country_name, country_code = country) %>%  
  arrange(country, country_code, admin_name, ipums_id, year)

attr(nl_shares$ipums_id, "label") <- "IPUMS ID"
attr(nl_shares$admin_name, "label") <- "Admin unit"
attr(nl_shares$areaha, "label") <- "Area (ha)"
attr(nl_shares$ntlla, "label") <- "Lit area (ha)"
attr(nl_shares$ntlv, "label") <- "Avg annual brightness"
attr(nl_shares$year, "label") <- "Year"
attr(nl_shares$ntlv_shr, "label") <- "Avg annual brightness - % Country"

## SAVE SHOULD BE PROBABLY TO OUT_DIR
write_dta(nl_shares, here(nl_build_dir, "nightlight_shares.dta"))