# scrape IPUMS API data
source("code/SSA_env_SetUp.R")
library(ipumsr)
# set_ipums_api_key('59cba10d8a5da536fc06b59d474ad74778ca482eb7e3f052669896a5',
#                   save = TRUE) # iddo's key

# mypums <- get_extract_history('ipumsi')

common_vars <- c('COUNTRY','YEAR','SERIAL','HHWT','URBAN','PERNUM','PERWT',
                 'AGE','SEX','EMPSTAT','EMPSTATD','OCCISCO','OCC'
                 #,'INDGEN',
                 #'IND'
                 )

# scrape example
zwe_extract <- ipumsr::define_extract_ipumsi(
  description = 'Zimbabwe 2012',
  samples = c('zw2012a'),
  variables = c(common_vars, 'GEO1_ZW', 'GEO2_ZW','BPLCOUNTRY',#'BPLZW',
                'ZW2012A_BPL2', 'ZW2012A_BPL', 
                'ZW2012A_RES10YR', 'ZW2012A_RES10YR3',
                'isco88a', 'geo1_zw2012', 'geo2_zw2012', 'geo3_zw2012',
                'MIG1_10_ZW', 'MIG2_10_ZW'
                #'MIGCTRY1','MIGCTRYP','GEOMIG1_1','MIGYRS1'
                #'MIG1_P_ZW', 'MIG1_1_ZW'
                )
)

ctry_submit <- submit_extract(zwe_extract)

is_extract_ready(ctry_submit)

ctry_df <- download_extract(ctry_submit,
                            download_dir = paste0(raw.dir,'Countries/ZWE/Census'))

# mwi_df <- read_ipums_micro(
#   ddi = '/Users/smarsh/Library/CloudStorage/Dropbox/Migration Africa/ipumsi_00128.xml',
#   data_file = '/Users/smarsh/Library/CloudStorage/Dropbox/Migration Africa/ipumsi_00128.dat.gz'
# )

zwe_df <- read_ipums_micro(
  data_file = here(raw.dir, 'Countries/ZWE/Census/ipumsi_00013.dat.gz'),
  ddi = here(raw.dir, 'Countries/ZWE/Census/ipumsi_00013.xml')
) %>% 
  rename_with(tolower)
# 
write_dta(zwe_df, here(raw.dir, 'Countries/ZWE/Census/consistent_12.dta'))
