# scrape IPUMS API data
source("code/SSA_env_SetUp.R")
library(ipumsr)

# common_vars <- c('COUNTRY','YEAR','SERIAL','HHWT','URBAN','PERNUM','PERWT',
#                  'AGE','SEX','EMPSTAT','EMPSTATD','OCCISCO','OCC','INDGEN',
#                  'IND')
# 
# # scrape example
# eth_extract <- define_extract_ipumsi(
#   description = 'Ethiopia again w/ geo vars',
#   samples = c('et1994a', 'et2007a'),
#   variables = c(common_vars, 'GEO1_ET','GEOMIG1_P','MIGYRS1')
# )
# 
# ctry_submit <- submit_extract(eth_extract)

ready <- is_extract_ready(ctry_submit)

ctry_df <- download_extract(
  i8, download_dir = here(raw.dir, "Countries/ETH")
  )

eth_df <- read_ipums_micro(
  ddi = '/Users/iddo2/Dropbox/Migration Africa/data/raw/countries/eth/ipumsi_00008.xml',
  data_file = '/Users/iddo2/Dropbox/Migration Africa/data/raw/countries/eth/ipumsi_00008.dat.gz'
)

#saveRDS(eth_df, here(raw.dir, "Countries", "ETH", "ipums_eth9407.rds"))

eth <- eth_df %>% #readRDS(here(raw.dir, "Countries", "ETH", "ipums_eth9407.rds")) %>% 
  filter(YEAR == 2007) %>% 
  select(-OCC, -OCCISCO, -INDGEN, -IND) %>% # none is available for 2007
  filter(PERWT > 0) %>% # weird but seems necessary
  mutate(
    prev_region = ifelse(MIGYRS1 <= 5, GEOMIG1_P, NA) 
    # two notes:
    ## 1) GEOMIG1_P is only available for level 1, but migyrs1 refers to level 3...
    ##  "What is the number of years [the respondent] has continuously lived in this town or the rural part of this Wereda?"
    ## 2) SPECIAL REGION (231017) is weird:
    ##  "special region comprised national parks, forests, and boarding schools 
    ##  including university dorms with greater than 100 persons. 
    ##  There are person counts for the special region but the region is not mappable."
  )


