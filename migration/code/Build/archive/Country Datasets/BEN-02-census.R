# Project: Migration Africa
# Author:  Sam Marshall
# Created: Jan 20, 2022
# Title:   Benin Clean Census
# Output:  Census/Census_2013.rds Census/Census_2002.rds
###############################################################################

source("code/SSA_env_SetUp.R")
source("code/Functions/F-BEN.R")

###############################################################################
##### 1. Clean Censuses 
###############################################################################
# 
# benin13_df <- clean_census(2013) %>%
#   select(-pop_92) %>%
#   rename(pop_start = pop_02, curr_pop = pop_13)
# 
# benin02_df <- clean_census(2002) %>%
#   select(-pop_13) %>%
#   rename(pop_start = pop_92, curr_pop = pop_02)
# 
# saveRDS( benin13_df, paste0(ben.dir, "Census/Census_2013.rds"))
# saveRDS( benin02_df, paste0(ben.dir, "Census/Census_2002.rds"))

# keeping everyone
benin13_df <- clean_census(2013, prime_age = FALSE) 
benin02_df <- clean_census(2002, prime_age = FALSE) 

saveRDS( benin13_df, paste0(build.dir, "Benin/Census/census13.rds"))
saveRDS( benin02_df, paste0(build.dir, "Benin/Census/census02.rds"))

