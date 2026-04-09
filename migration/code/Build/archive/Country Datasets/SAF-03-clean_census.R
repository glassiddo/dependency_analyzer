# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Dec 4, 2021
# Title:   South Africa Census
# Output:  
###############################################################################

source("code/SSA_env_SetUp.R")
source("code/Functions/F-SAF.R")

sa_2016 <- clean_census(2016)
sa_2011 <- clean_census(2011)
sa_2001 <- clean_census(2001)
#sa_1996 <- clean_census(1996)

saveRDS( sa_2016, paste0(saf.dir, "Census/Census_2016.rds"))
saveRDS( sa_2011, paste0(saf.dir, "Census/Census_2011.rds"))
saveRDS( sa_2001, paste0(saf.dir, "Census/Census_2001.rds"))
#saveRDS( sa_1996, paste0(saf.dir, "Census/Census_1996.rds"))

# keeping everyone
sa_2016 <- clean_census(2016, prime_age = FALSE)
sa_2011 <- clean_census(2011, prime_age = FALSE)
sa_2001 <- clean_census(2001, prime_age = FALSE)

saveRDS( sa_2016, paste0(saf.dir, "Census/Census16_all.rds"))
saveRDS( sa_2011, paste0(saf.dir, "Census/Census11_all.rds"))
saveRDS( sa_2001, paste0(saf.dir, "Census/Census01_all.rds"))