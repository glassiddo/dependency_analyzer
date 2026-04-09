source("code/SSA_env_SetUp.R")

# the 2014 census is level 2
# we also have a 1999 household survey, but not nationally representative
# has data about migration to 7 out of 18 provinces 
# (so the migration matrix would be 7*18)
# and some variables are a bit limited
# also the industry/employment variables are a bit less consistent

hh_survey99 <- read_dta(here(raw.dir, "Countries", "AGO", 
                   "Census", "archives", "indhh_99.dta")) %>% 
  select(
    # the following are presumably relevant variables
    hh_id = NIDF,
    id = CID01, 
    sex = sexo,
    wgt,
    age = idcal, 
    province = provinci,
    urban = xarea,
    birth_province = s1b12,
    when_moved_here = s1b14,
    # when moved here doesnt refer to x years ago, but to 'arbitrary' periods
    # e.g. between 75-79, 79-92, or 92-99 (last 7 years)
    occupation_main = s403,
    activity_type_main = s404,
    industry = grp_cae,
    employment_status = s401
  )

og <- read_sav(here(
  raw.dir, "Countries", "AGO", "Census", "archives", 
  "1999 Household survey", "Ficheiro dos individuos.sav"
  )) # essentially the same but sav... so unnecessary to use this version  
