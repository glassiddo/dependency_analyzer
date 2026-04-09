source("code/SSA_env_SetUp.R")
source("code/Build/1_Census/0-scrape_ipums_and_function/F-Census.R")

ctry_df <- read_dta(here(raw.dir, "Countries", "AGO", "Census", "perlim14.dta"))

###### clean the df --------

# get geo labels for level 2 before transforming variables (see the next section)
geo2_labels <- attr(ctry_df$geo2_ao, "labels")
mig2_labels <- attr(ctry_df$mig2_5_ao, "labels")

ctry_df <- ctry_df %>% 
  mutate(
    perwt = 10.535037,
    # original is 1.056 which makes little sense (but perwt is identical for all)
    # 25 789 024 # true population based on the following
    # https://www.ine.gov.ao/Arquivos/arquivosCarregados/Carregados/Publicacao_637981512172633350.pdf
    # turn to character
    geo2_ao = case_when(
      geo1_ao == 141012 & geo2_ao == 9999 ~ 141012017, # Luau
      geo2_ao != 9999 ~ 
        as.numeric(
          paste0(as.character(geo1_ao), "0", str_sub(as.character(geo2_ao), -2))
          ),
      TRUE ~ NA_real_
    ),
    ## recreating geo2_ao - in the original file, it's is 3/4 letter code 
    ## with the last 2 indicating the municipality code (odd numbers)
    ## and the first 1/2 indicating the code of the province (1-18)
    ## the ipums geo2_lev is structured as:
    ## first three digits are country code (here - 141)
    ## next three digits indicating geo1_lev (with 0/00 before if necessary)
    ## final three digits indicating geo2_lev (with 0/00 before if necessary)
    len_mig2_code1 = str_length(str_sub(as.character(mig2_5_ao), -4, -3)),
    mig2_5_ao = case_when(
      !mig2_5_ao %in% c(9998, 9999) & len_mig2_code1 == 1 ~
        as.numeric(paste0( 
          "141", "00", str_sub(as.character(mig2_5_ao), -4, -3), "0", 
          str_sub(as.character(mig2_5_ao), -2, -1)
        )),
      !mig2_5_ao %in% c(9998, 9999) & len_mig2_code1 == 2 ~
        as.numeric(paste0(
          "141", "0", str_sub(as.character(mig2_5_ao), -4, -3), "0", 
          str_sub(as.character(mig2_5_ao), -2, -1)
        )),
      TRUE ~ NA_real_
    )
  ) %>% 
  select(
    country, year, age, urban, geo1_ao, geo2_ao, mig1_5_ao, mig2_5_ao, 
    migctry5, empstat, indgen, perwt
  )

# waldo::compare(sort(unique(ctry_df$mig2_5_ao)), sort(unique(ctry_df$geo2_ao)))
## should have no diffs
# sum(ctry_df$perwt)
## should be 25m (population in 2014, by design gonna be that given perwt)

#### clean labels -----

# the labels in Moxico province (012) are wrong
# correct population numbers can be found here
# https://www.ine.gov.ao/Arquivos/arquivosCarregados//Carregados/Publicacao_637586912530087603.pdf
# based on that, I adjusted the labels to fit the municipalities
# geo2_ao == 141012013 ~ "Luchazes", # lowest population, around 14k
# geo2_ao == 141012011 ~ "Budas-Lumbala-Nguimbo", # around 69k
# geo2_ao == 141012015 ~ "Alto Zambeze", # around 111k
# geo2_ao == 141012007 ~ "Luacano", # around 20k
# geo2_ao == 141012009 ~ "Cameia", # around 29k
# geo2_ao == 141012005 ~ "Léua", # around 32k 
# geo2_ao == 141012017 ~ "Luau", # unknown label but 89k total, fits Luau (89k)
# # Luena and Camanongue are correct 


geo2_new_labels <- sapply(geo2_labels, function(x) {
  if (x == 9999) return(141012017)  # luau
  prov_code <- as.numeric(str_sub(as.character(x), 1, -3))
  mun_code <- str_sub(as.character(x), -2)
  as.numeric(paste0(
    "141", ifelse(nchar(as.character(prov_code)) == 1, "00", "0"), 
    prov_code, "0", mun_code))
})

names(geo2_new_labels)[geo2_new_labels == 141012005] <- "Léua"
names(geo2_new_labels)[geo2_new_labels == 141012007] <- "Luacano"
names(geo2_new_labels)[geo2_new_labels == 141012009] <- "Cameia"
names(geo2_new_labels)[geo2_new_labels == 141012011] <- "Budas-Lumbala-Nguimbo"
names(geo2_new_labels)[geo2_new_labels == 141012013] <- "Luchazes"
names(geo2_new_labels)[geo2_new_labels == 141012015] <- "Alto Zambeze"
names(geo2_new_labels)[geo2_new_labels == 141012017] <- "Luau" 

ctry_df$geo2_ao <- labelled(ctry_df$geo2_ao, labels = geo2_new_labels)
attr(ctry_df$geo2_ao, "label") <- "angola, municipality 2014 [level 2; consistent boundaries, gis]"

# Transform mig2_5_ao labels
mig2_new_labels <- sapply(mig2_labels, function(x) {
  if (x == 9999) return(141012017)  # TODO verify it!!! also check 9998
  
  len_code1 <- str_length(str_sub(as.character(x), -4, -3))
  
  if (len_code1 == 1) {
    as.numeric(paste0( 
      "141", "00", str_sub(as.character(x), -4, -3), "0", 
      str_sub(as.character(x), -2, -1)
    ))
  } else if (len_code1 == 2) {
    as.numeric(paste0(
      "141", "0", str_sub(as.character(x), -4, -3), "0", 
      str_sub(as.character(x), -2, -1)
    ))
  } else {
    NA_real_
  }
})

mig2_new_labels <- mig2_new_labels[!is.na(mig2_new_labels)]

names(mig2_new_labels)[mig2_new_labels == 141012005] <- "Léua"
names(mig2_new_labels)[mig2_new_labels == 141012007] <- "Luacano"
names(mig2_new_labels)[mig2_new_labels == 141012009] <- "Cameia"
names(mig2_new_labels)[mig2_new_labels == 141012011] <- "Budas-Lumbala-Nguimbo"
names(mig2_new_labels)[mig2_new_labels == 141012013] <- "Luchazes"
names(mig2_new_labels)[mig2_new_labels == 141012015] <- "Alto Zambeze"
names(mig2_new_labels)[mig2_new_labels == 141012017] <- "Luau" 

ctry_df$mig2_5_ao <- labelled(ctry_df$mig2_5_ao, labels = mig2_new_labels)
attr(ctry_df$mig2_5_ao, "label") <- "municipality of residence 5 years ago, angola; consistent boundaries, gis"

write_dta(ctry_df, here(raw.dir, "Countries", "AGO", "Census", "consistent14.dta"))