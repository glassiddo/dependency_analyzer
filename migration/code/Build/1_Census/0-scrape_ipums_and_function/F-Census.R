#* Project: Migration Africa
#* Author:  Sam Marshall
#* Created: June 9, 2022
#* Title:   Functions for cleaning census files for each country
#*NB: Kenya no mining data
#* Senegal has inconsistent coding
#*******************************************************************************


#*******************************************************************************
## functions ----
geo_labels <- function(dat_df, geo, name, fmt = "title") {
  lab_df <- tibble(d = get_labels(dat_df[[{{geo}}]], values = "p")) %>%
    # from 'dat_df', get_labels get the label of the column 'geo'  
    # values = "p" means that values are added as prefix
    # for example, if label is "paris" and value is "75", then it'll be "[75] paris"
    mutate(v_st = StrPos(d, "\\[", 1),
           v_end = StrPos(d, "\\]", 1),
           {{geo}} := as.numeric(substr(d, v_st + 1, v_end - 1)),
           {{name}} := substring(d, v_end + 2) ) %>%
    mutate(
      across(ends_with("name"), ~str_to_title(.x)),
      across(ends_with("name"), ~gsub("ã©", "e", .x)),
      across(ends_with("name"), ~gsub("ã¨", "e", .x)),
      across(ends_with("name"), ~gsub("ã¯", "i", .x)),
      across(ends_with("name"), ~gsub("Ã¯", "i", .x)),
      across(ends_with("name"), ~gsub("ã³", "o", .x)),
      across(ends_with("name"), ~gsub("ãº", "u", .x)),
      across(ends_with("name"), ~gsub("ã¡", "a", .x)),
      across(ends_with("name"), ~gsub("ã´", "o", .x)),
      across(ends_with("name"), ~gsub("ãç", "c", .x)),
      across(ends_with("name"), ~gsub("ã­", "a", .x)),
      across(ends_with("name"), ~gsub("å", "e", .x)),
      across(ends_with("name"), ~gsub("ãÂ", "e", .x)),
      across(ends_with("name"), ~gsub("Ã", "e", .x, ignore.case = TRUE)),
      across(ends_with("name"), ~str_to_title(.x)),
      across(ends_with("name"), ~ifelse(.x == "Foreign Country", 
                                        "Abroad", .x))
    ) %>%
    # this cleans the name, geo will be the clean name of the value and name of the label
    # back to prev example - geo column (with original name) will be "75" and name "Paris"
    # str_to_title - the first letter of 'name' is capitalised ("Paris" and not "paris")
    select(-d, -v_st, -v_end) 
  # 
  # formatting options of variable name
  if (fmt == "title") {
    lab_df %>% mutate({{name}} := str_to_title({{name}}))
  }
}

# create region and district codes
district_codes <- function(dat_df) {
  r_fac <- dat_df %>% 
    # the multiple for regions to districts, e.g. 10, 100
    # take the maximal number of districts for a single region
    # if its 10 or more, r_fac is 100
    # if its less than 10, r_fac is 10
    count(region_name, district_name) %>% 
    count(region_name) %>% 
    pull(n) %>% 
    max() %>% 
    {if (. >= 10) 10^ceiling(log10(.)) else 10}
  
  region <- dat_df %>% select(region_name) %>% unique() %>%
    arrange(region_name) %>%
    mutate(region = row_number())
  
  district <- dat_df %>% select(district_name, region_name) %>% unique() %>%
    arrange(region_name, district_name) %>%
    group_by(region_name) %>%
    mutate(district = row_number()) %>%
    ungroup() %>%
    full_join(region) %>%
    mutate(district = (r_fac * region) + district)
}

# function to add in the geographic names and keep selected variables
clean_census <- function(dat_df, agvar, ..., dur = FALSE) {
  # merge in name data frames
  dfs <- list(...)
  arg_len <- length(dfs)
  for (i in 1:length(dfs)) {
    dat_df %<>% left_join(dfs[[i]])
  }
  
  dat_df %<>% rename(wgt = perwt) 
  # create agricultural employment indicator
  # if (agvar == "indgen") {
  #   dat_df %<>% mutate(
  #     empl_ag = if_else(indgen == 10, 1, 0, missing = 0),
  #     empl_mining = if_else(indgen == 20, 1, 0, missing = 0),
  #     employed = ifelse(empstat == 1, 1, 0),
  #     empl_nam = if_else(employed == 1 & indgen %!in% c(10,20),1, 0, 
  #                        missing = 0))
  # }
  if (agvar == "indgen") {
    dat_df %<>% mutate(
      empl_ag = ifelse(indgen == 10, 1, 0),
      empl_mining = ifelse(indgen == 20, 1, 0),
      employed = ifelse(empstat == 1, 1, 0),
      empl_nam = ifelse(employed == 1 & indgen %!in% c(10,20),1, 0),
      empl_nam = ifelse(is.na(indgen) == TRUE, NA, empl_nam),
      # mining and manufacturing
      empl_mfg = case_when(
        indgen %in% c(20:30) ~ 1,
        indgen %in% c(10, 40:130) ~ 0,
        TRUE | is.na(indgen) == TRUE ~ 0
      ),
      # electricity/waste management, construction, retail, hotels, 
      # transportation, finance, govt, services, ind n.e.c.
      empl_svc = case_when(
        indgen %in% c(10:30) ~ 0,
        indgen %in% c(40:130) ~ 1,
        TRUE | is.na(indgen) == TRUE ~ 0
      ),
      # mining and manufacturing (could get detail to add wholesale trade)
      empl_trd = case_when(
        indgen %in% c(20:30) ~ 1,
        indgen %in% c(10,40:130) ~ 0,
        TRUE | is.na(indgen) == TRUE ~ 0
      ),
      empl_ntrd = case_when(
        indgen %in% c(10:30) ~ 0,
        indgen %in% c(40:130) ~ 1,
        #TRUE ~ NA
        TRUE | is.na(indgen) == TRUE ~ 0
      ))
  }
  else if (agvar == "gn1996a_occ") {
    dat_df %<>% mutate(
      empl_ag = if_else(gn1996a_occ %in% c(611:621,921) | 
                          gn2014a_occ %in% c(445,453), 1, 0, missing = 0),
      empl_mining = if_else(gn1996a_occ %in% c(711,931) | 
                              gn2014a_occ %in% c(463:469), 1, 0, missing = 0),
      employed = ifelse(empstat == 1, 1, 0),
      empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                         missing = 0),
      # mining and manufacturing
      empl_mfg = case_when(
        gn1996a_occ %in% c(711:823,921,931,932)  ~ 1,
        gn2014a_occ %in% c(463:469,494:514) ~ 1,
        TRUE ~ 0
      ),
      # electricity/waste management, construction, retail, hotels, 
      # transportation, finance, govt, services, ind n.e.c.
      empl_svc = case_when(
        gn1996a_occ %in% c(112:523,831:916,933,939)  ~ 1,
        gn2014a_occ %in% c(4:444,470:493,515:549) ~ 1,
        TRUE ~ 0
      ),
      # mining and manufacturing
      empl_trd = case_when(
        gn1996a_occ %in% c(711:823,921,931,932)  ~ 1,
        gn2014a_occ %in% c(429,463:469,494:514) ~ 1,
        TRUE ~ 0
      ),
      empl_ntrd = case_when(
        gn1996a_occ %in% c(112:523,831:916,933,939)  ~ 1,
        gn2014a_occ %in% c(4:428,430:444,470:493,515:549) ~ 1,
        TRUE ~ 0
      ))
  }
  else if (agvar == "ke1999a_econactx") {
    dat_df %<>% mutate(
      empl_ag = if_else(ke1999a_econactx == 4 | ke2009a_empstat == 5, 1, 0, 
                        missing = 0),
      employed = ifelse(empstat == 1, 1, 0),
      empl_mining = 0,
      empl_nam = if_else(employed == 1 & empl_ag == 0, 1, 0, 
                         missing = 0),
      #no data on ind/occ so put zero
      empl_mfg = 0,
      empl_svc = 0,
      empl_trd = 0,
      empl_ntrd = 0)
  }
  else if (agvar == "ls1996a_occ") {
    dat_df %<>% mutate(
      empl_ag = if_else(ls1996a_occ %in% c(61:66) | ls2006a_occ %in% c(61,62,92), 
                        1, 0, missing = 0),
      empl_mining = if_else(ls1996a_occ == 71 | ls1996a_occ == 93, 
                            1, 0, missing = 0),
      employed = ifelse(empstat == 1, 1, 0),
      empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                         missing = 0),
      # mining and manufacturing
      empl_mfg = case_when(
        ls1996a_occ %in% c(70:83)  ~ 1,
        ls2006a_occ %in% c(71:82) ~ 1,
        TRUE ~ 0
      ),
      # electricity/waste management, construction, retail, hotels, 
      # transportation, finance, govt, services, ind n.e.c.
      empl_svc = case_when(
        ls1996a_occ %in% c(2:59,84:99)  ~ 1,
        ls2006a_occ %in% c(1:52,83,91,93) ~ 1,
        TRUE ~ 0
      ),
      # mining and manufacturing
      empl_trd = case_when(
        ls1996a_occ %in% c(70:83)  ~ 1,
        ls2006a_occ %in% c(71:82) ~ 1,
        TRUE ~ 0
      ),
      empl_ntrd = case_when(
        ls1996a_occ %in% c(2:59,84:99)  ~ 1,
        ls2006a_occ %in% c(1:52,83,91,93) ~ 1,
        TRUE ~ 0
      ))
  }
  else if (agvar == "sn2002a_occ3") {
    dat_df %<>% mutate(
      empl_ag = if_else(sn2002a_occ3 %in% c(611:621) | 
                          sn2013a_occ3 %in% c(611:634,921), 
                        1, 0, missing = 0),
      empl_mining = if_else(sn2002a_occ3 == 711 | sn2013a_occ3 == 933, 
                            1, 0, missing = 0),
      employed = ifelse(empstat == 1, 1, 0),
      empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                         missing = 0),
      # mining and manufacturing
      empl_mfg = case_when(
        sn2002a_occ3 %in% c(711,731:821)  ~ 1,
        sn2013a_occ3 %in% c(721:810,932,933) ~ 1,
        TRUE ~ 0
      ),
      # electricity/waste management, construction, retail, hotels, 
      # transportation, finance, govt, services, ind n.e.c.
      empl_svc = case_when(
        sn2002a_occ3 %in% c(110:523,712:714:724,831:916)  ~ 1,
        sn2013a_occ3 %in% c(11:541,711:713,832:912,941:962) ~ 1,
        TRUE ~ 0
      ),
      # mining and manufacturing
      empl_trd = case_when(
        sn2002a_occ3 %in% c(711,731:821)  ~ 1,
        sn2013a_occ3 %in% c(721:810,932,933) ~ 1,
        TRUE ~ 0
      ),
      empl_ntrd = case_when(
        sn2002a_occ3 %in% c(110:523,712:714:724,831:916)  ~ 1,
        sn2013a_occ3 %in% c(11:541,711:713,832:912,941:962) ~ 1,
        TRUE ~ 0
      ))
  }
  else if (agvar == "sl2004a_ind") {
    dat_df %<>% mutate(
      empl_ag = if_else(sl2004a_ind %in% c(1:3) | sl2015a_ind %in% c(1,2), 
                        1, 0, missing = 0),
      empl_mining = if_else(sl2004a_ind == 7 | sl2015a_ind == 5, 
                            1, 0, missing = 0),
      employed = ifelse(empstat == 1, 1, 0),
      empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                         missing = 0),
      # mining and manufacturing, hunting, fishing, forestry (5)
      empl_mfg = case_when(
        sl2004a_ind %in% c(4:8)  ~ 1,
        sl2015a_ind %in% c(3:6) ~ 1,
        TRUE ~ 0
      ),
      # electricity/waste management, construction, retail, hotels, 
      # transportation, finance, govt, services, ind n.e.c.
      empl_svc = case_when(
        sl2004a_ind %in% c(9:21)  ~ 1,
        sl2015a_ind %in% c(7:24) ~ 1,
        TRUE ~ 0
      ),
      # mining and manufacturing
      empl_trd = case_when(
        sl2004a_ind %in% c(4:8)  ~ 1,
        sl2015a_ind %in% c(3:6) ~ 1,
        TRUE ~ 0
      ),
      empl_ntrd = case_when(
        sl2004a_ind %in% c(9:21)  ~ 1,
        sl2015a_ind %in% c(7:24) ~ 1,
        TRUE ~ 0
      ))
  }
  else if (agvar == "ug2002a_occ") {
    dat_df %<>% mutate(
      empl_ag = if_else(ug2002a_occ %in% c(611:623, 921) | 
                          ug2014a_occ %in% c(10:12,15,31,44), 1, 0, missing = 0),
      empl_mining = if_else(ug2002a_occ %in% c(711, 931) | 
                              ug2014a_occ == 60, 1, 0, missing = 0),
      employed = ifelse(empstat == 1, 1, 0),
      empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                         missing = 0),
      # mining and manufacturing
      empl_mfg = case_when(
        ug2002a_occ %in% c(711,732:751,811:829,931,933)  ~ 1,
        ug2014a_occ %in% c(24,28,35:37,54,60) ~ 1,
        TRUE ~ 0
      ),
      # electricity/waste management, construction, retail, hotels, 
      # transportation, finance, govt, services, ind n.e.c.
      empl_svc = case_when(
        ug2002a_occ %in% c(111:550,712:731,831:918,932,934:941)  ~ 1,
        ug2014a_occ %in% c(13,14,16:23,25:27,29,32:34,38:43,45:53,55:59,61:70) ~ 1,
        TRUE ~ 0
      ),
      # mining and manufacturing and 39,42
      empl_trd = case_when(
        ug2002a_occ %in% c(711,732:751,811:829,931,933,532:540)  ~ 1,
        ug2014a_occ %in% c(24,28,35:37,54,60,39,42) ~ 1,
        TRUE ~ 0
      ),
      empl_ntrd = case_when(
        ug2002a_occ %in% c(111:531,541:550,712:731,831:918,932,934:941)  ~ 1,
        ug2014a_occ %in% c(13,14,16:23,25:27,29,32:34,38,40,41,43,45:53,55:59,
                           61:70) ~ 1,
        TRUE ~ 0
      ))
  }
  else if (agvar == "occ") {
    dat_df %<>% mutate(
      empl_ag = if_else(occ %in% c(601:699,921), 1, 0, missing = 0),
      empl_mining = if_else(occ %in% c(711, 931), 1, 0, missing = 0),
      employed = ifelse(empstat == 1, 1, 0),
      empl_nam = if_else(employed == 1 & empl_ag == 0 & empl_mining == 0, 1, 0, 
                         missing = 0),
      # mining and manufacturing
      empl_mfg = case_when(
        occ %in% c(711,715,731:829,931,932)  ~ 1,
        TRUE ~ 0
      ),
      # electricity/waste management, construction, retail, hotels, 
      # transportation, finance, govt, services, ind n.e.c.
      empl_svc = case_when(
        occ %in% c(11:524,712:714,721:725,831:916,933,977)  ~ 1,
        TRUE ~ 0
      ),
      # mining and manufacturing
      empl_trd = case_when(
        occ %in% c(711,715,731:829,931,932)  ~ 1,
        TRUE ~ 0
      ),
      empl_ntrd = case_when(
        occ %in% c(11:524,712:714,721:725,831:916,933,977)  ~ 1,
        TRUE ~ 0
      ))
  }
  
  dat_df %<>% select(year, age, wgt, urban, contains(c("duration", "name")),
                     starts_with(c("empl", "geo", "area", "mig", "bpl")))
  if (dur != FALSE) {
    dat_df %<>% mutate(migration_year = year - duration)
  }
  
  return(dat_df)
}

# make urban-rural indicator for each unit
urban_status <- function(dat_df, yr, geo = district_name) {
  dat_df %<>% mutate(urban = case_when(
    urban == 1 ~ 0,
    urban == 2 ~ 1))
  
  urban_df <- dat_df %>% 
    filter(year == yr) %>%
    group_by({{geo}}) %>%
    summarise(urban_geo = weighted.mean(urban, w = wgt, na.rm = TRUE), 
              .groups = 'drop') %>%
    mutate(urban_geo = round(urban_geo))
  
  out_df <- full_join(dat_df, urban_df)
}

# function to combine birth locations when not already done
combine_bl <- function(dat_df, orig = FALSE) {
  
  # get a list of consistent district names
  district <- dat_df %>% select(district_name) %>% unique()
  
  di <- str_split(district$district_name, ", ")
  
  birth <- dat_df %>% select(birth_district_name) %>% unique() %>%
    mutate(bdn = birth_district_name)
  
  for (i in 1:length(di)) {
    birth %<>% mutate(bdn = ifelse(
      birth_district_name %in% di[[i]], district$district_name[i], bdn) )
  }
  
  dat_df %<>% left_join(birth) %>%
    mutate(birth_district_name_o = birth_district_name,
           birth_district_name = bdn) %>%
    select(-bdn)
  
  # keep original values?
  if (orig == FALSE) { dat_df %<>% select(-birth_district_name_o)}
  return(dat_df)
  
  #* to do for XX district unknown department it is
  #* 1. unknown = regexpr("Region, Unknown Department", birth_district_name))
  #* 2. if unknown > 0, substr(x, unknown -2)
  #* 3. get prev district names
  #* 4. get subset of prev dist names with "Regions, Unknown Department"
  #* 5. search over this set for the matches and fill
  
}

# combine birth regions, should incorporate this into previous func, sep for now
combine_br <- function(dat_df, orig = FALSE) {
  
  # get a list of consistent region names
  geo_names <- dat_df %>% select(region_name) %>% distinct()
  
  di <- str_split(geo_names$region_name, ", ")
  
  birth <- dat_df %>% select(birth_region_name) %>% distinct() %>%
    mutate(bdn = birth_region_name)
  
  for (i in 1:length(di)) {
    birth %<>% mutate(bdn = ifelse(
      birth_region_name %in% di[[i]], geo_names$region_name[i], bdn) )
  }
  
  dat_df %<>% left_join(birth) %>%
    mutate(birth_region_name_o = birth_region_name,
           birth_region_name = bdn) %>%
    select(-bdn)
  
  # keep original values?
  if (orig == FALSE) { dat_df %<>% select(-birth_region_name_o)}
  return(dat_df)
  
}

# function to combine prev district when not already done
combine_pd <- function(dat_df, orig = FALSE) {
  
  # get a list of consistent district names
  district <- dat_df %>% select(district_name) %>% distinct()
  
  di <- str_split(district$district_name, ", ")
  
  prev <- dat_df %>% select(prev_district_name) %>% distinct() %>%
    mutate(pdn = prev_district_name)
  
  for (i in 1:length(di)) {
    prev %<>% mutate(pdn = ifelse(
      prev_district_name %in% di[[i]], district$district_name[i], pdn) )
  }
  
  dat_df %<>% left_join(prev) %>%
    mutate(prev_district_name_o = prev_district_name,
           prev_district_name = pdn) %>%
    select(-pdn)
  
  # keep original values?
  if (orig == FALSE) { dat_df %<>% select(-prev_district_name_o)}
  return(dat_df)
  
}

# function to put codes in for previous district and birth district for abroad/unknown
code_prev_dist <- function(dat_df, abr_r, abr_d, unk_r = 0) {
  #unknown/ NIU codes
  niu_r <- abr_r - 1
  niu_d <- abr_d - 2
  unk_d <- niu_d - 1
  
  dat_df %<>%
    mutate(
      prev_region = case_when(
        prev_district_name %in% c("Niu (Not In Universe)", "Unknown") ~niu_r,
        prev_district_name == "Abroad" ~abr_r,
        TRUE~as.numeric(prev_region) ),
      prev_district = case_when(
        prev_district_name == "Niu (Not In Universe)" ~niu_d,
        prev_district_name == "Unknown"~ unk_d,
        prev_district_name == "Abroad"~ abr_d,
        TRUE~as.numeric(prev_district) ),
      prev_region_name = ifelse(
        prev_district_name %in% c("Abroad", "Unknown", "Niu (Not In Universe)"),
        prev_district_name, prev_region_name)) 
  
  if (unk_r != 0) {
    dat_df %<>% 
      group_by(prev_region_name) %>%
      mutate(prev_r_mean = mean(prev_region, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        prev_region = ifelse(is.na(prev_region), prev_r_mean, prev_region),
        prev_district = ifelse(is.na(prev_district), 
                               unk_r * prev_r_mean, 
                               prev_district)
      ) %>%
      select(-prev_r_mean)
  }
  
  return(dat_df)
}
