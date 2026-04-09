#* Project: Migration Africa
#* Author:  Flavia Grasso
#* Date:    January 23, 2024
#* Title:   Migration + International Migration
#* Desc:    Build migration Dataset for each admin unit, both national and international 
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

countries <- c( 
  "BEN", "BWA", "BFA", "CMR", "GIN", 
  "KEN", "LSO", "MWI", "MLI", "MUS", 
  "MOZ", "RWA", "SEN", "SLE", "ZAF", 
  "SDN", "TZA", "TGO", "UGA", "ZMB", 
  "ZWE", "AGO", "MDG"
)
# Ghana has no info about migration, for any given period

ctries_to_add_aggs <- c("GAB", "NGA", "ETH")

# yr_start/end_dic only feature countries with the MIGYRS1 variable + ZAF (?)
yr_start_dic <- c(
  "BEN"=2003, "CMR"= 1995, "GIN"= 2004, "MWI"=1998, "MLI"=1999, "RWA"=2002, 
  "ZAF"=2002, "SDN"=1998, "TGO"=2000, "UGA"=2004
)
yr_end_dic <- c(
  "BEN"=2012, "CMR"=2004, "GIN"=2013, "MWI"=2007, "MLI"=2008, "RWA"=2011,
  "ZAF"=2010, "SDN"=2007, "TGO"=2009, "UGA"=2013
)

# for the others - number of years for migration variable
period_dic <- c(
  "BWA"= 5, "BFA"=1, "KEN"= 1, "LSO"=10, "MUS"=5, "MOZ"=5, "SEN"=5, "SLE"=5,
  "TZA"=1, "ZMB"=1, "ZWE"=10, "AGO"=5, "MDG"=10
)

id_df <- readRDS(here(out.dir, "R", "id.rds"))

# set of countries combinations and creating common df of all countries df ----
ids_rel <- id_df %>%
  filter(country %in% countries | country %in% ctries_to_add_aggs)

d_df <- ids_rel %>%
  group_by(country_name) %>%
  select(
    country_name, 
    district, 
    region,
    ipums_id_d = ipums_id,
  ) %>%
  distinct() %>% 
  mutate(
    region = ifelse(is.na(district), region, NA)
  )

o_df <- ids_rel %>%
  group_by(country_name) %>%
  select(
    prev_country_name = country_name, 
    prev_district = district,
    prev_region = region,
    ipums_id_o = ipums_id,
  ) %>%
  distinct() %>% 
  mutate(
    prev_region = ifelse(is.na(prev_district), prev_region, NA)
  )

ctryset_df <- cross_join(d_df, o_df)

#imputing m_od, m_oo, etc for internal migrants ----
dat_df <- data.frame()

for (ctry_str in countries){
  print(ctry_str)
  
  census_data <- census_info %>% 
    filter(country == ctry_str) %>%
    select(yr2_suffix, lvl, prime_age)
  
  census_yr <- census_data$yr2_suffix
  lvl <- census_data$lvl
  prime_age <- census_data$prime_age
  
  ctry_df <- readRDS(here(
    build.dir, "Countries", ctry_str,"Census", 
    paste0("census",census_yr,".rds")
  ))
  
  if (prime_age == TRUE) {ctry_df %<>% filter(age >= min_pa & age <= max_pa)}
  
  # migration flows for given years   
  if(ctry_str %in%  names(yr_start_dic)){
    geo <- g_sym(lvl)
    prev_geo <- g_sym(lvl, r = "p")
    geo_name <- as.name(paste0(geo, '_name'))
    prev_geo_name <- as.name(paste0(prev_geo, '_name'))
    
    d_df <- ctry_df %>% 
      rename_with(~"ipums_id_d", 
                  starts_with(paste0("geo", lvl)) & 
                    !ends_with(c("0","1","2","3","4","5","6","7","8","9", 
                                 "geomig1_p")) ) %>%
      mutate(ipums_id_d = as.character(ipums_id_d)) %>%
      select({{geo}}, ipums_id_d) %>%
      distinct() 
    
    o_df <- d_df %>% 
      rename({{prev_geo}} := {{geo}}, ipums_id_o = ipums_id_d)
    
    distset_df <- merge(d_df, o_df)
    
    ctry_df %<>% 
      mutate(mig = ifelse(migration_year >= yr_start_dic[ctry_str] & migration_year <= yr_end_dic[ctry_str], wgt, 0),
             mig = ifelse(is.na(migration_year) == TRUE, 0, mig),
             mig = ifelse({{geo}} == {{prev_geo}}, 0, mig),
             nonmig = ifelse(mig>0, 0, wgt))%>%
      group_by({{geo}},{{prev_geo}}) %>%
      summarize(m_od = sum(mig, na.rm = TRUE),
                m_oo = sum(nonmig, na.rm = TRUE),
                .groups='drop') %>% 
      full_join(distset_df) %>%
      mutate(m_od = ifelse(is.na(m_od), 0, m_od),
             m_oo = ifelse(is.na(m_oo), 0, m_oo)) %>%
      group_by({{geo}}) %>%
      mutate(m_oo = sum(m_oo)) %>%
      ungroup()%>%
      drop_na(ipums_id_d) %>% #only want to count in m_o, m_d those who migrated to/from within their country of origin
      group_by({{geo}}) %>%
      mutate(m_d = sum(m_od)) %>%
      ungroup() %>%
      group_by({{prev_geo}})%>%
      mutate(m_o = sum(m_od)) %>%
      ungroup() %>%
      mutate(d_shares  = (m_od / m_d),
             o_shares  = (m_od / m_o)) %>%
      mutate(
        country_name = sapply(ctry_str, n_ctry),
        prev_country_name = sapply(ctry_str, n_ctry),
        mig_start_yr = yr_start_dic[ctry_str], 
        mig_end_yr = yr_end_dic[ctry_str],
        mig_prd_len = yr_end_dic[ctry_str] - yr_start_dic[ctry_str] + 1,
        mig_measure = paste0("window: ", mig_prd_len, " year"), .before={{geo}}
      )
  }
  
  #migration flows for a given period
  else{
    yr_end <- mean(ctry_df$year, na.rm=TRUE)
    
    if (is.na(period_dic[ctry_str]) == TRUE) {
      lag <- "birth"
      mig_meas <- "since birth"
    }
    else {
      lag <- "prev"
      mig_meas <- paste0("fixed: ", period_dic[ctry_str], " year")
    }
    
    geo <- g_sym(lvl)
    prev_geo <- g_sym(lvl, r = "p", l_stub = lag)
    
    geo_name <- as.name(paste0(geo, '_name'))
    prev_geo_name <- as.name(paste0(prev_geo, '_name'))
    
    d_df <- ctry_df %>% 
      rename_with(~"ipums_id_d", starts_with(paste0("geo", lvl)) & 
                    !ends_with(c("0","1","2","3","4","5","6","7","8","9", 
                                 "geomig1_p")) ) %>%
      mutate(ipums_id_d = as.character(ipums_id_d)) %>%
      select({{geo}}, ipums_id_d) %>%
      distinct() 
    
    o_df <- d_df %>% 
      rename({{prev_geo}} := {{geo}}, ipums_id_o = ipums_id_d)
    
    distset_df <- merge(d_df, o_df) 
    
    ctry_df %<>% 
      drop_na({{prev_geo}}) %>%
      group_by({{geo}}, {{prev_geo}}) %>%
      summarise(m_od = sum(wgt, na.rm = TRUE), .groups = 'drop') %>%
      full_join(distset_df) %>%
      mutate(m_od = ifelse(is.na(m_od) == TRUE, 0, m_od),
             m_oo = ifelse(ipums_id_o == ipums_id_d, m_od, 0),
             m_od = ifelse({{geo}} == {{prev_geo}}, 0, m_od)) %>%
      group_by({{geo}}) %>%
      # destination total immigration
      mutate(m_oo = sum(m_oo, na.rm = TRUE)) %>%
      ungroup() %>%
      # drop flows in from unsurveyed districts
      drop_na(ipums_id_d) %>%
      group_by({{geo}}) %>%
      mutate(m_d = sum(m_od)) %>%
      ungroup() %>%
      group_by({{prev_geo}}) %>%
      # origin total emigration
      mutate(m_o = sum(m_od)) %>%
      ungroup() %>%
      mutate(d_shares  = (m_od / m_d),
             o_shares  = (m_od / m_o),
             mig_measure = mig_meas,
             country_name = sapply(ctry_str, n_ctry),
             prev_country_name = sapply(ctry_str, n_ctry),
             mig_start_yr = yr_end - period_dic[ctry_str] + 1, 
             mig_end_yr = yr_end,
             mig_prd_len = period_dic[ctry_str])
    if (lag == "birth") {
      ctry_df %<>% rename_with(~gsub(lag, "prev", .x), starts_with(lag)) 
    }
  }
  dat_df <- bind_rows(dat_df, ctry_df) 
}

### add countries with aggregate values
for (ctry in ctries_to_add_aggs) {
  yr <- census_info %>% filter(country == ctry) %>% pull(yr2_suffix)
  agg_df <- readRDS(here(
    build.dir, "Countries", ctry, paste0(yr,"_mig_aggregates.rds")
  )
  )
  dat_df <- bind_rows(dat_df, agg_df)
}

mig_df <- dat_df %>%
  relocate(country_name, prev_country_name, ipums_id_d, ipums_id_o, district,
           prev_district, region, prev_region, mig_measure, mig_start_yr, 
           mig_end_yr, mig_prd_len) %>%
  var_labels(
    m_od = "migration from o to d",
    m_d = "total immigration at d",
    m_o = "total emigration from o",
    d_shares = "share of immig at d from o, m_od / m_d",
    o_shares = "share of emig from o at d , m_od / m_o",
    mig_measure = "migration survey measure",
  )

rm(d_df, o_df, distset_df)

# counting all international migrants in surveyed districts ----
dat_df <- data.frame()
for (ctry_str in countries){
  print(ctry_str)
  
  census_data <- census_info %>% 
    filter(country == ctry_str) %>%
    select(yr2_suffix, lvl, prime_age)
  
  census_yr <- census_data$yr2_suffix
  lvl <- census_data$lvl
  prime_age <- census_data$prime_age
  
  ctry_df <- readRDS(here(
    build.dir, "Countries", ctry_str,"Census", 
    paste0("census",census_yr,".rds")
  ))
  
  if(ctry_str == 'ZAF'){ 
    ctry_df %<>% 
      mutate(
        prev_country_name = ifelse(
          prev_country_name=='South Africa', 
          'Non-Migrants (International)', 
          prev_country_name)
      )
  }
  
  if("prev_country_name" %!in% names(ctry_df)){next}
  
  #migration flows for given years 
  if(ctry_str %in%  names(yr_start_dic)){
    geo <- g_sym(lvl)
    prev_geo <- g_sym(lvl, r = "p")
    
    ctry_df %<>% 
      filter(prev_country_name %in% sapply(countries, n_ctry)) %>%
      mutate(mig = wgt*((migration_year >= yr_start_dic[ctry_str] & 
                           migration_year <= yr_end_dic[ctry_str]) &
                          (!is.na(migration_year)))) %>%
      group_by({{geo}}, prev_country_name) %>% 
      summarize(m_pcd = sum(mig, na.rm = TRUE), #m_pcd = \sum_{o\in pctry}m_od 
                .groups='drop') %>%
      group_by(prev_country_name) %>%
      mutate(m_pcc = sum(m_pcd, na.rm = TRUE),
             pcd_shares = m_pcd/m_pcc)  %>% #of those who left their previous country (to go to ctry), how many went to d
      ungroup()%>%
      mutate(
        country_name = sapply(ctry_str, n_ctry)
      ) 
  }
  
  #flows for a fixed period 
  else{
    yr_end <- mean(ctry_df$year, na.rm=TRUE)
    if (is.na(period_dic[ctry_str]) == TRUE) {
      lag <- "birth"
      mig_meas <- "since birth" }
    else {
      lag <- "prev"
      mig_meas <- paste0("fixed: ", period_dic[ctry_str], " year")}
    
    geo <- g_sym(lvl)
    prev_geo <- g_sym(lvl, r = "p", l_stub = lag)
    
    if (prime_age == TRUE) {ctry_df %<>% filter(age >= min_pa & age <= max_pa)}
    
    ctry_df %<>% 
      filter(prev_country_name %in% sapply(countries, n_ctry)) %>%
      group_by({{geo}}, prev_country_name) %>% 
      summarize(m_pcd = sum(wgt, na.rm = TRUE), 
                .groups="drop") %>%
      group_by(prev_country_name) %>%
      mutate(m_pcc = sum(m_pcd, na.rm = TRUE),
             pcd_shares = m_pcd/m_pcc)  %>%
      ungroup()%>%
      mutate(
        country_name = sapply(ctry_str, n_ctry)
        )
  }
  
  dat_df <- bind_rows(dat_df, ctry_df)
}
dat_df %<>% 
  relocate(
    country_name, prev_country_name, district, region, m_pcd, m_pcc, pcd_shares
  )

rm(ctry_df)

# imputing m_od for international migrants ----

matching_pairs <- inner_join(
  ctryset_df, dat_df, by = c(
    "country_name", "prev_country_name", "district", "region")
) %>%
  mutate(
    co_shares = NA,
    prev_location = ifelse(is.na(prev_district), prev_region, prev_district)
  )

district_lookup <- matching_pairs %>%
  filter(!is.na(district)) %>%
  select(swap_country = country_name, swap_prev_country = prev_country_name, 
         match_location = district, pcd_shares)

region_lookup <- matching_pairs %>%
  filter(!is.na(region)) %>%
  select(swap_country = country_name, swap_prev_country = prev_country_name, 
         match_location = region, pcd_shares)

symmetric_lookup <- bind_rows(district_lookup, region_lookup) %>%
  group_by(swap_country, swap_prev_country, match_location) %>%
  slice_head(n = 1) %>%  # Take first match like original [1,]
  ungroup()

country_pairs <- matching_pairs %>%
  distinct(country_name, prev_country_name) %>%
  mutate(pair_exists = TRUE)

intmig_df <- matching_pairs %>%
  left_join(
    symmetric_lookup,
    by = c("prev_country_name" = "swap_country",
           "country_name" = "swap_prev_country", 
           "prev_location" = "match_location")
  ) %>%
  left_join(
    country_pairs,
    by = c("prev_country_name" = "country_name", 
           "country_name" = "prev_country_name")
  ) %>%
  mutate(
    co_shares = case_when(
      !is.na(pcd_shares.y) ~ pcd_shares.y,    # Symmetric match found
      pair_exists == TRUE ~ 0,                # Country pair exists  
      TRUE ~ NA_real_                         # No pair exists
    )
  ) %>%
  select(-ends_with(".y"), -pair_exists, -prev_location) %>% 
  rename(pcd_shares = pcd_shares.x) %>% 
  mutate(m_od = round(m_pcd*co_shares),
         m_o = 0, m_d = 0, o_shares = 0, d_shares=0) %>%
  select(-c(m_pcc, m_pcd, pcd_shares, co_shares))

mig_vars <- select(mig_df, 'country_name', 'mig_measure', 'mig_start_yr', 'mig_end_yr', 'mig_prd_len') %>% distinct()
moo_df <- select(mig_df, "ipums_id_d", "m_oo") %>% distinct()

intmig_df %<>% left_join(moo_df, by=join_by('ipums_id_d'))%>%
  left_join(mig_vars, by=join_by('country_name')) 

rm(
  matching_pairs, mig_vars, moo_df, dat_df,
  symmetric_lookup, region_lookup, district_lookup, country_pairs
)

#binding intmig and mig datasets into a single one. ----

final_df <- bind_rows(intmig_df, mig_df) %>%
  relocate(country_name, prev_country_name, ipums_id_d, ipums_id_o, district, prev_district, 
           region, prev_region, mig_measure, mig_start_yr, mig_end_yr, 
           mig_prd_len) %>%
  group_by(ipums_id_d) %>%
  mutate(intm_d = sum(m_od, na.rm=TRUE)) %>%
  ungroup()%>%
  group_by(ipums_id_o) %>%
  mutate(intm_o = sum(m_od, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    intd_shares  = (m_od / intm_d),
    into_shares  = (m_od / intm_o),
    into_shares = ifelse(is.infinite(into_shares), NaN, into_shares),
    scale_factor = case_when(
      is.na(mig_prd_len) ~ (2/3), # since birth, approx with 15 year avg. duration
      mig_prd_len == 1 ~ 1 + 0.95 + 0.95^2 + 0.95^3 + 0.95^4 + 0.95^5 + 0.95^6 + 0.95^7 + 0.95^8 + 0.95^9,  
      mig_prd_len == 2 ~ (1 + 0.95 + 0.95^2 + 0.95^3 + 0.95^4 + 0.95^5 + 0.95^6 + 0.95^7 + 0.95^8 + 0.95^9) / (1 + 0.95),
      mig_prd_len == 5 ~ (1 + 0.95 + 0.95^2 + 0.95^3 + 0.95^4 + 0.95^5 + 0.95^6 + 0.95^7 + 0.95^8 + 0.95^9) / (1 + 0.95 + 0.95^2 + 0.95^3 + 0.95^4),  
      mig_prd_len == 10 ~ 1,
      TRUE ~ NA_real_
    ),
    # scale and ignore international migration
    m_od_10years = ifelse(country_name != prev_country_name, 0, m_od * scale_factor)
  ) %>%
  group_by(ipums_id_d) %>%
  mutate(
    N0 = sum(m_od, na.rm = TRUE) + m_oo,
    m_d_10years = sum(m_od_10years, na.rm = TRUE),
    m_oo_10years = N0 - m_d_10years
  ) %>%
  ungroup() %>%
  group_by(ipums_id_o) %>%
  mutate(m_o_10years = sum(m_od_10years, na.rm = TRUE)) %>%
  ungroup() 

saveRDS(final_df, here(out.dir, "R", "full_migration_census2.rds"))
write_dta(final_df, here(out.dir, "full_migration_census2.dta"))

# creating emig and immig dfs: ----

id_df <- readRDS(here(out.dir, "R", "id.rds"))

emig_df <- final_df %>%
  filter(country_name == prev_country_name) %>%
  select(ipums_id_o, ends_with("m_o"), starts_with("mig")) %>%
  distinct(ipums_id_o, .keep_all = TRUE) %>%
  rename(emig = m_o, iemig = intm_o, ipums_id = ipums_id_o) %>%
  rename_with(~paste0("e", .), starts_with("mig")) %>%
  mutate(emig_ann = emig / emig_prd_len,
         log_emig = ifelse(emig > 0, log(emig), 0),
         log_emig_ann = log_emig / emig_prd_len,
         iemig_ann = iemig / emig_prd_len,
         ilog_emig = ifelse(iemig > 0, log(iemig), 0),
         ilog_emig_ann = ilog_emig / emig_prd_len,
         emig_measure = paste0("window: ", emig_prd_len, " year")) %>%
  left_join(id_df, by = "ipums_id") %>%
  relocate(country_name, ipums_id, admin_name, geo_lvl, 
           emig_start_yr, emig_end_yr, emig_prd_len, emig_measure) %>%
  var_labels(
    emig = "emigration, prime-aged",
    emig_ann = "annual emigration, prime-aged",
    log_emig = "log emigration, prime-aged",
    log_emig_ann = "annual log emigration, prime-aged",
    iemig = "international emigration, prime-aged",
    iemig_ann = "annual international emigration, prime-aged",
    ilog_emig = "log international emigration, prime-aged",
    ilog_emig_ann = "annual log international emigration, prime-aged",
    emig_measure = "emigration measure",
    emig_start_yr = "emigration period start year",
    emig_end_yr = "emigration period end year",
    emig_prd_len = "emigration period lenfth, years")

saveRDS(emig_df, here(out.dir, "R", "emigration_census2.rds"))
write_dta(emig_df, here(out.dir, "emigration_census2.dta"))

immig_df <- final_df %>%
  filter(country_name == prev_country_name) %>%
  select(ipums_id_d, ends_with("m_d"), starts_with("mig")) %>%
  distinct(ipums_id_d, .keep_all = TRUE) %>%
  rename(immig = m_d, iimmig = intm_d, ipums_id = ipums_id_d) %>%
  rename_with(~paste0("im", .), starts_with("mig")) %>%
  mutate(immig_ann = immig / immig_prd_len,
         log_immig = ifelse(immig > 0, log(immig), 0),
         log_immig_ann = log_immig / immig_prd_len,
         iimmig_ann = iimmig / immig_prd_len,
         ilog_immig = ifelse(iimmig > 0, log(iimmig), 0),
         ilog_immig_ann = ilog_immig / immig_prd_len,
         immig_measure = paste0("window: ", immig_prd_len, " year")) %>%
  left_join(id_df, by = "ipums_id") %>%
  relocate(country_name, ipums_id, admin_name, geo_lvl, 
           immig_start_yr, immig_end_yr, immig_prd_len, immig_measure) %>%
  var_labels(
    immig = "immigration, prime-aged",
    immig_ann = "annual immigration, prime-aged",
    log_immig = "log immigration, prime-aged",
    log_immig_ann = "annual log immigration, prime-aged",
    iimmig = "international immigration, prime-aged",
    iimmig_ann = "annual international immigration, prime-aged",
    ilog_immig = "log international immigration, prime-aged",
    ilog_immig_ann = "annual log international immigration, prime-aged",
    immig_measure = "immigration measure",
    immig_start_yr = "immigration period start year",
    immig_end_yr = "immigration period end year",
    immig_prd_len = "immigration period lenfth, years")

saveRDS(immig_df, here(out.dir, "R", "immigration_census2.rds"))
write_dta(immig_df, here(out.dir, "immigration_census2.dta"))

rm(intmig_df, mig_df)