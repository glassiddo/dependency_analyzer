source("do/conf_biodiversity_setup.R")

conf_df <- fread(here(cln_dir, "data_unlabeled.csv"))

grid <- read_sf(here(int_dir, "grid.gpkg")) %>%
  select(gid)

african_ctrys <- conf_df %>% 
  select(iso3c = iso_a3) %>% 
  unique() %>% 
  pull()

tourism <- fread(here(int_dir, "temp_tourism.csv")) %>% 
  mutate(
    year = as.integer(year),
    visitors = as.numeric(visitors)
  ) 

ctries <- wbstats::wb_cachelist$countries %>%
  filter(region != "Aggregates") %>%
  select(country, iso3c, income_level, region) %>% 
  unique() 
  
hi_ctries <- ctries %>% 
  filter(income_level == "High income") %>% 
  select(iso3c) %>% 
  pull()

tourism$iso3c_o <- countrycode::countrycode(
  sourcevar   = tourism$origin,
  origin      = "country.name",
  destination = "iso3c",
  warn        = TRUE
) 

tourism$iso3c_d <- countrycode::countrycode(
  sourcevar   = tourism$destination,
  origin      = "country.name",
  destination = "iso3c",
  warn        = TRUE
)

### some prep -----
baseline_year <- 2000

tourism_ww <- tourism %>% 
  left_join(ctries %>% select(iso3c, region) %>% rename(iso3c_o = iso3c, region_o = region), 
            by = "iso3c_o") %>%
  left_join(ctries %>% select(iso3c, region) %>% rename(iso3c_d = iso3c, region_d = region), 
            by = "iso3c_d") %>%
  filter(
    iso3c_o %in% hi_ctries, 
    region_o != "Sub-Saharan Africa", 
    region_d != "Sub-Saharan Africa"
  ) %>%
  filter(region_o != region_d) %>%
  group_by(iso3c_o, year) %>% 
  summarise(
    total_visitors_o = sum(visitors, na.rm = T)
  ) %>% 
  ungroup()

tourism_ww0 <- tourism_ww %>% 
  filter(year == baseline_year) %>% 
  select(iso3c_o, total_visitors_o_baseline = total_visitors_o) 

rel_ctries <- tourism_ww0 %>% 
  filter(total_visitors_o_baseline > 10000) %>% 
  select(iso3c_o) %>% 
  pull()

tourism_afr <- tourism %>% 
  filter(
    !is.na(iso3c_o) & 
      iso3c_d %in% african_ctrys &
      iso3c_o %in% rel_ctries &
      iso3c_o %in% hi_ctries
  ) %>% 
  select(iso3c_d, iso3c_o, year, visitors) %>% 
  arrange(iso3c_d, year, iso3c_o)

ctries_with_full_data <- tourism_afr %>%
  group_by(iso3c_d, year) %>%
  summarise(total_visitors = sum(visitors, na.rm = TRUE), .groups = 'drop') %>%
  mutate(total_visitors = ifelse(total_visitors == 0, NA, total_visitors)) %>%
  pivot_wider(
    names_from = year,
    values_from = total_visitors
  ) %>% 
  filter(!is.na(`2000`)) %>% 
  select(iso3c_d) %>%
  unique() %>%
  pull()


tourism_ww <- tourism_ww %>% 
  filter(iso3c_o %in% rel_ctries) %>% # ignore tiny countries
  left_join(tourism_ww0, by = "iso3c_o") %>% 
  mutate(
    gr_tour_o = 
      (total_visitors_o-total_visitors_o_baseline) / 
      total_visitors_o_baseline
  ) %>% 
  select(iso3c_o, year, gr_tour_o)


#### tourism by year -----
years <- sort(unique(tourism$year))
tourism_shares_list <- list()

for (yr in years) {
  
  # Calculate total visitors by destination country for this year
  t_ctry <- tourism_afr %>%
    filter(year == yr, iso3c_o %in% rel_ctries) %>% 
    group_by(iso3c_d) %>%
    summarise(
      total_visitors = sum(visitors, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Calculate shares for this year
  t_year <- tourism_afr %>% 
    filter(year == yr, iso3c_o != "TOTAL") %>% 
    left_join(t_ctry, by = "iso3c_d") %>% 
    mutate(
      w_ck = visitors / total_visitors,
      year = yr
    ) %>% 
    select(year, iso3c_d, iso3c_o, w_ck, visitors, total_visitors)
  
  # Store in list
  tourism_shares_list[[as.character(yr)]] <- t_year
}

# Combine all years into one dataframe
tourism_shares_panel <- bind_rows(tourism_shares_list)

# Extract baseline year shares
baseline_shares <- tourism_shares_panel %>%
  filter(year == baseline_year) %>%
  select(iso3c_d, iso3c_o, w_ck_baseline = w_ck)

# Merge baseline shares with all years
tourism_shares_with_baseline <- tourism_shares_panel %>%
  left_join(baseline_shares, by = c("iso3c_d", "iso3c_o"),
            relationship = 'many-to-many') %>% 
  filter(year > baseline_year)

feols(
  w_ck ~ w_ck_baseline | iso3c_d + iso3c_o + year,
  data = tourism_shares_with_baseline
)

# Alternative: by year regressions
reg_by_year <- tourism_shares_with_baseline %>%
  filter(year > baseline_year) %>%
  group_by(year) %>%
  group_map(~ {
    feols(w_ck ~ w_ck_baseline | iso3c_d + iso3c_o, data = .x)
  }, .keep = TRUE)

reg_by_year



######### actual instrument -----
bl_tourism_afr <- tourism_afr %>%
  filter(year == baseline_year,
         iso3c_o %in% rel_ctries) %>% 
  group_by(iso3c_d) %>%
  summarise(
    total_visitors = sum(visitors, na.rm = T)
  ) %>%
  ungroup() 

shares_tourism_afr <- tourism_afr %>% 
  filter(iso3c_d %in% ctries_with_full_data) %>% 
  filter(year == baseline_year, !is.na(visitors)) %>% 
  left_join(bl_tourism_afr, by = "iso3c_d") %>% 
  mutate(
    w_ck0 = visitors / total_visitors
  ) %>% 
  select(iso3c_d, iso3c_o, w_ck0)

shares_grid_country <- conf_df %>%
  #filter(iso_a3 %in% ctries_with_full_data) %>% 
  filter(year == baseline_year) %>%
  group_by(iso_a3) %>%
  mutate(
    s_ic0 = 
      avg_distinct_birds_m_roll_5yr / 
      sum(avg_distinct_birds_m_roll_5yr, na.rm = TRUE),
    s_ic0 = ifelse(is.na(s_ic0), 0, s_ic0)
  ) %>%
  ungroup() %>%
  select(gid, iso_a3, s_ic0)

grid_origin_exposure <- shares_grid_country %>%
  left_join(
    shares_tourism_afr, 
    by = c("iso_a3" = "iso3c_d"),
    relationship = "many-to-many"
  ) %>% 
  mutate(
    s_ik = s_ic0 * w_ck0
  ) %>%
  select(gid, iso_a3, iso3c_o, s_ik)

instrument_zit <- grid_origin_exposure %>%
  left_join(
    tourism_ww, by = "iso3c_o",
    relationship = "many-to-many"
  ) %>% 
  filter(year > baseline_year) %>% 
  group_by(gid, year) %>%
  summarise(
    z_it = sum(s_ik * gr_tour_o, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ungroup()

shocks_yr <- grid %>%
  left_join(
    conf_df %>% 
      filter(year == 2006) %>% 
      select(country, gid),
    by = "gid"
  ) %>% 
  left_join(
    instrument_zit %>% 
      filter(year == 2006) %>% 
      select(gid, z_it), 
    by = "gid"
  )

ggplot(data = shocks_yr %>% 
         filter(
           country %in% c("Tanzania", "Kenya")
         ), 
       aes(fill = z_it)) + 
  geom_sf() + 
  scale_fill_viridis_c()

control_Si <- grid_origin_exposure %>% ## unnecessary as should sum to 1 (?)
  group_by(gid,) %>% ## also absorbed by gid FE
  summarise(S_i = sum(s_ik, na.rm = TRUE))

analysis_df <- conf_df %>%
  ## currently have data only for 2000-2014
  filter(
    year > baseline_year & year < 2015
  ) %>% 
  left_join(instrument_zit, by = c("gid", "year")) %>% 
  left_join(control_Si, by = "gid")

model_2sls <- feols(
  total_events ~ asinh(population) + share_area_protected |
    gid + year |
    avg_distinct_birds_m_roll_5yr ~ z_it
  ,
  data = analysis_df,
  cluster = ~gid_2deg ## need to check about clustering
)

first_stage <- feols(
  avg_distinct_birds_m_roll_5yr ~ z_it + asinh(population) + share_area_protected
  | gid + year
  ,
  data = analysis_df,
  cluster = ~gid_2deg ## need to check
)

model_2sls
first_stage

analysis_df <- analysis_df %>%
  group_by(gid) %>%
  mutate(conflict_lagged = dplyr::lag(total_events, n = 1)) %>%
  ungroup()

pre_trend_test <- feols(
  conflict_lagged ~ z_it + asinh(population) + share_area_protected |
    gid + year
  ,
  data = analysis_df,
  cluster = ~gid_2deg ## need to check about clustering
)

balance_pop <- feols(
  asinh(population) ~ z_it |
    gid + year
  ,
  data = analysis_df,
  cluster = ~gid_2deg ## need to check about clustering
)

balance_pa <- feols(
  share_area_protected ~ z_it |
    gid + year
  ,
  data = analysis_df,
  cluster = ~gid_2deg ## need to check about clustering
)

pre_trend_test
balance_pop
balance_pa
