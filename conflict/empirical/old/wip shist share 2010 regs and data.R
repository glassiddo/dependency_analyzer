source("do/conf_biodiversity_setup.R")

conf_df <- fread(here(cln_dir, "data_unlabeled.csv"))

grid <- read_sf(here(int_dir, "grid.gpkg")) %>%
  select(gid)

baseline_year <- 2000
bird_var <- "avg_distinct_birds_m_roll_5yr"
outcome <- "fatalities_dum"

tourism <- fread(here(int_dir, "temp_tourism.csv")) %>% 
  mutate(
    year = as.integer(year),
    visitors = as.numeric(visitors)
  ) %>% 
  filter(destination != "China") # excluded as some of their values in 2010 are clearly mistakes (in the pdf extraction)

ctries <- wbstats::wb_cachelist$countries %>%
  filter(region != "Aggregates") %>%
  select(country, iso3c, income_level, region) %>% 
  unique() 

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

# Prepare tourism data
rel_ctries <- tourism %>%
  filter(year == baseline_year) %>%
  group_by(iso3c_o) %>%
  summarise(total = sum(visitors, na.rm = TRUE)) %>%
  filter(total > 100000) %>%
  pull(iso3c_o)

tourism_ww0 <- tourism %>%
  left_join(ctries %>% select(iso3c, region) %>%
              rename(iso3c_o = iso3c, region_o = region),
            by = "iso3c_o") %>%
  left_join(ctries %>% select(iso3c, region) %>%
              rename(iso3c_d = iso3c, region_d = region),
            by = "iso3c_d") %>%
  filter(region_o != "Sub-Saharan Africa",
         region_o != region_d) %>%
  group_by(iso3c_o, year) %>%
  summarise(total_visitors_o = sum(visitors, na.rm = TRUE), .groups = "drop")

tourism_ww_baseline <- tourism_ww0 %>%
  filter(year == baseline_year) %>%
  select(iso3c_o, total_visitors_o_baseline = total_visitors_o)

tourism_ww <- tourism_ww0 %>%
  left_join(tourism_ww_baseline, by = "iso3c_o") %>%
  mutate(gr_tour_o = (total_visitors_o - total_visitors_o_baseline) / 
           total_visitors_o_baseline) %>%
  filter(iso3c_o %in% rel_ctries)

# Tourism to Africa
tourism_afr <- tourism %>%
  left_join(ctries %>% select(iso3c, region) %>%
              rename(iso3c_d = iso3c, region_d = region),
            by = "iso3c_d") %>%
  left_join(ctries %>% select(iso3c, region) %>%
              rename(iso3c_o = iso3c, region_o = region),
            by = "iso3c_o") %>%
  filter(region_d == "Sub-Saharan Africa",
         region_o != "Sub-Saharan Africa",
         iso3c_o %in% rel_ctries) %>%
  select(iso3c_d, iso3c_o, year, visitors)

# Baseline tourism shares
bl_tourism_afr <- tourism_afr %>% 
  filter(year == baseline_year)

ctries_with_suff_o <- bl_tourism_afr %>%
  filter(!is.na(visitors)) %>%
  group_by(iso3c_d) %>%
  summarise(n = n_distinct(iso3c_o)) %>%
  filter(n >= 5) %>%
  pull(iso3c_d)

bl_summary_afr <- bl_tourism_afr %>%
  group_by(iso3c_d) %>%
  summarise(total_visitors = sum(visitors, na.rm = TRUE)) %>%
  filter(total_visitors > 0)

shares_tourism_afr <- bl_tourism_afr %>%
  filter(iso3c_d %in% ctries_with_suff_o, !is.na(visitors)) %>%
  left_join(bl_summary_afr, by = "iso3c_d") %>%
  mutate(w_ck0 = visitors / total_visitors) %>%
  select(iso3c_d, iso3c_o, w_ck0)

# Grid-level shares based on bird variable
shares_grid_country <- conf_df %>%
  filter(year == baseline_year) %>%
  group_by(iso_a3) %>%
  mutate(s_ic0 = .data[[bird_var]] / sum(.data[[bird_var]], na.rm = TRUE),
         s_ic0 = ifelse(is.na(s_ic0), 0, s_ic0)) %>%
  ungroup() %>%
  select(gid, iso_a3, s_ic0)

grid_origin_exposure <- shares_grid_country %>%
  left_join(shares_tourism_afr, by = c("iso_a3" = "iso3c_d"),
            relationship = "many-to-many") %>%
  mutate(s_ik = s_ic0 * w_ck0) %>%
  select(gid, iso_a3, iso3c_o, s_ik)

# Construct instrument
instrument_zit <- grid_origin_exposure %>%
  left_join(tourism_ww, by = "iso3c_o", relationship = "many-to-many") %>%
  filter(year > baseline_year) %>%
  group_by(gid, year) %>%
  summarise(z_it = sum(s_ik * gr_tour_o, na.rm = TRUE), .groups = "drop")

# Prepare analysis data
analysis_df <- conf_df %>%
  filter(year > baseline_year & year < 2015) %>%
  left_join(instrument_zit, by = c("gid", "year"))

#############

### correlation between z and other initial characteristics at t=0
cov_o = WDI::WDI(
  indicator='NY.GDP.PCAP.KD.ZG', # gdp per capita growth
  #indicator='NY.GDP.PCAP.PP.CD', # GDP per capita, PPP 
  start=1995, 
  end=2024
) %>%
  left_join(ctries, by = c("iso3c", "country")) %>% 
  filter(
    region != "Sub-Saharan Africa", # Seychells and Mauritius
    iso3c %in% rel_ctries,
    year < 2015, year > 2000
  ) %>% 
  select(iso3c_o = iso3c, year, gdp_growth = NY.GDP.PCAP.KD.ZG) 


agg_controls <- grid_origin_exposure %>%
  left_join(cov_o, by = "iso3c_o", relationship = "many-to-many") %>% 
  # now each row is gid x origin x year with s_ik and q_ot
  mutate(sq = s_ik * gdp_growth) %>%
  group_by(gid, year) %>%
  summarise(Q_it = sum(sq, na.rm = TRUE), .groups = "drop")

#### values at 00 -- population, PA, mines, big5, colonial origin,
#### forest cover, rain

baseline <- conf_df %>% 
  filter(year == 1997) %>%
  select(
    gid, population, share_area_protected, mines, big5, big4, big3,
    colonial_origin, forest_cover, rain, climate
  ) %>% 
  rename_with(~paste0(., "_bl"), -c("gid")) 

control_Si <- grid_origin_exposure %>% 
  group_by(gid) %>% ## also absorbed by gid FE
  summarise(S_i = sum(s_ik, na.rm = TRUE))

analysis_df <- analysis_df %>% 
  left_join(baseline, by = "gid") %>% 
  left_join(control_Si, by = "gid") %>% 
  left_join(agg_controls, by = c("gid", "year")) %>% 
  mutate(S_i_x_year = S_i * year)  # or use year factor for non-linear trends

## sum of shares


##### population at 00
##### protected area at 00
# 
# library(ggpubr)
# ggplot(
#   data = analysis_df %>% filter(!is.na(z_it)),
#   aes(
#     x = z_it, 
#     y = year
#     )
#   ) + 
  # geom_point()
  #stat_cor(method="pearson")
# 
# feols(share_area_protected ~ z_it
#       | gid + year
#       ,
#       data = analysis_df,
#       cluster = ~gid_2deg)

### same with placebo outcomes - e.g.weather

### test the pre-trend?

### robustness by using each zk0 as K instruments, 
### rather than summarizing to Blt.

model_2sls <- feols(
  ihs_conflict ~ asinh(population) + share_area_protected + Q_it  |
    gid + year |
    avg_distinct_birds_m_roll_5yr ~ z_it,
  data = analysis_df,
  cluster = ~gid_2deg
)

model_2sls

