rm(list=ls())
pacman::p_load(
  dplyr, haven, sandwich, lmtest, plm, reshape2, data.table, 
  tidyverse, stargazer, ggplot2, purrr, readxl, plotrix, classInt, 
  geodata, spData, sf, terra, maps, sp, raster, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  ncdf4, # Read, write, and create netCDF files
  bacondecomp,  # Goodman-Bacon decomposition 
  did, # callaway santanna event study 
  fixest, # sun abraham even study
  DIDmultiplegtDYN, # chaisemartin and d'
  staggered, # staggered treatments
  conleyreg, # spatial standard errors conley
  lfe # felm
)

setwd("C:/Users/iddo2/Nextcloud/conflict and protected areas")
select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

# read data ----
merged_data_full_geom <- st_read("data/cln/merged_data_full_more_vars.gpkg")
#merged_data_full_v2 <- merged_data_full_geom %>% st_set_geometry(NULL)
merged_data_full_geom <- merged_data_full_geom %>% filter(year == 2010) # for graphs
merged_data_full <- readRDS("data/cln/data_with_labels.rds")
gc()

country_wdpa <- fread("data/intermediate/country_wdpa.csv")
merged_data_full <- merged_data_full %>% 
  left_join(country_wdpa, by = c("year", "country"))

### feols (main regression) ----
print(summary(feols(
  Conflicts_dum 
  #Fatalities_dum
  #Conflicts_dum_jihad
  #Conflicts_dum_non_state
  ~  
  #+ Over30*distance_to_capital
  + n_distinct_viewers_year_inat*predicted_state_presence
  #+ Over30*big_game
  #+ forest_cover # in the year 2000
  #+ activity # dominant subsistence activity - from ethnographic atlas
  + lpop 
  + precipitation 
  #+ rain # rain from mcguirk and nunn (2024), might be (or might not be) more accurate
  #+ area # to an extent, controlling for being a coastal grid cell (as they are smaller)
  | gid + country_year, 
  data = merged_data_full,
  vcov = cluster ~ gid_1deg + country_year
  )))

big34 <- merged_data_full_geom %>% filter(num_big_game > 2)
### plot to visualize a variable - for the year 2010
ggplot() +
  geom_sf(data = big34, aes(
    fill = distance_to_min5m_city
    )) +
  scale_fill_viridis_c() + # continous
  #scale_fill_viridis_d() + # discrete
  theme_minimal()
  #theme(legend.position="none")

### felm ----
# est_cl <- felm(Conflicts_dum ~ Over10 + lpop
#                + Over10*pastor_narrow 
#                + Over10*nbor_pastor_narrow
#                + precipitation 
#                | gid + country_year
#                | 0
#                | country_year + gid_1deg, 
#                data = merged_data_full)
# summary(est_cl)

# # Event studies ------
# 
# ### chaisemartin ----
# m1norang <- merged_data_full %>%
#   mutate(strong_state = case_when(
#     predicted_state_presence > 0.365 ~ 4,
#     predicted_state_presence > 0.286 ~ 3,
#     predicted_state_presence > 0.216 ~ 2,
#     predicted_state_presence > 0 ~ 1,
#     TRUE ~ NA
#   )) %>%
#   filter(!is.na(strong_state))


# 
# ### staggered package ----
# 
# eventstudy_data_nogeom_noat <- eventstudy_data %>% 
#   filter(always_treated == 0) 
# #mutate( first_treat = ifelse(is.na(first_treat), 100, first_treat))
# 
# eventPlotResults <- staggered(
#   df = eventstudy_data_nogeom_noat, 
#   i = "gid",
#   t = "Year",
#   g = "first_treat",
#   y = "Conflicts_dum", 
#   #estimand = "simple",
#   estimand = "eventstudy",
#   eventTime = -5:5
# )
# 
# eventPlotResults %>% 
#   mutate(ymin_ptwise = estimate + 1.96*se,
#          ymax_ptwise = estimate - 1.96*se)%>%
#   ggplot(aes(x=eventTime, y =estimate)) +
#   geom_pointrange(aes(ymin = ymin_ptwise, ymax = ymax_ptwise))+ 
#   geom_hline(yintercept =0) +
#   xlab("Event Time") + ylab("Estimate")
# 
# # conley ----
# coef_conley <-conleyreg(Conflicts_dum ~ Over10 + lpop + precipitation
#                         | gid + country_year,
#                         data = merged_data_full,
#                         unit = "gid", 
#                         time = "year",
#                         lat = "lat", lon = "lon",
#                         #dist_fn = "SH", 
#                         dist_cutoff = 500, 
#                         lag_cutoff = 0,
#                         #cores = 1, 
#                         verbose = TRUE
# ) 
# print(coef_conley)
# 
# # bacon -----
# 
# random_gids <- sample(unique(merged_data_full$gid), 2000)
# m1 <- merged_data_full %>%
#   filter(gid %in% random_gids) %>%
#   select(gid, year, Conflicts_dum, Over10)
# 
# ## smaller df to actually enable it to run (very heavy function)
# df_bacon <- bacon(Conflicts_dum ~ Over10,
#                   data = m1,
#                   id_var = "gid",
#                   time_var = "year")
# 
# ggplot(df_bacon) +
#   aes(x = weight,
#       y = estimate,
#       #shape = factor(type),
#       colour = factor(type)) +
#   geom_point(size = 2) +
#   geom_hline(yintercept = 0,
#              linetype = "longdash") +
#   labs(x = "Weight",
#        y = "Estimate",
#        shape = "Type",
#        colour = "Type",
#        title = "Goodman-Bacon decomposition of diff-in-diff estimates vs weights") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
