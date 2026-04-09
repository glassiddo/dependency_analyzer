## mean and standard deviation of z_i  and  g_k

#### describe the distribution of the shifts
#### after residualizing them on shift-level controls 

#### regress predetermined balance vars on the instrument, controlling  for 
#### the sum of shares interacted with period fixed effects

## hhi 

hhi_df <- grid_origin_exposure %>%
  left_join(control_Si, by = "gid") %>% 
  filter(S_i > 0) %>% 
  mutate(r_io_sq = (s_ik / S_i)^2) %>% ## hhi index
  group_by(gid, iso_a3) %>%
  summarise(HHI_i = sum(r_io_sq, na.rm = TRUE), .groups = "drop")

origin_weights <- grid_origin_exposure %>%
  group_by(iso3c_o) %>%
  summarise(weight = sum(s_ik, na.rm = TRUE), .groups="drop") %>%
  mutate(share = weight / sum(weight))
overall_eff_num <- 1 / sum(origin_weights$share^2)
overall_eff_num

analysis_df <- analysis_df %>%
  left_join(hhi_df, by = "gid")

# Check if high-HHI grids drive your instrument or outcomes
cor(analysis_df$z_it, analysis_df$HHI_i, 
    use = "pairwise.complete.obs")

grid_origin_exposure %>%
  filter(s_ik > 0) %>% 
  group_by(gid) %>%
  summarise(HHI_i = sum((s_ik/sum(s_ik))^2, na.rm = TRUE),
            N_eff_i = 1 / HHI_i) %>%
  summarise(mean_N_eff = mean(N_eff_i, na.rm = TRUE))

top_origin_weights <- shares_tourism_afr %>% 
  # could be computed with the s_ik of grid_exposure instead
  group_by(iso3c_o) %>%
  summarise(weight = sum(w_ck0, na.rm = TRUE)) %>%
  mutate(share = weight / sum(weight)) %>%
  arrange(desc(share))

head(top_origin_weights, 10)

# Calculate HHI at destination level
hhi_country <- shares_tourism_afr %>%
  group_by(iso3c_d) %>%
  summarise(
    HHI_d = sum(w_ck0^2, na.rm = TRUE),
    N_eff_d = 1 / HHI_d,        # effective number of origins
    n_origins = n_distinct(iso3c_o)
  ) %>%
  arrange(desc(HHI_d))

hhi_country

g_shocks_reg <- tourism_ww %>%
  left_join(ctries %>% 
              select(iso3c_o = iso3c, region_o = region, income_level),
            by = c("iso3c_o")) %>%
  group_by(region_o, year) %>%
  summarise(g_region_t = mean(gr_tour_o, na.rm = TRUE), .groups = "drop")

g_shocks_inc <- tourism_ww %>%
  left_join(ctries %>% 
              select(iso3c_o = iso3c, region_o = region, income_level),
            by = c("iso3c_o")) %>%
  group_by(income_level, year) %>%
  summarise(g_income_t = mean(gr_tour_o, na.rm = TRUE), .groups = "drop")

# Add region to your grid_origin_exposure
grid_origin_cluster <- grid_origin_exposure %>%
  left_join(ctries %>% 
              select(iso3c_o = iso3c, region_o = region, income_level),
            by = c("iso3c_o")) %>%
  group_by(gid, region_o) %>%
  summarise(s_ik_cluster = sum(s_ik, na.rm = TRUE), .groups = "drop")

hhi_cluster_df <- grid_origin_cluster %>%
  left_join(control_Si, by = "gid") %>%
  mutate(r_ic_sq = (s_ik_cluster / S_i)^2) %>%
  group_by(gid) %>%
  summarise(HHI_cluster_i = sum(r_ic_sq, na.rm = TRUE), .groups = "drop")

summary(hhi_cluster_df$HHI_cluster_i)