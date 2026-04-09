#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    June 8, 2022
#* Title:   Bartik
#* Desc:    Create bartik instruments using final datasets
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

id_df <- readRDS(file.path(out.dir, "R/id.rds"))
fl_df <- readRDS(file.path(out.dir, "R/forestloss.rds")) %>%
  select(-c(admin_name_l2:region_name))
nl_df <- readRDS(file.path(out.dir, "R/nightlight.rds"))
pop_df <- readRDS(file.path(out.dir, "R/population.rds"))
empl_df <- readRDS(file.path(out.dir, "R/employment.rds"))
ca_df <- readRDS(file.path(out.dir, "R/croparea.rds")) %>%
  select(-c(admin_name_l2:region_name))
emig_df <- readRDS(file.path(out.dir, "R/emigration.rds")) %>%
  select(-c(admin_name_l2:region))

mig_df <- readRDS(file.path(out.dir, "R/migration.rds"))
dist_df <- readRDS(file.path(out.dir, "R/distance.rds"))
urb_df <- readRDS(file.path(out.dir, "R/urban.rds"))

#*******************************************************************************
# Summary Table ----
# describing admin unit level and number of units. To add census years
id_tab <- id_df %>%
  mutate(units = 1) %>%
  group_by(country_name) %>%
  summarise(admin_level = mean(geo_lvl),
            units = sum(units)) %>%
  full_join(
    pop_df %>% select(country_name, 
                      census1 = pop_start_yr, 
                      census2 = pop_end_yr) %>%
      distinct()
  )

library(xtable)

print(xtable(id_tab, digits = 0), 
      only.contents = TRUE, 
      include.colnames = FALSE, 
      hline.after = NULL,
      include.rownames=FALSE, type = 'latex',
      #file = paste0("table.tex")
)

#*******************************************************************************
# pop Bartik ----
#* new names
#* delta_pop_ann_d = D_delta_Nd_avg
#* delta_log_pop_ann_d = D_delta_log_pop_d_avg
#* delta_urban_pop_ann_d = D_delta_Nd_urban
#* bl_pa_o = O_N0_pa

o_pop_df <- pop_df %>%
  select(country_name, ipums_id, bl_pa) %>%
  rename(ipums_id_o = ipums_id, bl_pa_o = bl_pa)

d_pop_df <- pop_df %>%
  select(country_name, ipums_id, delta_pop_ann, delta_log_pop_ann, 
         delta_urban_pop_ann) %>%
  rename_with(~paste0(.x, "_d"), !country_name)

Btk_pop_df <- full_join(mig_df, o_pop_df) %>%
  full_join(d_pop_df) %>%
  full_join(dist_df) %>%
  filter(ipums_id_d != ipums_id_o) %>%
  drop_na(bl_pa_o) %>%
  mutate(
    # Bartiks for change in dest pop / orig baseline pop
    Bm_d    = d_shares * (delta_pop_ann_d / bl_pa_o),
    Bm_o    = o_shares * (delta_pop_ann_d / bl_pa_o),
    Bd      = inv_dist_shares * (delta_pop_ann_d / bl_pa_o),
    Bd_avg  = avg_dist_shares * (delta_pop_ann_d / bl_pa_o),
    # Bartiks for delta log pop at destination
    Bm_d_dlp = d_shares * delta_log_pop_ann_d,
    Bm_o_dlp = o_shares * delta_log_pop_ann_d,
    Bd_dlp   = inv_dist_shares * delta_log_pop_ann_d,
    Bd_avg_dlp = avg_dist_shares * delta_log_pop_ann_d,
    # Bartiks for urban pop
    Bm_d_urban  = d_shares * (delta_urban_pop_ann_d / bl_pa_o),
    Bm_o_urban  = d_shares * (delta_urban_pop_ann_d / bl_pa_o),
    Bd_urban    = inv_dist_shares * (delta_urban_pop_ann_d / bl_pa_o),
    Bd_avg_urban    = avg_dist_shares * (delta_urban_pop_ann_d / bl_pa_o),
  ) %>%
  group_by(ipums_id_o, country_name) %>%
  summarise(across(c(starts_with("Bm"), starts_with("Bd")), 
                   ~sum(.x, na.rm = TRUE)),
            #d_shares_sum = sum(d_shares),
            #dist_shares_sum = sum(inv_dist_shares),
            #avg_dist_shares = mean(avg_dist_shares),
            .groups = 'drop') %>%
  rename(ipums_id = ipums_id_o)

#*******************************************************************************
# empl Bartik ----
#* new names
#* delta_log_nonag_ann_d = D_log_gr_nonag
#* delta_pop_ann_d = D_delta_Nd_avg
#* delta_log_pop_ann_d = D_delta_log_pop_d_avg
#* bl_pa_o = O_N0_pa


d_empl_df <- empl_df %>%
  select(country_name, ipums_id, delta_log_nonag_ann, delta_nonag_ann,
         delta_ag_ann, delta_mining_ann) %>%
  rename_with(~paste0(.x, "_d"), !country_name)

Btk_empl_df <- full_join(mig_df, o_pop_df) %>%
  full_join(d_empl_df) %>%
  full_join(dist_df) %>%
  filter(ipums_id_d != ipums_id_o) %>%
  drop_na(bl_pa_o) %>%
  drop_na(delta_nonag_ann_d) %>%
  mutate(
    # Bartiks for delta log non agricultural employment
    Bm_d_gr_nonag = d_shares * delta_log_nonag_ann_d,
    Bm_o_gr_nonag = o_shares * delta_log_nonag_ann_d,
    Bd_gr_nonag   = inv_dist_shares * delta_log_nonag_ann_d,
    Bd_avg_gr_nonag = avg_dist_shares * delta_log_nonag_ann_d,
    # other miscellaneous variants
    Bm_d_nonag  = d_shares * (delta_nonag_ann_d / bl_pa_o),
    Bd_nonag    = inv_dist_shares * (delta_nonag_ann_d / bl_pa_o),
    Bd_avg_nonag    = avg_dist_shares * (delta_nonag_ann_d / bl_pa_o),
    
    Bm_d_ag     = d_shares * (delta_ag_ann_d / bl_pa_o),
    Bd_ag       = inv_dist_shares * (delta_ag_ann_d / bl_pa_o),
    Bm_d_mining = d_shares * (delta_mining_ann_d / bl_pa_o),
    Bd_mining    = inv_dist_shares * (delta_mining_ann_d / bl_pa_o),
  ) %>%
  group_by(ipums_id_o, country_name) %>%
  summarise(across(c(starts_with("Bm"), starts_with("Bd")), 
                   ~sum(.x, na.rm = TRUE)),
            .groups = 'drop') %>%
  rename(ipums_id = ipums_id_o)

#*******************************************************************************
# immig Bartik ----

Btk_i_df <- mig_df %>%
  select(country_name, ipums_id_o, ipums_id_d, d_shares) %>%
  rename(tvar = ipums_id_o, ipums_id_o = ipums_id_d) %>%
  rename(ipums_id_d = tvar) %>%
  full_join(o_pop_df) %>%
  full_join(d_pop_df) %>%
  full_join(d_empl_df) %>%
  full_join(dist_df) %>%
  filter(ipums_id_d != ipums_id_o) %>%
  drop_na(bl_pa_o) %>%
  mutate(
    Bi   = d_shares * (delta_pop_ann_d / bl_pa_o),
    Bi_ag    = d_shares * (delta_ag_ann_d / bl_pa_o),
    Bi_nonag = d_shares * (delta_nonag_ann_d / bl_pa_o),
    Bi_urban = d_shares * (delta_urban_pop_ann_d / bl_pa_o)) %>%
  group_by(ipums_id_o, country_name) %>%
  summarise(across(starts_with("Bi"), ~sum(.x, na.rm = TRUE)),
            .groups = 'drop') %>%
  rename(ipums_id = ipums_id_o)

#*******************************************************************************
# night light Bartik ----
#* new names
#* delta_log_nonag_ann_d = D_log_gr_nonag
#* delta_pop_ann_d = D_delta_Nd_avg
#* delta_log_pop_ann_d = D_delta_log_pop_d_avg
#* bl_pa_o = O_N0_pa

d_nl_df <- nl_df %>%
  select(country_name, ipums_id, delta_ntla, delta_log_ntlv, delta_log_lag_ntlv, 
         delta_lag_ntla, delta_nla, delta_log_nlv, delta_nla_ra, delta_log_nlv_ra,) %>%
  rename_with(~paste0(.x, "_d"), !country_name)

o_nl_df <- nl_df %>%
  select(country_name, ipums_id, bl_ntla, bl_log_ntlv, bl_nla, bl_log_nlv,
         bl_nla_ra, bl_log_nlv_ra) %>%
  rename_with(~paste0(.x, "_o"), !country_name)

Btk_nl_df <- full_join(mig_df, d_nl_df) %>%
  full_join(o_nl_df) %>%
  full_join(dist_df) %>%
  filter(ipums_id_d != ipums_id_o) %>%
  drop_na(bl_ntla_o) %>%
  mutate(
    # Bartik with dest share, origin nitelight denominator shift
    Bd_ntla1 = inv_dist_shares * (delta_ntla_d / (1 + bl_ntla_o)),
    Bd_ntlv1 = inv_dist_shares * (delta_log_ntlv_d / (1 + bl_log_ntlv_o)),
    Bm_ntla1 = d_shares * (delta_ntla_d / (1 + bl_ntla_o)),
    Bm_ntlv1 = d_shares * (delta_log_ntlv_d / (1 + bl_log_ntlv_o)),
    # log change at d bartiks
    Bm_ntlv = d_shares * (delta_log_ntlv_d),
    Bm_ntla = d_shares * (delta_ntla_d),
    Bm_ntlvo = o_shares * (delta_log_ntlv_d),
    Bm_ntlao = o_shares * (delta_ntla_d),
    Bd_ntla   = inv_dist_shares * (delta_ntla_d),
    Bd_ntlv   = inv_dist_shares * (delta_log_ntlv_d),
    Bd_avg_ntlv = avg_dist_shares * (delta_log_ntlv_d),
    Bd_avg_ntla = avg_dist_shares * (delta_ntla_d),
    # lagged ntl changes
    Bm_ntlv_lag = d_shares * (delta_log_lag_ntlv_d),
    Bm_ntla_lag = d_shares * (delta_lag_ntla_d),
    Bm_ntlvo_lag = o_shares * (delta_log_lag_ntlv_d),
    Bm_ntlao_lag = o_shares * (delta_lag_ntla_d),
    Bd_avg_ntlv_lag = avg_dist_shares * (delta_log_lag_ntlv_d),
    Bd_avg_ntla_lag = avg_dist_shares * (delta_lag_ntla_d),
    Bd_ntla_lag   = inv_dist_shares * (delta_lag_ntla_d),
    Bd_ntlv_lag   = inv_dist_shares * (delta_log_lag_ntlv_d),
    #*********** harmonized values changes
    Bd_nla1 = inv_dist_shares * (delta_nla_d / (1 + bl_nla_o)),
    Bd_nlv1 = inv_dist_shares * (delta_log_nlv_d / (1 + bl_log_nlv_o)),
    Bm_nla1 = d_shares * (delta_nla_d / (1 + bl_nla_o)),
    Bm_nlv1 = d_shares * (delta_log_nlv_d / (1 + bl_log_nlv_o)),
    # log change at d bartiks
    Bm_nlv = d_shares * (delta_log_nlv_d),
    Bm_nla = d_shares * (delta_nla_d),
    Bm_nlvo = o_shares * (delta_log_nlv_d),
    Bm_nlao = o_shares * (delta_nla_d),
    Bd_nla   = inv_dist_shares * (delta_nla_d),
    Bd_nlv   = inv_dist_shares * (delta_log_nlv_d),
    Bd_avg_nlv = avg_dist_shares * (delta_log_nlv_d),
    Bd_avg_nla = avg_dist_shares * (delta_nla_d),
    #*********** harmonized values rolling average
    Bd_nla1_ra = inv_dist_shares * (delta_nla_ra_d / (1 + bl_nla_ra_o)),
    Bd_nlv1_ra = inv_dist_shares * (delta_log_nlv_ra_d / (1 + bl_log_nlv_ra_o)),
    Bm_nla1_ra = d_shares * (delta_nla_ra_d / (1 + bl_nla_ra_o)),
    Bm_nlv1_ra = d_shares * (delta_log_nlv_ra_d / (1 + bl_log_nlv_ra_o)),
    # log change at d bartiks
    Bm_nlv_ra = d_shares * (delta_log_nlv_ra_d),
    Bm_nla_ra = d_shares * (delta_nla_ra_d),
    Bm_nlvo_ra = o_shares * (delta_log_nlv_ra_d),
    Bm_nlao_ra = o_shares * (delta_nla_ra_d),
    Bd_nla_ra   = inv_dist_shares * (delta_nla_ra_d),
    Bd_nlv_ra   = inv_dist_shares * (delta_log_nlv_ra_d),
    Bd_avg_nlv_ra = avg_dist_shares * (delta_log_nlv_ra_d),
    Bd_avg_nla_ra = avg_dist_shares * (delta_nla_ra_d),
  ) %>%
  group_by(ipums_id_o, country_name) %>%
  summarise(across(c(starts_with("Bm_"), starts_with("Bd_")), 
                   ~sum(.x, na.rm = TRUE)),
            .groups = 'drop') %>%
  rename(ipums_id = ipums_id_o)

#*******************************************************************************
# Distance shares ----
shares_df <- full_join(dist_df, mig_df) %>%
  filter(ipums_id_d != ipums_id_o) %>%
  group_by(ipums_id_o, country_name) %>%
  summarise(d_shares_sum = sum(d_shares, na.rm = TRUE),
            dist_shares_sum = sum(inv_dist_shares, na.rm = TRUE),
            #avg_dist_shares = mean(avg_dist_shares),
            .groups = 'drop') %>%
  rename(ipums_id = ipums_id_o)

#*******************************************************************************
# Combine ----

Btk_df <- full_join(Btk_pop_df, Btk_empl_df) %>%
  full_join(Btk_nl_df) %>%
  full_join(Btk_i_df) %>%
  full_join(shares_df) %>%
  left_join(id_df %>% select(ipums_id, admin_name, geo_lvl )) %>%
  var_labels(Bm_o = "$\\text{B}_{m}^{\\text{o}}$",
             Bm_ntlao = "$\\text{B}_{m}^{\\text{ntla,o}}$",
             Bm_ntlvo = "$\\text{B}_{m}^{\\text{ntlv,o}}$",
             Bd_avg_ntla = "$\\text{B}_{d}^{\\text{avg,ntla}}$",
             Bd_avg_ntlv = "$\\text{B}_{d}^{\\text{avg,ntlv}}$",
             Bm_ntla = "$\\text{B}_{m}^{\\text{ntla}}$",
             Bd_ntla = "$\\text{B}_{d}^{\\text{ntla}}$",
             Bm_ntlv = "$\\text{B}_{m}^{\\text{ntlv}}$",
             Bd_ntlv = "$\\text{B}_{d}^{\\text{ntlv}}$",
             # Bartiks for change in dest pop / orig baseline pop
             Bm_d = "$\\sum (m_{od}/m_d)\\times\\Delta N_d/N{o0}$",
             Bm_o = "$\\sum (m_{od}/m_o)\\times\\Delta N_d/N{o0}$",
             Bd = "$\\sum (1/d_{od})\\times\\Delta N_d/N{o0}$",
             Bd_avg = "$\\sum (d^{-1}_{od}/\bar{d}^{-1}_{od})\\times\\Delta N_d/N{o0}$",
             # Bartiks for delta log pop at destination
             Bm_d_dlp = "$\\sum (m_{od}/m_d)\\times\\Delta\\log\\text{pop}$",
             Bm_o_dlp = "$\\sum (m_{od}/m_o)\\times\\Delta\\log\\text{pop}$",
             Bd_dlp   = "$\\sum (1/d_{od})\\times\\Delta\\log\\text{pop}",
             Bd_avg_dlp = "$\\sum (d^{-1}_{od}/\bar{d}^{-1}_{od})\\Delta\\log\\text{pop}")

rm(o_pop_df, d_pop_df, d_empl_df, d_nl_df, o_nl_df, Btk_pop_df, Btk_empl_df,
   Btk_nl_df, Btk_i_df)

#table(Btk_empl_df$country_name)
#table(Btk_nl_df$country_name)

# outcomes and weights 
y_df <- full_join(
  fl_df %>% filter(bl_treecover > 0.0001),
  ca_df) %>%
  full_join(
    emig_df %>% select(-starts_with("bl"))
  ) %>%
  full_join(urb_df) %>%
  full_join(pop_df %>% select(ipums_id, bl_pa, bl_pop, bl_rural_pop, 
                              delta_pop_ann, starts_with("age"))) %>%
  full_join(empl_df %>% 
              select(ipums_id, bl_ag, delta_nonag_ann,delta_log_nonag_ann) ) %>%
  select(-c(ends_with("yr"), ends_with("prd_len"))) %>%
  full_join(nl_df %>% select(ipums_id, delta_ntla, delta_log_ntlv)) %>%
  rename_with(~paste0("O_", .x), c(delta_ntla, delta_log_ntlv, delta_nonag_ann,
                                   delta_log_nonag_ann, delta_pop_ann)) %>%
  mutate(
    el_emig_rate = emig_ann / bl_pa, 
    log_area = log(total_area),
    log_urban = log(1+ urban_area),
    O_rural_pop = bl_rural_pop / bl_pop,
    O_ag_pop = bl_ag / bl_pop,
    # Deforestation = forest loss/ total area
    deforestation=avg_loss / total_area,
    deforestation_simple = avg_loss_simple / total_area)

# panel dataset for analysis 
final_df <- left_join(Btk_df, y_df)

# names_df <- panel_df %>% select(country_name) %>% distinct() 
# ctry_names <- t(names_df) %>% as.vector()
# ctry_codes <- g_ctry3(ctry_names)
# temp_df <- panel_df %>% mutate(Country = g_ctry3(country_name))

# temp_df <- panel_df %>% filter(is.na(deforestation) == TRUE) %>%
# select(ipums_id, country_name, admin_name, deforestation, avg_loss, avg_log_loss, total_area)

saveRDS(Btk_df, file.path(out.dir, "R/Bartik.rds"))
write_dta(Btk_df, file.path(out.dir, "Bartik.dta"))

# before saving final_df, now just y
y_df <- left_join(id_df, y_df)
saveRDS(y_df, file.path(out.dir, "R/data_merged.rds"))
write_dta(y_df, file.path(out.dir, "data_merged.dta"))
