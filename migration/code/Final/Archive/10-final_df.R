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
  select(-c(admin_name_l2:Country))
nl_df <- readRDS(file.path(out.dir, "R/nightlight.rds"))
pop_df <- readRDS(file.path(out.dir, "R/population.rds"))
empl_df <- readRDS(file.path(out.dir, "R/employment.rds"))
ca_df <- readRDS(file.path(out.dir, "R/croparea.rds")) %>%
  select(-c(admin_name_l2:region_name))
emig_df <- readRDS(file.path(out.dir, "R/emigration_census2.rds")) %>%
  select(-c(admin_name_l2:Country))

mig_df <- readRDS(file.path(out.dir, "R/full_migration_census1.rds"))
dist_df <- readRDS(file.path(out.dir, "R/full_distance_census1.rds"))
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


# outcomes and weights ---- 
y_df <- full_join(
  fl_df %>% filter(bl_treecover > 0.0001),
  ca_df) %>%
  full_join(
    emig_df)  %>%
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
    el_iemig_rate = iemig_ann / bl_pa,
    log_area = log(total_area),
    log_urban = log(1+ urban_area),
    O_rural_pop = bl_rural_pop / bl_pop,
    O_ag_pop = bl_ag / bl_pop,
    # Deforestation = forest loss/ total area
    deforestation=avg_loss / total_area,
    deforestation_simple = avg_loss_simple / total_area)


# names_df <- panel_df %>% select(country_name) %>% distinct() 
# ctry_names <- t(names_df) %>% as.vector()
# ctry_codes <- g_ctry3(ctry_names)
# temp_df <- panel_df %>% mutate(Country = g_ctry3(country_name))

# temp_df <- panel_df %>% filter(is.na(deforestation) == TRUE) %>%
# select(ipums_id, country_name, admin_name, deforestation, avg_loss, avg_log_loss, total_area)

# before saving final_df, now just y
y_df <- left_join(id_df, y_df)
saveRDS(y_df, file.path(out.dir, "R/data_merged.rds"))
write_dta(y_df, file.path(out.dir, "data_merged.dta"))
