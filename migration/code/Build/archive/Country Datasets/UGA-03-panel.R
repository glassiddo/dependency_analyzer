# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Feb 7, 2021
# Title:   Uganda Panel 
# Output:  Support/population.rds  LSMS/panel.rds
# Desc:    Creates panel dataset and population dataset 
###############################################################################

source("code/SSA_env_SetUp.R")

###############################################################################
##### Create panel of location and migration dates
###############################################################################

# dataset of all migration years and beginning and ending location
sample_df <- bind_rows(
  read_dta( paste0(uga.dir, "LSMS/Uganda_2005.dta" )) %>%
    remove_all_labels(),
  read_dta( paste0(uga.dir, "LSMS/Uganda_2009.dta" )) %>%
    remove_all_labels(),
  read_dta( paste0(uga.dir, "LSMS/Uganda_2010.dta" )) %>%
    remove_all_labels(),
  read_dta( paste0(uga.dir, "LSMS/Uganda_2011.dta" )) %>%
    remove_all_labels(),
  read_dta( paste0(uga.dir, "LSMS/Uganda_2013.dta" )) %>%
    remove_all_labels() ) %>%
  rename(pid = PID, wgt = p_wgt, surv_year = year) 

panel_df <- sample_df %>%
  mutate(
    # if missing interview year, use survey year
    irw_year = ifelse(is.na(irw_year) == TRUE, surv_year, irw_year),
    migration_year = ifelse(duration != 100, irw_year - duration, NA),
    ever_migrate = ifelse(duration != 100, 1, 0),
    ever_migrate_dist = ifelse(district != birth_district, 1, 0),
    move_reason = str_to_title(move_reason),
    move_reason = str_trim(move_reason, side = 'both'),
    move_reason = ifelse(move_reason == "Other (Specify)", "Other", move_reason),
    move_reason = ifelse(substring(move_reason, 1, 4) == "Drou", 
                         "Drought, Flood Or Other Weather Condition", move_reason),
    move_reason = ifelse(move_reason %in% c("Divorce", "Follow/Join Family",
                                            "Marriage"),
                         "Family Reasons", move_reason) ) %>%
  filter(age >= 15 & age <= 65) %>%
  select(region, region_name, district, district_name, prev_region, 
         prev_region_name, prev_district, prev_district_name, irw_year, age,               
         duration, female, birth_district, urban, surv_year, move_reason, wgt,
         pid, migration_year, ever_migrate, ever_migrate_dist) %>%
  drop_na(wgt) %>%
  filter(duration != 0)

saveRDS( panel_df, paste0(uga.dir, "LSMS/panel.rds"))


###############################################################################
##### Prime aged population and implied growth rate
###############################################################################

gr_df <- read_excel(paste0(uga_raw.dir, "Census_populations_2014.xlsx"),
                    range = "A1:F140") %>%
  filter(Status != "Region") 

names(gr_df) <- c("district_name", "type", "pop_91", "pop_02", "pop_14", "pop_20")

gr_df %<>%
  mutate(
    paren_pos = str_locate(district_name, "←"),
    district_name = ifelse(is.na(paren_pos) == FALSE, 
                           substring(district_name, 1, paren_pos -3), district_name),
    district_name = ifelse(substr(district_name, 1, 6) == "Luwero", "Luwero", district_name),
    district_name = ifelse(substr(district_name, 1, 10) ==  "Ssembabule", "Ssembabule", district_name),
    # get the implied growth rate using 2020 projection
    growth_rate = (pop_20 / pop_14)^(1/6) - 1) %>%
  select(district_name, growth_rate)

# prime aged population in 2014 from census
pop_df <- read_dta(paste0(uga.dir, "Census/Census.dta") ) %>%
  filter(age >= 15 & age <= 65) %>%
  filter(year == 2014) %>%
  mutate(d_name = ifelse(d_name == "sembabule", "ssembabule", d_name),
         district_name = str_to_sentence(d_name)) %>%
  group_by(district_name) %>%
  summarise(pop = sum(perwt, na.rm = TRUE),
            urban = round(weighted.mean(urban, w = perwt)),
            .groups = 'drop') %>%
  left_join(gr_df) %>%
  mutate(pop_00 = pop / (1 + growth_rate)^14) %>%
  select(-pop) %>%
  full_join(sample_df %>% select(district, district_name) %>% unique() )


saveRDS( pop_df, paste0(uga.dir, "Support/population.rds"))

###############################################################################
##### Appendix urban status from surveys
###############################################################################
# # rural-urban status
# urban_df <- sample_df %>%
#   group_by(district, surv_year) %>%
#   summarise(pop = sum(wgt),
#             urban = sum(urban * wgt), .groups = 'drop') %>%
#   mutate(urban = round(urban/ pop),
#          surv_year = surv_year - 2000) %>%
#   select(-pop) %>%
#   pivot_wider(names_from = surv_year,
#               names_glue = "urban_{surv_year}",
#               values_from = urban) %>%
#   rename(urban_05 = urban_5, urban_09 = urban_9) %>%
#   mutate(urban_13 = ifelse(is.na(urban_13) == TRUE, urban_11, urban_13),
#          urban_11 = ifelse(is.na(urban_11) == TRUE, urban_13, urban_11),
#          urban_10 = ifelse(is.na(urban_10) == TRUE, urban_11, urban_10),
#          urban_09 = ifelse(is.na(urban_09) == TRUE, urban_10, urban_09),
#          urban_05 = ifelse(is.na(urban_05) == TRUE, urban_09, urban_05)) %>%
#   rename(urban = urban_13)
# 
# pop_df <- full_join(pop_df, urban_df)

