#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    March 5, 2025
#* Title:   MODIS VCF
#* Desc:    
#* Note:    Converted code from stata and updated file in GEE w/ new IPUMS units
#*******************************************************************************
#*

source("code/SSA_env_SetUp.R")

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country)

# data_vcf <- read_csv(here(
#   raw.dir, "Africa", "MODIS_VCF", "sampleUnitsVCF.csv")
#   ) %>% 
#   ## values are different from the old version as it uses scale of 250
#   ## rather than 500. the actual resolution is 250
#   pivot_longer(
#     cols = matches("vcfa\\d{4}"),
#     names_to = "year",
#     names_pattern = "vcfa(\\d{4})",
#     values_to = "vcfa"
#   ) %>%
#   mutate(year = as.integer(year)) %>% 
#   select(ipums_id, year, vcfa)

data_modisvcf <- read_csv(here(
  raw.dir, "Africa", "MODIS_VCF", "sampleUnitsMODISVCF.csv")
) %>% 
  ## values are different from the old version as it uses scale of 250
  ## rather than 500. the actual resolution is 250
  pivot_longer(
    cols = matches("treecover_(sum|count)\\d{4}"),
    names_to = c("variable", "year"),
    names_pattern = "treecover_(sum|count)(\\d{4})",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  mutate(year = as.integer(year)) %>% 
  mutate(
    modis_vcf = sum / 100, 
    modis_vcf_avg = modis_vcf / count
  ) 

vcf_waves <- list()

for (w in 1:2) {
  
  # get the fl start and end years
  fl_yrs <- readRDS(here(
    build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", w, ".rds")
  )) 

  forestloss_yrs <- id_df %>% 
    left_join(fl_yrs, by = "country")
  
  summary_vcf <- inner_join(data_modisvcf, forestloss_yrs, by = "ipums_id") %>% 
    #left_join(data_vcf, by = c("year", "ipums_id")) %>%
    filter(year == fl_start_yr | year == fl_end_yr) %>%
    arrange(ipums_id, year) %>% 
    mutate(
      delta_modis_vcf = ifelse(
        year == fl_end_yr, 
        (modis_vcf - lag(modis_vcf)) / 
          (fl_end_yr - fl_start_yr), 
        NA),
      delta_modis_vcf_avg = ifelse(
        year == fl_end_yr,
        (modis_vcf_avg - lag(modis_vcf_avg)) /
          (fl_end_yr - fl_start_yr),
        NA),
      # Log delta calculations
      delta_log_modis_vcf = ifelse(
        year == fl_end_yr,
        log((modis_vcf / lag(modis_vcf)) ^
              (1 / (fl_end_yr - fl_start_yr))),
        NA)
      ## new version
      # delta_vcfa = ifelse(
      #   year == fl_end_yr, 
      #   (vcfa - lag(vcfa)) / 
      #     (fl_end_yr - fl_start_yr), 
      #   NA),
      # delta_log_vcfa = ifelse(
      #   year == fl_end_yr,
      #   log((vcfa / lag(vcfa)) ^
      #         (1 / (fl_end_yr - fl_start_yr))),
      #   NA)
    ) %>% 
    filter(year == fl_end_yr) %>% 
    select(ipums_id, fl_start_yr, fl_end_yr, modis_vcf, modis_vcf_avg,
           delta_modis_vcf, delta_log_modis_vcf, delta_modis_vcf_avg,
           #delta_vcfa, delta_log_vcfa
           )
  
  vcf_waves[[w]] <- summary_vcf
  
}

vcf_wide <- bind_rows(vcf_waves, .id = "wave")

saveRDS(vcf_wide, here(out.dir, "R", "MODIS_VCF_wide.rds"))
write_dta(vcf_wide, here(out.dir, "MODIS_VCF_wide.dta"))
