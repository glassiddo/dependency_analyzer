#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    October 9, 2025
#* Title:   Merge datasets
#* Desc:    Take the final datasets and merge into a single df
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

# TODO - figure out the migration datasets - what needs to be in

### load dfs ----
id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country, everything())
cols_to_remove <- setdiff(names(id_df), "ipums_id") # admin_name etc

codes_ctries <- id_df %>% 
  transmute(
    country_code = str_sub(ipums_id, 1, 3),
    country
  ) %>% 
  distinct()
# read all the other dfs, and always remove cols_to_remove
# they refer to the ipums_id of destination rather than origin country
# and are unnecessary for the reduced form anyway...

df_list <- list(
  fl1 = readRDS(here(out.dir, "R", "forestloss_cuts.rds")) %>% mutate(wave = "1"),
  fl2 = readRDS(here(out.dir, "R", "forestloss_cuts_period2.rds")) %>% mutate(wave = "2"),
  nl_df = readRDS(here(out.dir, "R", "nightlight.rds")),
  pop_df = readRDS(here(out.dir, "R", "population.rds")),
  empl_df = readRDS(here(out.dir, "R", "employment.rds")),
  shifts = read_dta(here(out.dir, "shifts.dta")) %>% mutate(wave = as.character(wave)),
  emig2_df = readRDS(here(out.dir, "R", "emigration_census2.rds")),
  emig1_df = readRDS(here(out.dir, "R", "emigration_census1.rds")),
  mig2_df = readRDS(here(out.dir, "R", "immigration_census2.rds")),
  mig1_df = readRDS(here(out.dir, "R", "immigration_census1.rds")),
  urb_df = readRDS(here(out.dir, "R", "urban.rds")),
  gaez = read_dta(here(out.dir, "GAEZ.dta")), # TODO implement ken's new GAEZ data
  tmf = readRDS(here(out.dir, "R", "TMF_wide.rds")),
  vcf = readRDS(here(out.dir, "R", "MODIS_VCF_wide.rds")),
  #gaf = read_data(here(out.dir, "GAFTC_wide.dta")),
  #wdpa = read_dta(here(build.dir, "Africa", "wdpa", "ipums_share_pa.dta")),
  crop = read_dta(here(out.dir, "sharecrop_new.dta")) %>% mutate(wave = as.character(wave)),
  geo_ctrls = readRDS(here(out.dir, "R", "geo_controls.rds")) %>% mutate(wave = as.character(wave))
)

df_list <- lapply(df_list, function(df) {
  df %>% select(-any_of(cols_to_remove)) 
})
list2env(df_list, envir = .GlobalEnv)

bartiks <- read_dta(here(out.dir, "new_bartiks.dta")) %>% mutate(wave = as.character(wave))

trade <- read_dta(here(out.dir, "trade.dta")) %>% 
  rename(country = country_code) %>% 
  filter(year == 2000) %>% 
  select(
    country, contains("gdp")
  )

#### merge -----
forestloss <- bind_rows(fl1, fl2)

df <- bartiks %>% 
  full_join(forestloss, by = c("ipums_id", "wave"), suffix = c("", "_remove_fl")) %>% 
  filter(!is.na(fl_end_yr)) %>% # only keep if has forest loss data
  left_join(urb_df %>% 
              select(ipums_id, total_area), # only that is used 
            by = "ipums_id") %>%
  left_join(emig1_df, by = "ipums_id", suffix = c("", "_remove_emig1")) %>%
  left_join(emig2_df, by = "ipums_id", suffix = c("", "_remove_emig2")) %>%
  left_join(mig1_df, by = "ipums_id", suffix = c("", "_remove_immig1")) %>%
  left_join(mig2_df, by = "ipums_id", suffix = c("", "_remove_immig2")) %>%
  left_join(pop_df, by = "ipums_id", suffix = c("", "_remove_pop")) %>%
  left_join(empl_df, by = "ipums_id", suffix = c("", "_remove_emp")) %>%
  left_join(
    shifts %>% 
      rename(ipums_id = ipums_id_d),
    by = c("ipums_id", "wave")
  ) %>% 
  left_join(
    nl_df %>% 
      select(ipums_id, delta_ntlla, delta_log_ntlv),
    by = "ipums_id"
  ) %>%
  left_join(tmf , by = c("ipums_id", "wave")) %>% 
  left_join(vcf , by = c("ipums_id", "wave")) %>%
  #left_join(gaf , by = c("ipums_id", "wave")) %>% 
  left_join(crop, by = c("ipums_id", "wave")) %>% 
  left_join(geo_ctrls, by = c("ipums_id", "wave")) %>% 
  # rename_with( # was done in the old 10 - final_df.R code 
  # but i dont know why its needed and it wasnt impacting data_merged_13
  #   ~paste0("O_", .x), 
  #   c(
  #     delta_ntla, delta_log_ntlv, delta_nonag_ann, 
  #     delta_log_nonag_ann, delta_pop_ann
  #   )
  # ) %>%
  #left_join(wdpa, by = "ipums_id") %>% 
  left_join(gaez, by = "ipums_id") %>% 
  left_join(id_df %>% select(country, country_name) %>% distinct(), by = "country_name") %>% 
  left_join(trade, by = "country") %>% 
  mutate(
    #share_pa = replace_na(share_pa, 0),
    el_iemig_rate = iemig_ann / bl_pop,
    ## the following were done in 10-final_df.R but excluded in 13_merged_data.do
    # el_emig_rate = emig_ann / bl_pop, 
    # log_area = log(total_area),
    # log_urban = log(1+ urban_area),
    # O_rural_pop = bl_rural_pop / bl_pop,
    # O_ag_pop = bl_ag / bl_pop,
    # Deforestation = forest loss/ total area
    # deforestation =avg_loss / total_area,
    # deforestation_simple = avg_loss_simple / total_area,
    ipums_id = str_trim(ipums_id) # probably unnecessary
  ) %>% 
  rename(delta_sharecrop_ann = delta_sharecrop) %>% 
  select(-contains("_remove_")) %>% # and remove the duplicates - need to check
  select(-c(contains("yr"), ends_with("prd_len"))) 

for (cut in seq(10, 90, by = 10)) {
  avg_name <- paste0("avg_loss_t", cut)
  new_name <- paste0("deforestation_t", cut)
  df <- df %>%
    mutate(!!sym(new_name) := .data[[avg_name]] / .data[["total_area"]])
}

rm(df_list, id_df, nl_df, pop_df, urb_df, forestloss, empl_df, fl1, fl2, gaez,
   emig1_df, emig2_df, crop, vcf, mig1_df, mig2_df, shifts, bartiks, tmf, 
   trade, geo_ctrls)

write_dta(df, here(out.dir, "data_merged.dta"))
saveRDS(df, here(out.dir, "R", "data_merged.RDS"))
