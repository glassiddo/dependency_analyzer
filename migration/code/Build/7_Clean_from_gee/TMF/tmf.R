#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    November 5, 2025
#* Title:   Tropical moist forests
#* Desc:    Processes tropical moist forest data for both waves
#* Note:    Converted code from stata and updated file in GEE w/ new IPUMS units
#*******************************************************************************
#*

source("code/SSA_env_SetUp.R")

mpiotmf <- read_csv(here(
  raw.dir, "Africa", "Tropical Moist Forests", "MPIOTMF.csv")
)

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country)

long_tmf <- mpiotmf %>%
  pivot_longer(
    cols = starts_with(c("intact", "defor", "degra", "regro")),
    names_to = c(".value", "year"),
    names_pattern = "([a-zA-Z]+)(\\d{4})", 
    # split "intact2000" to "intact" and "2000"
    names_transform = list(year = as.integer)
  )

tmf_waves <- list()

for (w in 1:2) {
  
  # get the fl start and end years
  fl_yrs <- readRDS(here(
    build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", w, ".rds")
  )) 

  forestloss_yrs <- id_df %>% 
    left_join(fl_yrs, by = "country")
  
  summary_tmf <- inner_join(long_tmf, forestloss_yrs, by = "ipums_id") %>% 
    group_by(ipums_id) %>%
    mutate(intact_start = intact[year == fl_start_yr[1]]) %>%
    filter(year > fl_start_yr & year <= fl_end_yr) %>%
    summarise(
      intact_start = first(intact_start), 
      years = n(),
      defor = sum(defor, na.rm = TRUE),
      degra = sum(degra, na.rm = TRUE),
      regro = sum(regro, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    mutate( # replace 0s with NAs
      across(
        .cols = c(defor, degra, regro),
        .fns = ~ if_else(intact_start == 0, NA_real_, .x / years)
      )
    )
  tmf_waves[[w]] <- summary_tmf
}

tmf_wide <- bind_rows(tmf_waves, .id = "wave")

saveRDS(tmf_wide, here(out.dir, "R", "TMF_wide.rds"))
write_dta(tmf_wide, here(out.dir, "TMF_wide.dta"))
