#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    January, 13, 2026
#* Title:   Forest Loss
#* Desc:    Clean GEE files and create long forest loss df, including for each cut threshold  
#* 1 hectare = 10,000 sq m = 1x10^4 sq m
#*         1 km^2 = 100 hectare = 1x10^6 sq m
#*******************************************************************************
#*
# Set Up ----
source("code/SSA_env_SetUp.R")

# read the sample unit files (from GEE) ---
af_cover <- read_csv(here(
  raw.dir,"Africa","Forest", "sampleUnitsTreeCover.csv"))
af_loss <- read_csv(here(
  raw.dir,"Africa","Forest", "sampleUnitsTreeCoverLoss.csv"))

# read global datasets for ROW calculation ---
world_cover <- read_csv(here(
  raw.dir, "World", "GEE", "worldUnitsTreeCover.csv"))
world_loss  <- read_csv(here(
  raw.dir, "World", "GEE", "worldUnitsTreeCoverLoss.csv"))

af_countries <- unique(af_cover$country)

# mark whatever's not in africa
row_cover <- world_cover %>%
  filter(ISO3 %in% af_countries) %>%
  summarise(across(contains("area"), sum, na.rm = TRUE)) %>%
  mutate(country = "Rest of the World",
         admin_name = "ROW",
         ipums_id = "1")

row_loss <- world_loss %>%
  filter(ISO3 %in% af_countries) %>%
  summarise(across(contains("area") | contains("loss"), sum, na.rm = TRUE)) %>%
  mutate(country = "Rest of the World",
         admin_name = "ROW",
         ipums_id = "1")

cover_full <- bind_rows(af_cover, row_cover)
loss_full  <- bind_rows(af_loss, row_loss)

saveRDS(cover_full, here(build.dir, "Africa", "Forest", "worldwide_fcover.rds"))

### add 0 loss in 2000 
thresholds <- sprintf("%02d", seq(0, 90, by = 10))  # "00", "10", ..., "90"

for (t in thresholds) {
  new_col <- paste0("fcloss", t, "2000")
  loss_full[[new_col]] <- 0
}

## conver into long
loss_long <- loss_full %>%
  select(ipums_id, starts_with("fc")) %>%
  pivot_longer(
    cols = contains("loss"),
    names_to = "measure",
    values_to = "loss"
  ) %>%
  # measure is then defined as e.g., fcloss002003
  # so year is the last four digits
  # and then two before that are the threshold
  mutate(
    year = as.numeric(str_sub(measure, -4)),
    threshold = str_sub(measure, -6, -5)
  ) %>%
  select(-measure)

saveRDS(loss_long, here(build.dir, "Africa", "Forest", "loss_long.rds"))

# save a file for each cut threshold - used for reduced form

cuts <- unique(loss_long$threshold)

for (t in cuts) {
  tc_cut <- loss_long %>% 
    filter(threshold == t, year != "2000") %>%
    full_join(
      cover_full %>% 
        select(ipums_id, treecover = matches(paste0("^f.*", t, "$"))),
      by = "ipums_id"
      )
  
  saveRDS(tc_cut, here(
    build.dir, "Africa", "Forest", "Cuts", paste0("loss",t ,"cut.rds")
    ))
#   write_dta(tc_cut, here( 
#   # not actually necessary to save in both formats as this isn't used in any stata code
#     build.dir, "Africa", "Forest", "Cuts", paste0("loss",t ,"cut.dta")
#     ))
}