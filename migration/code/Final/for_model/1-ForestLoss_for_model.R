#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    January, 13, 2026
#* Title:   Forest Loss
#* Desc:    Clean forest loss file for the model  
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

loss_long <- readRDS(here(build.dir, "Africa", "Forest", "loss_long.rds"))
cover_full <- readRDS(here(build.dir, "Africa", "Forest", "worldwide_fcover.rds"))

model_df <- loss_long %>%
  full_join(
    cover_full %>%
      pivot_longer(
        cols = matches("^f.*\\d{2}$"),
        names_to = "treecover_var",
        values_to = "treecover"
      ) %>%
      mutate(threshold = str_extract(treecover_var, "\\d{2}$")) %>%
      select(ipums_id, threshold, treecover),
    by = c("ipums_id", "threshold")
  ) %>%
  arrange(ipums_id, threshold, year) %>%
  group_by(ipums_id, threshold) %>%
  mutate(
    cumulative_loss = cumsum(replace_na(loss, 0)),
    treecover_new = treecover - cumulative_loss
  ) %>%
  ungroup() %>%
  select(ipums_id, year, threshold, treecover, treecover_new) %>%
  pivot_wider(
    names_from = threshold,
    values_from = c(treecover, treecover_new),
    names_glue = "{.value}_{threshold}"
  ) %>%
  arrange(ipums_id, year)

write_dta(model_df, here(out.dir, "forestloss_cuts_long_with_row.dta"))