#* Project: Migration Africa
#* Author:  Sam Marshall (edited by Iddo Glass)
#* Date:    Oct 7, 2023 (edited in Jan 27, 2026)
#* Title:   Get residuals of prices
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

# load the price df 
p1_df <- readRDS(here(build.dir, "Africa", "Mines", "commodity_price_ts.rds")) %>% 
  # only keep nominal prices
  select(year, ends_with('nom')) %>% 
  # ignore the 2c and 3t as they're averages of other minerals
  select(-contains("3t"), -contains("2c")) %>% 
  # then make prices relative to base year 1990
  mutate(across(!year, ~ .x/.x[year == 1990]))

# get average price of all commodities in each year (non weighted, dropping NAs)
p_bar <- p1_df %>%
  pivot_longer(! year,
               names_to = 'commod',
               values_to = 'p'
  ) %>%
  group_by(year) %>%
  reframe(P_bar = mean(p, na.rm = TRUE)) 

# join the prices and the mean and turn every non-NA value into log
ar_df <- full_join(p1_df, p_bar, by = "year") %>%
  mutate(across(!year, ~ifelse(is.na(.x) == FALSE,log(.x), NA)))

# get all the mineral-price list (e.g., "P_gold_nom", "P_copper_nom" etc.)
p_list <- names(ar_df %>% select(-year, -P_bar))

# create columns of lags for all the mineral-prices (e.g., "P_gold_nom_lag")
ar_df_lag <- ar_df %>%
  arrange(year) %>%
  mutate(across(
    all_of(p_list),
    ~ lag(.x),
    .names = "lag_{.col}"
  ))

# initiate an empty list with the mineral-price names (for residuals)
resid_list <- vector("list", length(p_list))
names(resid_list) <- p_list
# and an empty data with all years (for joining with the residuals)
resid_df <- tibble(year = ar_df_lag$year)

# for each of the price-minerals, compute the following regression 
# price ~ lag (price) + mean_price_all_commodities
# then get the residuals and assign to resid_df for each year
for (p in p_list) {
  
  lag_p <- paste0("lag_", p)
  
  est_df <- ar_df_lag %>%
    transmute(
      year,
      y     = .data[[p]],
      lag_y = .data[[lag_p]],
      P_bar
    )
  
  ok <- complete.cases(est_df)
  
  m1 <- feols(y ~ lag_y + P_bar, data = est_df[ok, ])
  
  # assign residuals back by year index
  resid_df[[p]] <- NA_real_
  resid_df[[p]][ok] <- resid(m1)
}

# name with _resid
resid_df <- resid_df %>%
  rename_with(~ str_replace(.x, "nom", "resid"), ends_with("nom"))

saveRDS(resid_df, here(build.dir, "Africa", "Mines", "commodity_price_resid.rds"))