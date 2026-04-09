#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    Oct 7, 2023
#* Title:   Mine Instrument
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

price_df <- readRDS(here(build.dir, "Africa", "Mines", "commodity_price_ts.rds"))

# AR1 prices 
p1_df <- price_df %>% select(year, ends_with('nom')) %>%
  # prices in different units, make relative to some base year 1990
  mutate(across(!year, ~ .x/.x[year == 1990]))

# make average price in each year
p_bar <- p1_df %>%
  pivot_longer(! year,
    names_to = 'commod',
    values_to = 'p'
  ) %>%
  group_by(year) %>%
  summarise(P_bar = mean(p, na.rm = TRUE)) 

ar_df <- full_join(p1_df, p_bar) %>%
  mutate(across(!year, ~ifelse(is.na(.x) == FALSE,log(.x), NA)))


p_list <- names(ar_df %>% select(-year, -P_bar))

resid_df <- ar_df %>% select(year) 

# calculate AR1 residual for each price
for (p in p_list) {
  p_sym <- sym(p)
  est_df <- ar_df %>%
    mutate(lag_p = lag({{p_sym}}, order_by = year )) %>%
    select(year, {{p_sym}}, lag_p, P_bar) %>%
    drop_na()
  mod <- paste0(p, '~ lag_p + P_bar')
  
  m1 <- lm(mod, data = est_df, na.action = 'na.omit')
  
  m0 <- augment(m1)
  
  out_df <- bind_cols(est_df %>% select(year), {{p}} := m0$.resid )
  resid_df %<>% full_join(out_df)
    
}

resid_df %<>% rename_with(~str_replace(.x,'nom','resid'), ends_with('nom'))

saveRDS(resid_df, here(build.dir, "Africa", "Mines", "commodity_price_resid.rds"))
