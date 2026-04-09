#* Project: Migration Africa
#* Author:  Sam Marshall (edited by Iddo Glass)
#* Date:    Oct 7, 2023 (edited on Jan 29 2026)
#* Title:   Mine Instrument
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

#*******************************************************************************
# read data ----
#*******************************************************************************
mines_dir <- here(build.dir, "Africa", "Mines")

mines_df <- readRDS(here(mines_dir, "sample_mines.rds")) 
price_df <- readRDS(here(build.dir, "Africa", "Mines", "commodity_price_long.rds"))
resid_df <- readRDS(here(mines_dir, "commodity_price_resid.rds"))

asm_df <- readRDS(here(build.dir, "Africa", "Artisinal", "highest_prob_asm.rds"))

prob_asm_factor <- 1 # multiple the prob_asm [0,1] by this to get 'num of asm'
prob_asm_min <- 0.5 # minimal probability to be counted as unit with asm

clean_commodies_prices <- function(df, ctry_commodities, residual = F) {
  ## can be removed if ar1 is removed - it's already done for the regular prices
  # set it to long, the idea is to get price/residuals by year per commodity
  # while filtering only to commodities that exist in the country
  df_long <- df %>%
    pivot_longer(
      cols = -year,
      names_to = "var",
      values_to = if (residual) {"AR1_residual"} else{"price"}
    ) %>%
    mutate(
      commodity = if (residual) {
        var %>% 
          str_remove("^P_") %>% 
          str_remove("_resid$") %>% 
          str_to_title()
      } else {
        var %>%
          str_remove("^P_") %>%
          str_remove("_(nom|real)$") %>%
          str_to_title()
      },
      
      primary_commodity = case_when(
        commodity == "Iron" ~ "Iron Ore",
        commodity == "U308" ~ "U3O8",
        commodity == "Heavymineralsands" ~ "Heavy Mineral Sands",
        TRUE ~ commodity
      )
    )
  
  if (!residual) {
    df_long <- df_long %>%
      mutate(type = ifelse(str_detect(var, "_nom$"), "Nominal", "Real"))
  }
  
  df_long <- df_long %>%
    filter(primary_commodity %in% ctry_commodities)
  
  return(df_long)
}

f_mines <- function(ctry_str, wave) {
  print("Processing:")
  print(ctry_str)
  
  fl_yrs <- readRDS(here(
    build.dir, "Africa", "Support", paste0("fl_nl_yrs_w", wave, ".rds")
  )) %>% 
    filter(country == ctry_str) 
  
  # get forest loss years
  yr_start <- fl_yrs %>% pull(fl_start_yr)
  yr_end <- fl_yrs %>% pull(fl_end_yr)
  
  if (is.na(yr_start) | is.na(yr_end)) {
    warning(paste("Skipping", ctry_str, "due to missing years (one census country)"))
    return(NULL) 
  }
  
  # get mines in country
  ctry_mine_df <- mines_df %>% 
    filter(country == ctry_str) %>%
    select(country, ipums_id, primary_commodity)
  
  ctry_asm_df <- asm_df %>% 
    filter(country == ctry_str) %>% 
    filter(prob_asm > prob_asm_min)
  
  all_ctry_mines <- ctry_mine_df %>% 
    bind_rows(ctry_asm_df) 
  
  ctry_commodities <- all_ctry_mines %>% 
    select(primary_commodity) %>% distinct() %>% pull()
  
  price_ctry <- price_df %>% 
    filter(primary_commodity %in% ctry_commodities)
  
  end_years <- price_ctry %>%
    filter(year <= yr_end, !is.na(price)) %>%
    group_by(primary_commodity) %>%
    reframe(end_year = max(year)) 

  g_price <- price_ctry %>%
    inner_join(end_years, by = "primary_commodity") %>%
    rowwise() %>% 
    filter(year %in% c(yr_start, end_year)) %>% 
    arrange(primary_commodity, year) %>%
    group_by(primary_commodity) %>%
    reframe(
      p_len = last(year) - first(year),
      gr_p_mine = (log(last(price)) - log(first(price))) / p_len
    )

  resid_long <- clean_commodies_prices(resid_df, ctry_commodities, residual = T)

  # average residual from the AR1 process
  #calculate change in mineral prices during that period
  AR1_df <- resid_long %>%
    inner_join(end_years, by = "primary_commodity") %>%
    rowwise() %>%
    filter(year %in% c(yr_start:end_year)) %>%
    group_by(primary_commodity) %>%
    summarise(AR1_residual = mean(AR1_residual, na.rm = TRUE))
  
  mines_with_p <- all_ctry_mines %>% 
    left_join(g_price, by = "primary_commodity") %>% 
    left_join(AR1_df, by = "primary_commodity") %>%
    mutate(
      n_mines = ifelse(is.na(prob_asm), 1, 0),
      n_mines_asm = ifelse(is.na(prob_asm), 1, prob_asm_factor * prob_asm)
    )
  
  price_changes_no_asm <- mines_with_p %>% 
    filter(is.na(prob_asm)) %>% 
    group_by(country, ipums_id) %>%
    reframe(
      N_mines = sum(n_mines),
      across(
        c(gr_p_mine, AR1_residual), 
        ~mean(.x, na.rm = TRUE))
    )

  price_changes_w_asm <- mines_with_p %>% 
    rename(
      gr_p_mine_asm = gr_p_mine
    ) %>%
    group_by(country, ipums_id) %>%
    reframe( # need to calculate weighted average cause N_mines_asm isn't always 1
      N_mines_asm = sum(n_mines_asm),
      across(
        c(gr_p_mine_asm), 
        ~weighted.mean(.x, w = n_mines_asm, na.rm = TRUE)
        )
    ) 
  
  out_df <- price_changes_no_asm %>% 
    full_join(price_changes_w_asm, by = c("ipums_id", "country")) %>% 
    mutate(wave = wave)
  
  return(out_df)
}

#*******************************************************************************
# Run ----
#*******************************************************************************
# for all countries-wave combinations; for single census countries, just wave 2
countries_wave <- census_info %>% 
  select(country) %>% 
  crossing(wave = c(1,2)) 

inst_df <- countries_wave %>% 
  mutate(
    data = map2(country, wave, f_mines)
  ) %>%
  select(data) %>%
  unnest(data) 

id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% 
  select(ipums_id, country) %>% 
  crossing(wave = c(1,2))

inst_df_for_save <- inst_df %>%
  full_join(id_df, by = c("ipums_id", "wave", "country")) %>% 
  inner_join(countries_wave, by = c("country", "wave")) %>% 
  mutate(
    # fill NAs with zeros
    N_mines = ifelse(is.na(N_mines), 0, N_mines),
    N_mines_asm = ifelse(is.na(N_mines_asm), 0, N_mines_asm),
    # create binary
    has_mine = ifelse(N_mines > 0, 1, 0),
    has_mine_asm = ifelse(N_mines_asm > 0, 1, 0),
    # create log(x+1)
    log_mines = log(N_mines+1),
    log_mines_asm = log(N_mines_asm+1)
    ) %>%
  var_labels(
    # gr_p_mine_n = "weighted average of nominal price change of mines",
    gr_p_mine = "weighted average of real price change of mines",
    # gr_p_mine_asm_n = "weighted average of nominal price change of mines inc. ASM",
    gr_p_mine_asm = "weighted average of real price change of mines inc. ASM",
    N_mines = "Num mines in admin unit",
    N_mines_asm = "Num mines in admin unit including ASM",
    log_mines = "log (num mines + 1)",
    log_mines_asm = "log (num mines including ASM + 1)",
    has_mine = "has SNL mine",
    has_mine_asm = "has SNL mine or ASM"
  )

saveRDS(inst_df_for_save, here(out.dir, "R", "mine_instrument.rds"))
write_dta(inst_df_for_save, here(out.dir, "mine_instrument.dta"))

units_with_mines <- inst_df_for_save %>% 
  group_by(ipums_id) %>% 
  reframe(
    has_mine_asm = max(has_mine_asm, na.rm = T),
    log_mines_asm = max(log_mines_asm, na.rm = T)
  )  

write_dta(units_with_mines, here(out.dir, "mines_loc.dta"))

