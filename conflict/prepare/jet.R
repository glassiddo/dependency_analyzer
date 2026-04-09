source("do/conf_biodiversity_setup.R")

# library(blscrapeR)
# 
# get_cpi_monthly <- function() {
#   
#   cpi_1991 <- bls_api("CUUR0000SA0", startyear = 1991, endyear = 2000)
#   cpi_2001 <- bls_api("CUUR0000SA0", startyear = 2001, endyear = 2010)
#   cpi_2011 <- bls_api("CUUR0000SA0", startyear = 2011, endyear = 2020)
#   
#   cpi_data <- bind_rows(cpi_1991, cpi_2001, cpi_2011) %>%
#     transmute(
#       year  = as.integer(year),
#       month = as.integer(str_remove(period, "M")),
#       cpi = as.numeric(value)
#     ) 
# }
# 
# cpi_month <- get_cpi_monthly()
# 
# saveRDS(cpi_month, here(int_dir, "cpi.rds"))

cpi_month <- readRDS(here(int_dir, "cpi.rds"))

jet <- fread(here(raw_dir, "fuelprices",
                  "jet_fuel_price_daily.csv")) %>% 
  mutate(
    date = as.Date(Day, format = "%m/%d/%Y"),
    month = month(date),
    quarter = quarter(date),
    year = year(date),
    p_nominal = `U.S. Gulf Coast Kerosene-Type Jet Fuel Spot Price FOB  Dollars per Gallon`
  ) %>%
  left_join(cpi_month, by = c("month", "year")) %>% 
  filter(year > 1990, year < 2020) %>% 
  transmute(
    date,
    month = month(date),
    quarter = quarter(date),
    year,
    fuel_p = p_nominal / (cpi / 100),
  ) %>% 
  arrange(date)


fuel_monthly <- jet %>%
  group_by(month, year) %>%
  reframe(
    fuel_p = mean(fuel_p, na.rm = TRUE)
  ) %>%
  arrange(year, month) %>% 
  mutate(
    change_mom = (fuel_p / lag(fuel_p, 1)) - 1,
    change_yoy = (fuel_p / lag(fuel_p, 12)) - 1
  )

fuel_quarterly <- jet %>% 
  mutate(
    qtr_yr = as.yearqtr(paste0(year, quarter), format = "%Y %q")
  ) %>% 
  group_by(qtr_yr, year) %>%
  reframe(
    fuel_p = mean(fuel_p, na.rm = TRUE)
  ) %>% 
  arrange(qtr_yr) %>% 
  mutate(
    baseline_mean = (lag(fuel_p, 4) + lag(fuel_p, 3) + lag(fuel_p, 2)) / 3,
    shock = (fuel_p / baseline_mean) - 1,
    shock_lag1 = lag(shock, 1),
    shock_lag2 = lag(shock, 2)
    # change_qoq = (fuel_p / lag(fuel_p, 1)) - 1,
    # change_q_yoy  = (fuel_p / lag(fuel_p, 4)) - 1,
    # prev_year_mean = # mean of the last 4 quarters
    #   (lag(fuel_p, 4) + lag(fuel_p, 3) + lag(fuel_p, 2) + lag(fuel_p, 1)) / 4,
    # change_vs_prev_year =
    #   (fuel_p / prev_year_mean) - 1
  ) 

fuel_yearly <- jet %>%
  group_by(year) %>%
  reframe(
    fuel_p = mean(fuel_p, na.rm = TRUE)
  ) %>% 
  arrange(year) %>% 
  mutate(
    prev_year_p = lag(fuel_p, 1)
  )

fuel_safari_shock <- jet %>%
  mutate(qtr_yr = as.yearqtr(date)) %>%
  group_by(qtr_yr) %>%
  reframe(fuel_p = mean(fuel_p, na.rm = TRUE)) %>%
  arrange(qtr_yr) %>%
  mutate(
    baseline_1yr = rollmean(lag(fuel_p, 1), k = 4, fill = NA, align = "right"),
    q_shock = (fuel_p / baseline_1yr) - 1
  ) %>%
  # 3. Filter for the peak safari season (Q3) to represent the "Yearly Shock"
  filter(month(as.Date(qtr_yr)) == 7) %>% 
  transmute(
    year = year(as.Date(qtr_yr)),
    safari_fuel_shock = q_shock
  )

f <- jet %>%
  group_by(year) %>%
  summarise(
    # Reference: Average real price of the entire previous year
    annual_p = mean(fuel_p, na.rm = TRUE)
  ) %>%
  mutate(
    prev_year_anchor = lag(annual_p)
  ) %>% 
  left_join(
    fuel_monthly %>% 
      filter(month <= 6) %>% 
      group_by(year) %>% 
      summarise(booking_window_p = mean(fuel_p, na.rm = TRUE)),
    by = "year"
  ) %>% 
  mutate(
    booking_shock = (booking_window_p / prev_year_anchor) - 1
  ) 

f_extended <- jet %>%
  group_by(year) %>%
  reframe(
    annual_p = mean(fuel_p, na.rm = TRUE)
  ) %>%
  mutate(prev_year_p = lag(annual_p)) %>%
  left_join(
    fuel_monthly %>%
      mutate(
        period = case_when(
          month <= 6 ~ "H1",
          TRUE ~ "H2"
          # month <= 3 ~ "Q1",
          # month <= 6 ~ "Q2",
          # month <= 9 ~ "Q3",
          # TRUE ~ "Q4"
        )
      ) %>%
      group_by(year, period) %>%
      summarise(period_p = mean(fuel_p, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = period, values_from = period_p),
    by = "year"
  ) %>% 
  mutate(
    across(
      H1:H2,
      ~ (.x / lag(.x)) - 1,
      .names = "{.col}_shock"
    ),
    across(
      H1:H2,
      ~ (lag(.x, 1)),
      .names = "{.col}_lag"
    ),
    across(
      ends_with("_shock"),
      ~ lag(.x, 1),
      .names = "{.col}_lag"
    ),
    across(
      ends_with("_shock"),
      ~ lead(.x, 1),
      .names = "{.col}_lead"
    )
  ) 

saveRDS(f_extended, here(int_dir, "fuel_prices.rds"))
