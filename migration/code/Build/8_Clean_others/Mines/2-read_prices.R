#* Project: Migration Africa
#* Author:  Sam Marshall (edited by Iddo Glass)
#* Date:    Oct 7, 2023 (edited in Jan 27, 2026)
#* Title:   Read price data
#* Desc:    Read price data for all commodities
#* NB: coal is also in the World bank pink sheets data column F from 1970
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

price_dir <- here(raw.dir, "Africa", "Mines", "Prices")

read_price <- function(fl, commod) {
  p_nom <- sym(paste0("P_", commod, "_nom"))
  p_real <- sym(paste0("P_", commod, "_real"))
  out_df <- read_xlsx(
    here(price_dir, fl),
    skip = 4) %>%
    select(Year, starts_with("Unit")) %>%
    rename_with(~str_squish(.x)) %>%
    mutate(across(starts_with("Unit"), ~ifelse(.x == "NA", NA, .x)),
           across(starts_with("Unit"), ~as.numeric(.x ))) %>%
    rename(year = Year,
           {{p_nom}} := "Unit value ($/t)",
           {{p_real}} := "Unit value (98$/t)") %>%
    drop_na({{p_real}}) %>%
    mutate(year = as.numeric(year)) %>% 
    group_by(year) %>% # if there is more than a single value per year, average
    # relevant for nickel with diff values in 2019
    # keep the same col name
    reframe(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "{.col}"))
}

#*******************************************************************************
# read data ----
#*******************************************************************************

mines_sf <- readRDS(here(build.dir, "Africa", "Mines", "mines_sf.rds"))

commod_df <- readRDS(here(build.dir, "Africa", "Mines", "commodity_counts.rds"))

p_nom <- sym(paste0("P_", "nickel", "_nom"))
p_real <- sym(paste0("P_", "nickel", "_real"))

## FRED ---- 

coal_df <- read_xls(
  here(price_dir, "PCOALAUUSDA.xls"),
  #col_types = "text",
  skip = 11) %>%
  mutate(year = as.numeric(str_sub(observation_date, 1, 4)),
         P_coal_real = PCOALAUUSDA / PCU21212121 * 100) %>%
  rename(P_coal_nom = PCOALAUUSDA) %>%
  select(year, P_coal_nom, P_coal_real) %>%
  drop_na(P_coal_nom)

u308_df <- read_xls(
  here(price_dir, "PURANUSDA.xls"),
  #col_types = "text",
  skip = 12) %>%
  mutate(year = as.numeric(str_sub(observation_date, 1, 4)),
         P_u308_real = PURANUSDA / WPU10 * 100,
         P_u308_real1 = PURANUSDA / PCU2122902122901 * 100,
         rat = P_u308_real/ P_u308_real1) %>%
  rename(P_u308_nom = PURANUSDA) %>%
  select(year, P_u308_nom, P_u308_real) %>%
  drop_na(P_u308_nom)

## USGS ----
price_df <- full_join(
  read_price("ds140-gold-2022.xlsx", "gold"),
  read_price("ds140-diamond-2021.xlsx", "diamonds")
) %>%
  full_join( read_price("ds140-copper-2020.xlsx", "copper") ) %>%
  full_join( read_price("ds140-platinum-2022.xlsx", "platinum") ) %>%
  full_join( read_price("ds140-iron_ore-2021.xlsx", "iron") ) %>%
  # nickel has for some reason two values in 2019 
  # seems like a mistake in the original df
  full_join( read_price("ds140-nickel-2019.xlsx", "nickel") ) %>%
  full_join( read_price("ds140-chromium-2022.xlsx", "chromite") ) %>%
  full_join( read_price("ds140-manganese-2022.xlsx", "manganese") ) %>%
  full_join( read_price("ds140-graphite-2022.xlsx", "graphite") ) %>%
  full_join( read_price("ds140-bauxite-alumina-2021.xlsx", "bauxite") ) %>%
  full_join( read_price("ds140-phosphate-2022.xlsx", "phosphate") ) %>%
  full_join( read_price("ds140-lithium-2021.xlsx", "lithium") ) %>%
  full_join( read_price("ds140-tin-2021.xlsx", "tin") ) %>%
  full_join( read_price("ds140-zinc-2022.xlsx", "zinc") ) %>%
  full_join( read_price("ds140-lead-2021.xlsx", "lead") ) %>%
  full_join( read_price("ds140-potash-2022.xlsx", "potash") ) %>%
  full_join( read_price("ds140-tantalum-2022.xlsx", "tantalum") ) %>%
  full_join( read_price("ds140-vanadium-2022.xlsx", "vanadium") ) %>%
  full_join( read_price("ds140-cobalt-2021.xlsx", "cobalt") ) %>%
  full_join( read_price("ds140-tungsten-2019.xlsx", "tungsten") ) %>%
  full_join( read_price("ds140-niobium-2021.xlsx", "niobium") ) %>%
  full_join( read_price("ds140-silver-2021.xlsx", "silver") ) %>%
  full_join( read_price("ds140-molybdenum-2022.xlsx", "molybdenum") ) %>%
  full_join( read_price("ds140-titanium-metal-2020.xlsx", "ilmenite") ) %>%
  full_join( read_price("ds140-rare-earths-2020.xlsx", "lanthanides") ) %>%
  full_join( read_price("ds140-zirconium-2021.xlsx", "heavymineralsands") ) %>% 
  full_join(coal_df) %>%
  full_join(u308_df) 

### add averages for 3t and 2c, for artisanal mining
## from Rigterink et al., 2024 - 
## "For 3t and 2c minerals, price is the simple average of prices 
## of tin, tantalum and tungsten, and copper and cobalt respectively."
## 3t - tin, tantalum and tungsten
## 2c - copper and cobalt

price_df <- price_df %>% 
  mutate(
    # 3t - tin, tantalum, tungsten
    P_3t_nom  = rowMeans(across(c(P_tin_nom, P_tantalum_nom, P_tungsten_nom)),
                         na.rm = TRUE),
    P_3t_real = rowMeans(across(c(P_tin_real, P_tantalum_real, P_tungsten_real)),
                         na.rm = TRUE),
    # 2c - copper and cobalt ---
    P_2c_nom  = rowMeans(across(c(P_copper_nom, P_cobalt_nom)),
                         na.rm = TRUE),
    P_2c_real = rowMeans(across(c(P_copper_real, P_cobalt_real)),
                         na.rm = TRUE)
    )

saveRDS(price_df, here(build.dir, "Africa", "Mines", "commodity_price_ts.rds"))

### get a long version that could be used directly in the file with the mines
### the above version is needed if we use residual (ar1) as well

price_long <- price_df %>%
  pivot_longer(
    cols = -year,
    names_to = "var",
    values_to = "price"
  ) %>%
  mutate(
    commodity = var %>%
      str_remove("^P_") %>%
      str_remove("_(nom|real)$") %>%
      str_to_title(),
    primary_commodity = case_when(
      commodity == "Iron" ~ "Iron Ore",
      commodity == "U308" ~ "U3O8",
      commodity == "Heavymineralsands" ~ "Heavy Mineral Sands",
      TRUE ~ commodity
    ),
    type = ifelse(str_detect(var, "_nom$"), "Nominal", "Real")
    ) %>% 
  filter(type == "Real") %>% 
  select(
    year, 
    price, 
    primary_commodity
  )
  
saveRDS(price_long, here(build.dir, "Africa", "Mines", "commodity_price_long.rds"))
