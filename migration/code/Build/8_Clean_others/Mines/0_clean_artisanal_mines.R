source("code/SSA_env_SetUp.R")

africa_sf <- read_sf(here(build.dir, "Africa", "Maps", "SampleUnits.shp"))

main_data <- read_dta(here(raw.dir, "Africa", "Artisinal Mines", "dataverse_files", 
                           "Stata", "Data", "main.dta")) %>%
  select(gid, x, y, starts_with("snl_"), starts_with("asm_voted"), 
         modal_asm, snl_commodity, year, snl_price, asm_price)

main_sf <- main_data %>%
  filter(!is.na(x), !is.na(y)) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(africa_sf))

sf_use_s2(FALSE)
joined_data <- st_join(main_sf, africa_sf %>% select(ipums_id), left = FALSE) %>%
  st_drop_geometry()

processed_data <- joined_data %>%
  mutate(asm_prob = pmax(!!!select(., starts_with("asm_voted_prob")), na.rm = TRUE)) %>%
  mutate(snl_mines = rowSums(select(., snl_aluminum:snl_zinc), na.rm = TRUE)) %>%
  mutate(
    snl_price = if_else(snl_price == 0, NA_real_, snl_price),
    asm_price = if_else(asm_price == 0, NA_real_, asm_price)
  )

collapsed_data <- processed_data %>%
  group_by(ipums_id, year) %>%
  reframe(
    snl_price = max(snl_price, na.rm = TRUE),
    asm_price = max(asm_price, na.rm = TRUE),
    asm_prob = mean(asm_prob, na.rm = TRUE),
    snl_mines = sum(snl_mines, na.rm = TRUE),
  ) %>%
  mutate(
    snl_price = if_else(is.infinite(snl_price), NA_real_, snl_price),
    asm_price = if_else(is.infinite(asm_price), NA_real_, asm_price)
  )

# forestloss <- read_dta(here(out.dir, "forestloss.dta")) %>%
#   select(ipums_id, starts_with("fl"))
# 
# merged_data <- collapsed_data %>%
#   inner_join(forestloss, by = "ipums_id") %>%
#   filter(year == fl_start_yr | year == fl_end_yr) %>%
#   arrange(ipums_id, year)
# 
# final_data <- merged_data %>%
#   group_by(ipums_id) %>%
#   mutate(
#     d_logsnl_price = if_else(
#       year == fl_end_yr & !is.na(snl_price) & !is.na(lag(snl_price)),
#       log((snl_price / lag(snl_price))^(1 / (fl_end_yr - fl_start_yr))),
#       NA_real_
#     ),
#     d_logasm_price = if_else(
#       year == fl_end_yr & !is.na(asm_price) & !is.na(lag(asm_price)),
#       log((asm_price / lag(asm_price))^(1 / (fl_end_yr - fl_start_yr))),
#       NA_real_
#     ),
#     grsnl_price = if_else(
#       year == fl_end_yr & !is.na(snl_price) & !is.na(lag(snl_price)),
#       (snl_price - lag(snl_price)) / (lag(snl_price) * (fl_end_yr - fl_start_yr)),
#       NA_real_
#     ),
#     grasm_price = if_else(
#       year == fl_end_yr & !is.na(asm_price) & !is.na(lag(asm_price)),
#       (asm_price - lag(asm_price)) / (lag(asm_price) * (fl_end_yr - fl_start_yr)),
#       NA_real_
#     )
#   ) %>%
#   ungroup()
# 
# output_data <- final_data %>%
#   filter(year == fl_end_yr) %>%
#   select(ipums_id, starts_with("d_"), starts_with("gr"), snl_mines, asm_prob)


existing <- read_dta(here(build.dir, "Africa/Artisinal/Artisinal.dta"))

## will keep asm_voted_probabilty rather than the binary variable
id_df <- readRDS(here(out.dir, "R", "id.rds")) %>% select(ipums_id, country)

highest_prob_asm <- joined_data %>% 
  #filter(gid == "35694") %>% 
  select(gid, ipums_id, starts_with("asm_voted_p")) %>%  # no need for the price/year
  distinct() %>%  # there are all constant over time
  pivot_longer(
    cols = starts_with("asm_voted_p"),
    names_to = "commodity",
    values_to = "prob"
  ) %>% 
  group_by(ipums_id, gid) %>% 
  slice_max(prob, n = 1, with_ties = F) %>% 
  group_by(ipums_id) %>% 
  slice_max(prob, n = 1, with_ties = F)%>% 
  mutate(
    cleaned_commodity = sub("^asm_voted_prob_", "", commodity),
    primary_commodity = case_when( 
      # could be done with str_to_title but need to add s to diamond..
      # and only two variables that require changes
      cleaned_commodity == "diamond" ~ "Diamonds",
      cleaned_commodity == "gold" ~ "Gold",
      TRUE ~ cleaned_commodity
    )
  ) %>% 
  full_join(id_df, by = "ipums_id") %>% 
  select(ipums_id, country, primary_commodity, prob_asm = prob)

saveRDS(highest_prob_asm, here(build.dir, "Africa", "Artisinal", "highest_prob_asm.rds"))