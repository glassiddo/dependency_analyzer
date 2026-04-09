#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    October 9, 2025
#* Title:   Create instruments
#* Desc:    Create shift share instruments using migration, pop., and empl. data
#*******************************************************************************

########## ======================= ############
# TODO
## 1) determine nl_cutoff with the new
## 2) should the country urban vs nonag logic be determined elsewhere?
## 3) fix the country variable - specifically for ctries_to_always_take_mig1
## 4) simplify the code whereever possible
## 5) nonag is replaced by urb_pop, probably should be urb_emp?
## 6) burkina faso - explain 

# Set Up ----
source("code/SSA_env_SetUp.R")

##### ------------ Parameters ------------ 

# the nightlight variables used in the loop
lvec <- c(
  "tlv", # nightlight value, 
  "tlla", # nightlight area
  "tlv_ra", 
  "tlla_ra"
  )

shifts <- c(
  "gr_nonag_ann", "gr_urban_pop_ann", "AR1_residual",
  paste0("gr_n", lvec) # add gr_n_[lvec]
)

ishares <- c("d", "m_d", "m_od", "m_od_L", "dXp_L", "d_C_Xp_L", "d_S_Xp_L")
shares <- c(
  "m_od", "DurXp", "m_od_L", "DXp_L","DurXp_L","D_S_Xp_L", "D_C_Xp_L" ,"Dur_C_Xp_L", 
  "Dur_S_Xp_L","m_od_p","DXp", "DurXp", "DXp_p", "DurXp_p"
)

# countries (iso3 codes) that should take urban values for nonag variables
ctries_urban_vals_for_nonag <- c("CMR", "ZAF")
# or vice versa
ctries_nonag_vals_for_urban <- c("BWA", "BFA", "ZMB")
# countries that should always take migration 1 data
ctries_to_always_take_mig1 <- c("Ghana", "Ivory Coast") # should be changed to iso3

# minimal threshold for night light 
nl_cutoff <- 0
# built types
built_types <- c("prop2", "type1", "type2", "type3", "type4", "type5")

### read data -----
employment <- readRDS(here(out.dir, "R", "employment.rds"))
population <- readRDS(here(out.dir, "R", "population.rds")) 
urban <- readRDS(here(out.dir, "R", "urban.rds"))
mine_instrument <- readRDS(here(out.dir, "R", "mine_instrument.rds")) 
modis_lc_wide <- readRDS(here(out.dir, "R", "MODIS_LC_wide.rds")) %>% 
  select(ipums_id, wave, contains("barea"))
nightlight_p1 <- readRDS(here(out.dir, "R", "nightlight.rds")) %>% 
  mutate(wave = 1)
nightlight_p2 <- readRDS(here(out.dir, "R", "nightlight_period2.rds")) %>% 
  mutate(wave = 2)
dist_mat <- readRDS(here(out.dir, "R", "dist_mat.rds")) %>% 
  select(ipums_id_d, ipums_id_o, contains("osm"))

####### repeated functions -----
fix_ctry_nonag_urban <- function(data, take_urban_ctries, take_nonag_ctries,
                                 prefix = "", suffix = "") {
  ## some countries require replace urban variables with nonag ones
  ## or vice versa
  ## this code takes the non agg variables or the urban pop vars
  ## depending on the exact prefix and suffix (often there's no suffix)
  nonag_var <- paste0(prefix, "nonag", suffix)
  urban_var <- paste0(prefix, "urban_pop", suffix)
  
  data %>%
    mutate(
      "{nonag_var}" := ifelse( # if in this country codes list, 
        country %in% take_urban_ctries, 
        .data[[urban_var]], # take the value from urban 
        .data[[nonag_var]] # to the nonag var
      ),
      "{urban_var}" := ifelse(
        country %in% take_nonag_ctries, # and vice versa
        .data[[nonag_var]], 
        .data[[urban_var]]
      )
    )
}

create_growth_rates <- function(df, types, 
                                delta_prefix, base_prefix, gr_prefix, 
                                cutoff = NULL) {
  for (t in types) {
    delta_var <- paste0(delta_prefix, t)
    base_var  <- paste0(base_prefix, t)
    gr_var    <- paste0(gr_prefix, t)
    
    df[[gr_var]] <- safe_divide(df[[delta_var]], df[[base_var]])
    
    # if a cutoff is provided, set to 0 when below cutoff
    if (!is.null(cutoff)) {
      df[[gr_var]] <- ifelse(df[[base_var]] <= cutoff, 0, df[[gr_var]])
    }
  }
  return(df)
}

# ---- Wave 1 shifts ----
# *** merge data necessary to generate the shifts and save this 
# in a file called shifts (which is also used later when we do the inversion)
# This does it for wave 1, when we have multiple shifts to choose from

# Start from employment then inner join population and mine_instrument (matched only)
shifts_1 <- employment %>%
  left_join(population, by = "ipums_id", suffix = c("", "_remove_pop")) %>%
  # has the shift "ar1_residual"
  left_join(mine_instrument, by = "ipums_id", suffix = c("", "_remove_mines")) %>% 
  left_join(nightlight_p1, by = "ipums_id") %>%  
  left_join(modis_lc_wide, by = c("ipums_id", "wave")) %>% 
  select(-contains("_remove_")) %>% # and remove it
  mutate(
    AR1_residual = replace_na(AR1_residual, 0),
    gr_nonag_ann = safe_divide(delta_nonag_ann, bl_nonag)
    # get the growth of non agricultural employment - 
    # the change (in levels) divided by the level at baseline
  ) %>% 
  fix_ctry_nonag_urban(
    # for some countries, take urban/nonag baseline values for the other type 
    take_urban_ctries = ctries_urban_vals_for_nonag, 
    take_nonag_ctries = ctries_nonag_vals_for_urban,
    prefix = "bl_"
  ) 

# get the (prime aged - if prime_age = TRUE) population by country
pop_by_ctry <- shifts_1 %>% 
  group_by(country) %>% 
  summarise(sum_pop = sum(bl_pop, na.rm = TRUE), .groups = "drop") 

shifts_1 <- shifts_1 %>% 
  left_join(pop_by_ctry, by = "country") %>%
  mutate(
    # change in non agricultural employment
    # compared to the baseline pop *in the country*
    delta_nonag_ann_sum_pop = safe_divide(delta_nonag_ann, sum_pop),
    # growth of urban population
    gr_urban_pop_ann = safe_divide(delta_urban_pop_ann, bl_urban_pop)
  ) %>%
  create_growth_rates( # built growth rates
    # for each type, get the growth rate
    # delta prefix is the change in levels
    # base_prefix is the baseline level
    # gr_prefix is the variable that is create, of the growth rate
    types = built_types,
    delta_prefix = "delta_barea_lc_",
    base_prefix  = "barea_lc_",
    gr_prefix    = "gr_barea_"
  ) %>% 
  create_growth_rates( # nightlight growth rates
    types = lvec,
    delta_prefix = "delta_n",
    base_prefix  = "bl_n",
    gr_prefix    = "gr_n",
    # only keep if above the nightlight cutoff, otherwise set to 0
    cutoff       = nl_cutoff
  )

# replace delta_/delta_log_/gr_ variants for those countries
prefixes <- c("delta_", "delta_log_", "gr_")
for (pref in prefixes) {
  shifts_1 <- shifts_1 %>% 
    fix_ctry_nonag_urban(
      take_urban_ctries = ctries_urban_vals_for_nonag, 
      take_nonag_ctries = ctries_nonag_vals_for_urban,
      prefix = pref,
      suffix = "_ann" # suffix of the variables
    )
}

### only keep the shifts, country name, wave and the shifts' list 
shifts_1 <- shifts_1 %>% 
  select(
    any_of(
      c("ipums_id", "wave", shifts))
    ) %>%
# rename everything with a _d on end
  rename_with(.fn = ~ paste0(.x, "_d"), .cols = -c("wave")) 

# ---- Wave 2 shifts ----
shifts_2 <- nightlight_p2 %>%
  left_join(modis_lc_wide, by = c("ipums_id", "wave")) %>% 
  create_growth_rates( # built growth rates
    types = built_types,
    delta_prefix = "delta_barea_lc_",
    base_prefix  = "barea_lc_",
    gr_prefix    = "gr_barea_"
  ) %>% 
  create_growth_rates( # nightlight growth rates
    types = lvec,
    delta_prefix = "delta_n",
    base_prefix  = "bl_n",
    gr_prefix    = "gr_n",
    cutoff       = nl_cutoff
  ) %>%   # for some countries swap urban/nonag baseline values 
  select(
    ipums_id, wave, starts_with("delta"), 
    starts_with("gr") # one thing to note about that - 
    # given the flexible gr, some growth rates will be kept in final shifts df
    # but have NA values for wave 1
  ) %>%
  rename_with(~ paste0(.x, "_d"), .cols = -c("wave")) 

#### save shifts -----
shifts_all <- bind_rows(shifts_1, shifts_2)

saveRDS(shifts_1, here(out.dir, "R", paste0("shifts_1.rds")))
write_dta(shifts_1, here(out.dir, paste0("shifts_1.dta")))

saveRDS(shifts_2, here(out.dir, "R", paste0("shifts_2.rds")))
write_dta(shifts_2, here(out.dir, paste0("shifts_2.dta")))

saveRDS(shifts_all, here(out.dir, "R", paste0("shifts.rds")))
write_dta(shifts_all, here(out.dir, paste0("shifts.dta")))

######## bartiks ------

########## generate population, employment vars for origin 

origin <- employment %>%
  left_join(
    population, 
    by = "ipums_id",
    # add '_suffpop' suffix to resolve name conflicts between datasets
    suffix = c("", "_suffpop") 
  ) %>% 
  select(-ends_with("_suffpop")) %>% # and remove it
  left_join(urban %>% select(ipums_id, total_area), by = "ipums_id") %>%
  # add '_o' suffix to all columns except identifiers (that are needed for merging)
  rename_with(~ paste0(.x, "_o"), -c(ipums_id, country_name, country))

#### loop over waves and build shares/bartiks
# create lists to store intermediate data frames for new_bartiks / shares
new_bartiks_list <- list()

for (w in c(1, 2)) {
  # get the relevant shifts
  shifts_w <- if (w == 1) shifts_1 else shifts_2
  # load migration matrix for the wave
  df <- readRDS(here(out.dir, "R", paste0("full_migration_census", w, ".rds")))
  # merge distance matrices and distance census
  dist_df <- readRDS(here(out.dir, "R", paste0("full_distance_census", w, ".rds"))) 
  
  # # for specific countries - always take rows of wave 1
  if (w == 2 & length(ctries_to_always_take_mig1) > 0) {
    df1_spec_ctries <- readRDS(here(out.dir, "R", "full_migration_census1.rds")) %>%
      filter(country_name %in% ctries_to_always_take_mig1)

    df2_no_spec_ctries <- df %>%
      filter(!country_name %in% ctries_to_always_take_mig1)

    df <- bind_rows(df1_spec_ctries, df2_no_spec_ctries)
  }
  
  df <- df %>% 
    mutate(
      # rename vars
      intm_od = m_od, # internals migration from origin to destination
      # for the origin variable, set NA if migration is between countries
      m_od = if_else(country_name != prev_country_name, NA_real_, m_od)
    )
  
  df <- df %>% 
    left_join(
      dist_df, 
      by = c("ipums_id_d", "ipums_id_o"),
      suffix = c("", "_dist")
    ) %>% 
    select(-ends_with("_dist")) %>% 
    left_join(
      dist_mat,
      by = c("ipums_id_d", "ipums_id_o")
    ) %>% 
    # same thing as before with internal migration vars
    mutate(
      intinv_dist_shares = inv_dist_shares,
      inv_dist_shares = if_else(
        country_name != prev_country_name, NA_real_, inv_dist_shares)
    ) %>%
    left_join(
      origin, # (employment, urban and pop in origin, vars end with _o)
      by = c("ipums_id_o" = "ipums_id"),
      suffix = c("", "_orig") 
    ) %>% 
    select(-ends_with("_orig")) %>% 
    left_join(shifts_w, by = "ipums_id_d") %>% 
    rename(
      ipums_id = ipums_id_d
    ) %>%
    # merge population and employment by ipums_id 
    left_join(
      population %>% 
        select(ipums_id, 
               contains("l_pop"), 
               contains("l_urban_pop")
        ), 
      by = "ipums_id",
      suffix = c("", "_sufpop") 
    ) %>% 
    left_join(
      employment %>% 
        select(ipums_id, 
               starts_with("l_nonag_")
        ), 
      by = "ipums_id",
      suffix = c("", "_sufemp")
    ) %>% 
    select(
      -ends_with("_sufpop"),
      -ends_with("_sufemp")
    )
  
  # for wave 2, replace some baseline population variables with endline
  if (w == 2) {
    for (p in c("pop", "urban_pop", "nonag")) {
      blp <- paste0("bl_", p)
      elp <- paste0("el_", p)
      
      if (elp %in% names(df)) {
        df <- df %>% 
          mutate(!!blp := .data[[elp]]
          )
      }
    }
  }
  
  df <- df %>%
    rename(
      bl_nonag = bl_nonag_o
    ) %>% 
    fix_ctry_nonag_urban(
      take_urban_ctries = ctries_urban_vals_for_nonag, 
      take_nonag_ctries = ctries_nonag_vals_for_urban,
      prefix = "bl_"
    ) %>% 
    # drop country_name and rename prev_country_name -> country_name
    select(-country_name) %>% 
    rename(
      bl_pop_d = bl_pop, 
      bl_urban_pop_d = bl_urban_pop, 
      bl_nonag_d = bl_nonag,
      ipums_id_d = ipums_id,
      country_name = prev_country_name,
      
      m_d_shares      = d_shares,
      m_o_shares      = o_shares,
      im_d_shares     = intd_shares,
      im_o_shares     = into_shares,
      
      d_shares        = inv_dist_shares,
      id_shares       = intinv_dist_shares,
      
      im_od_shares    = intm_od,
      m_od_shares     = m_od,
      
      D_shares        = inv_osm_dist_shares,
      Dur_shares      = inv_osm_dur_shares
    )
  
  transform_shares <- function(df, share, pop_var = "bl_pop_d") {
    share_col <- paste0(share, "_shares")
    if (!share_col %in% names(df)) return(df)
    
    df %>%
      mutate(
        !!paste0(share, "Xp_shares") := .data[[share_col]] * .data[[pop_var]] / 100,
        !!paste0(share, "_C_Xp_shares") := (.data[[share_col]] ^ (-0.17)) * .data[[pop_var]] / 100,
        !!paste0(share, "_S_Xp_shares") := (1 / (1 + exp(-0.104 + 0.437 * log(.data[[share_col]] + 1)))) * .data[[pop_var]] / 100
      )
  }
  
  for (share in c("d", "D", "Dur")) {
    df <- transform_shares(df, share)
    df <- transform_shares(df, paste0("i", share), pop_var = "bl_pop_d")
  }
  
  share_set2 <- c("im_od", "m_od", "idXp", "id_C_Xp", "id_S_Xp",
                  "DXp", "D_C_Xp", "D_S_Xp",
                  "DurXp", "Dur_C_Xp", "Dur_S_Xp")
  
  for (share in share_set2) {
    share_shares <- paste0(share, "_shares")
    if (share_shares %in% names(df)) {
      df <- df %>%
        mutate(
          !!paste0(share, "_L_shares") := .data[[share_shares]] / .data[["total_area_o"]],
          !!paste0(share, "_p_shares") := .data[[share_shares]] / .data[["bl_pop_o"]]
        )
    }
  }
  
  # First loop
  for (shift in shifts) {
    # Create the shift_d column if it doesn't exist
    shift_d_col <- paste0(shift, "_d")
    if (!shift_d_col %in% names(df)) {
      df[[shift_d_col]] <- NA
    }
    
    # Loop through shares
    for (share in shares) {
      share_col <- paste0(share, "_shares")
      
      # Check if the required column exists
      if (share_col %in% names(df)) {
        df[[paste0("B", share, "_", shift)]] <- df[[share_col]] * df[[shift_d_col]]
      } 
    }
  }
  
  # Second loop
  for (shift in shifts) {
    shift_d_col <- paste0(shift, "_d")
    
    for (share in ishares) {
      ishare_col <- paste0("i", share, "_shares")
      
      # Check if the required column exists
      if (ishare_col %in% names(df)) {
        df[[paste0("iB", share, "_", shift)]] <- df[[ishare_col]] * df[[shift_d_col]]
      } 
    }
  }
  
  
  # NEW CODE: Create normalized shares and normalized Bartiks
  # First, calculate sum of shares for each origin
  df <- df %>%
    group_by(ipums_id_o) %>%
    mutate(
      # Calculate sum of shares across all destinations for each share type
      across(
        ends_with("_shares"),
        ~ sum(.x, na.rm = TRUE),
        .names = "{.col}_origin_sum"
      )
    ) %>%
    ungroup() %>%
    mutate(
      # Create normalized shares (shares that sum to 1 for each origin)
      across(
        ends_with("_shares") & !ends_with("_origin_sum"),
        ~ .x / get(paste0(cur_column(), "_origin_sum")),
        .names = "{.col}_norm"
      )
    )
  
  # Create normalized Bartiks (BN prefix) using normalized shares
  for (shift in shifts) {
    shift_d_col <- paste0(shift, "_d")
    
    if (!shift_d_col %in% names(df)) {
      df[[shift_d_col]] <- NA
    }
    
    for (share in shares) {
      share_norm_col <- paste0(share, "_shares_norm")
      
      if (share_norm_col %in% names(df)) {
        df[[paste0("BN", share, "_", shift)]] <- df[[share_norm_col]] * df[[shift_d_col]]
      }
    }
  }
  
  # Clean up temporary columns
  df <- df %>%
    select(-ends_with("_origin_sum"))
  
  
  
  # sum all columns starting with B, iB, or ending with _shares
  # then group by ipums_id_o and country_name
  
  df <- df %>%
    select(
      ipums_id_o, country, 
      starts_with("B", ignore.case = FALSE), # if ignore.case = T, keeps bl_
      starts_with("iB", ignore.case = FALSE),
      starts_with("BN", ignore.case = FALSE), 
      ends_with("_shares")
    ) %>%
    group_by(ipums_id_o, country) %>%
    summarise(
      across(
      everything(),
      ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)
      ), .groups = "drop"
    ) %>% 
    # explanation about the summarise above - 
    # it should be NA if all values are NA 
    # but if some are NA and some aren't, then exclude NA (na.rm = T)
    # if we use a simple na.rm = T for all cases here, it creates 0s
    rename(ipums_id = ipums_id_o)
  
  # rename shares and ishares
  all_shares <- c(shares, paste0("i", ishares))
  
  for (share in all_shares) {
    old_name <- paste0(share, "_shares")
    new_name <- paste0(share, "_shares_sum")
    if (old_name %in% names(df)) {
      df <- df %>% rename(!!sym(new_name) := !!sym(old_name))
    }
  }

  # store new_bartiks for this wave
  df <- df %>% 
    mutate(wave = w)

  new_bartiks_list[[as.character(w)]] <- df 
  
  saveRDS(df, here(out.dir, "R", paste0("new_bartiks_", w, ".rds")))
  write_dta(df, here(out.dir, paste0("new_bartiks_", w, ".dta")))
}

# ---- Combine new_bartiks_1 and new_bartiks_2 into a single df ----
new_bartiks <- bind_rows(new_bartiks_list)

saveRDS(new_bartiks, here(out.dir, "R", paste0("new_bartiks.rds")))
write_dta(new_bartiks, here(out.dir, paste0("new_bartiks.dta")))

rm(df, df1_spec_ctries, df2_no_spec_ctries, dist_df, dist_mat, employment,
   mine_instrument, modis_lc_wide, new_bartiks_list,
   nightlight_p1, nightlight_p2, origin, pop_by_ctry, population, 
   shifts_w, urban)
