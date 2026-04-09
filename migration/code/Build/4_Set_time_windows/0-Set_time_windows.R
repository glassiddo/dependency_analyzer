#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    January 13, 2026
#* Title:   Forest Loss and nightlight - set start+end yrs and
#* Desc:    For each country, determine which years we use for forest loss and nl change
#*******************************************************************************
# essentially moved from Final/2-forestloss_cuts.R and Final/3-NightLight.R 
# to an earlier and separate stage
id_df <- readRDS(here(out.dir, "R", "id.rds")) 

# these are all constant per countries

## for builtup area, it's every 5 years, take the nearest one
five_years_iv <- seq(1990, 2020, by = 5)

make_years <- function(id_df, w) {
  
  ## defaults based on wave for single census countries
  d_fl_yr_s <- 2001 # for wave 2, a bit diff logic. same with nl
  d_fl_yr_e <- if (w == 1) NA   else 2020
  
  d_nl_yr_s <- if (w == 1) 1996 else NA # data starts in 1993
  # so 3 yr rolling avg available from 96
  d_nl_yr_e <- if (w == 1) 2013 else 2020 # 2013 is the year of changing satellites
  
  d_bu_yr_s <- if (w == 1) 2000 else 2010
  d_bu_yr_e <- if (w == 1) 2010 else 2020
  
  id_df %>%
    rowwise() %>%
    transmute(
      country,
      wave = w,
      ## forest loss - also used for crop area, tmf, modis_vcf, geo. cotrols, modis_lc, gaftc
      fl_start_yr = if (w == 1) {
        # wave 1 - start at *census_yr1* + 1 or in 2001
        ifelse(census_yr1 >= 2000, census_yr1 + 1, d_fl_yr_s)
      } else {
        # wave 2 - start at *census_yr2* + 1 
        # or in 2001 if no second census (CIV) or if census_yr2 < 2000
        case_when(
          is.na(census_yr2) ~ max(census_yr1 + 1, 2001),
          # census_yr2 <= 2000 ~ 2001, # never relevant... 
          TRUE ~ census_yr2 + 1
        )
      },
      fl_end_yr = if (w == 1) { 
        ifelse(
          !is.na(census_yr2), 
          census_yr2, # should be based on the formula census_yr2 - 1
          # but it might be accounted for when using > rather than >=
          # TODO - check!
          d_fl_yr_e)
          } else{
            d_fl_yr_e
          },
      fl_period_len = fl_end_yr - fl_start_yr + 1,
      ## nightlight
      nl_start_yr = if (w == 1) {
        ifelse(census_yr1 >= 1995, census_yr1 + 1, d_nl_yr_s)
      } else {
        ifelse(is.na(census_yr2), census_yr1 + 1, census_yr2 + 1)
      },
      nl_end_yr = d_nl_yr_e,
      nl_prd_len = nl_end_yr - nl_start_yr + 1,
      ## ghsl
      bu_start_yr = ifelse(
        !is.na(census_yr1),
        five_years_iv[which.min(abs(five_years_iv - census_yr1))],
        d_bu_yr_s
      ),
      bu_end_yr = ifelse(
        !is.na(census_yr2),
        five_years_iv[which.min(abs(five_years_iv - census_yr2))],
        d_bu_yr_e
      ),
      bu_prd_len = bu_end_yr - bu_start_yr + 1
    ) %>%
    distinct()
}

yrs_wave1 <- make_years(id_df, 1)
yrs_wave2 <- make_years(id_df, 2)

saveRDS(yrs_wave1, here(build.dir, "Africa", "Support", "fl_nl_yrs_w1.rds"))
saveRDS(yrs_wave2, here(build.dir, "Africa", "Support", "fl_nl_yrs_w2.rds"))