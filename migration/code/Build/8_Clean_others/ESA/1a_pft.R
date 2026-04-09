#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    February 6, 2026
#* Title:   Get forest loss per unit before from Harper et al (2025) 
#* Desc:    Get forest loss estimates from ESA Global Plant Functional Types
#*******************************************************************************
# data - https://catalogue.ceda.ac.uk/uuid/313854aedcb04a5eb56f711401a87396/

source("code/SSA_env_SetUp.R")

africa_ext <- ext(-20, 60, -40, 20)

forest_threshold <- 30
calculate_forest_loss <- function(year_start, year_end, africa_extent, threshold) {
  
  # process the file name and only keep TREES-BD and TREES-BE classes
  process_year <- function(yr) {
    file_name <- paste0("pft/ESACCI-LC-L4-PFT-Map-300m-P1Y-", yr, "-v2.0.81.nc")
    r <- rast(file_name, subds = c("TREES-BD", "TREES-BE"))
    
    # crop to africa
    r_cropped <- crop(r, africa_extent)
    # combine the values of the two forest classes
    r_sum <- sum(r_cropped, na.rm = TRUE)
    
    # return binary if above threshold
    return(r_sum > threshold)
  }
  
  cat("Processing year", year_start, "...\n")
  r_start <- process_year(year_start)
  
  cat("Processing year", year_end, "...\n")
  r_end <- process_year(year_end)
  
  # get the loss (forest in start year but not in end year)
  cat("Calculating forest loss...\n")
  loss <- (r_start == 1) & (r_end == 0)
  
  # calculate cell area
  cell_area_ha <- cellSize(r_start, unit = "ha")
  
  # get the areas of initial forest and of loss in hectares
  forest_start_ha <- r_start * cell_area_ha
  loss_ha <- loss * cell_area_ha
  
  return(list(
    forest_start = r_start,
    forest_end = r_end,
    loss = loss,
    forest_start_ha = forest_start_ha,
    loss_ha = loss_ha,
    year_start = year_start,
    year_end = year_end
  ))
}

# calculate for both periods
cat("=== Processing 1992-2000 ===\n")
results_92_00 <- calculate_forest_loss(1992, 2000, africa_ext, forest_threshold)

cat("\n=== Processing 2000-2010 ===\n")
results_00_10 <- calculate_forest_loss(2000, 2010, africa_ext, forest_threshold)

# extract the values from the raster to the sample units
su <- read_sf(here(build.dir, "Maps", "sampleUnits.shp")) %>% 
  mutate(total_area_ha = as.numeric(st_area(geometry)) / 10000)

add_forest_metrics <- function(su, results) {
  yr_start <- results$year_start
  yr_end <- results$year_end
  
  cat("Extracting metrics for", yr_start, "-", yr_end, "...\n")
  
  su %>% 
    mutate(
      !!paste0("forest_", yr_start, "_ha") := exact_extract(
        results$forest_start_ha, geometry, 'sum'
      ),
      !!paste0("loss_", yr_start, "_", yr_end, "_ha") := exact_extract(
        results$loss_ha, geometry, 'sum'
      )
    ) %>%
    mutate(
      !!paste0("share_lost_", yr_start, "_", yr_end) := 
        .data[[paste0("loss_", yr_start, "_", yr_end, "_ha")]] / 
        .data[[paste0("forest_", yr_start, "_ha")]]
    )
}

su <- su %>%
  add_forest_metrics(results_92_00) %>%
  add_forest_metrics(results_00_10) %>% 
  st_drop_geometry()

saveRDS(
  su, 
  here(raw.dir, "Africa", "Forest", "esa and pft", "sample_units_pft.rds")
  ) 