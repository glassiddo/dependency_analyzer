#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    February 6, 2026
#* Title:   Get forest loss per unit before from ESA
#* Desc:    Get forest loss estimates from ESA, only forest classes
#*******************************************************************************

source("code/SSA_env_SetUp.R")

africa_ext <- ext(-20, 60, -40, 20)

forest_classes <- c(
  50, 60, 61, 62, 70, 80, 90
  # 40 # ignore
  # 71-72, 81-82 don't really seem to exist in africa
  #, 160, 100 # could be included
)

reclass_m <- cbind(forest_classes, 1)

calculate_forest_loss <- function(year_start, year_end, africa_extent) {
  
  # read and process both years
  process_year <- function(yr) {
    file_name <- paste0("esa/LC_", yr, ".tif")
    r <- rast(file_name)
    
    # crop to africa
    r_cropped <- crop(r, africa_extent)
    # keep all cells that are classified as one of the relevant classes
    forest_mask <- classify(r_cropped, reclass_m, others = 0)
    
    return(forest_mask)
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
results_92_00 <- calculate_forest_loss(1992, 2000, africa_ext)

cat("\n=== Processing 2000-2010 ===\n")
results_00_10 <- calculate_forest_loss(2000, 2010, africa_ext)

su <- read_sf(here(build.dir, "Maps", "sampleUnits.shp")) %>% 
  mutate(total_area_ha = as.numeric(st_area(geometry)) / 10000)

# extract the values from the raster to the sample units
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
  here(raw.dir, "Africa", "Forest", "esa and pft", "sample_units_esa_5090.rds")
) 