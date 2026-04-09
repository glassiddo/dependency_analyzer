rm(list=ls())
pacman::p_load(
  terra, janitor, tidyr, stringr, purrr, sf, ggplot2, dplyr,
  exactextractr
)
setwd("C:/Users/iddo2/Dropbox/ESA")
sf_use_s2(FALSE)

su <- read_sf("sampleUnits.shp") %>% 
  mutate(
    total_area_ha = as.numeric(st_area(geometry)) / 10000
  )

forest_classes <- c(
  50, 60, 61, 62, 70, 80, 90
  # 40, 100
  # 71-72, 81-82 don't really seem to exist in africa
  # 150, 160
)
reclass_m <- cbind(forest_classes, 1) 
years <- 1992:2015

results_list <- list()

base_1992 <- rast("LC_1992.tif")
base_2000 <- rast("LC_2000.tif")

pixel_area <- prod(res(base_1992)) / 10000

su_transformed <- st_transform(su, st_crs(base_1992))
su_vect <- vect(su_transformed)

base_1992 <- crop(base_1992, su_vect, snap = "out")
base_mask_1992 <- classify(base_1992, reclass_m, others=0)
base_forest_pixels_1992 <- exact_extract(
  base_mask_1992, su_transformed, 'sum', progress = FALSE
)
  
base_2000 <- crop(base_2000, su_vect, snap="out")
base_mask_2000 <- classify(base_2000, reclass_m, others=0)
base_forest_pixels_2000 <- exact_extract(
  base_mask_2000, su_transformed, 'sum', progress = FALSE
)

for (year in years) {
  cat("Processing year", year, "\n")
  
  if (year <= 2000) {
    base_mask <- base_mask_1992
    base_year <- 1992
    base_forest_pixels <- base_forest_pixels_1992
  } else {
    base_mask <- base_mask_2000
    base_year <- 2000
    base_forest_pixels <- base_forest_pixels_2000
  }
  
  target_yr <- rast(paste0("LC_", year, ".tif"))
  target_yr <- crop(target_yr, su_vect, snap="out")
  target_mask <- classify(target_yr, reclass_m, others=0)
  
  forest_loss <- (base_mask == 1) & (target_mask == 0)
  
  loss_count <- exact_extract(
    forest_loss, su_transformed, 'sum', progress = T
  )
  
  result <- su_transformed %>%
    st_drop_geometry() %>%
    mutate(
      year = year,
      base_year = base_year,
      base_treecover_ha = pixel_area * base_forest_pixels,
      loss_pixels = loss_count,
      loss_ha = pixel_area * loss_pixels,
      share_lost = loss_ha / total_area_ha,
      share_lost_treecover = loss_ha / base_treecover_ha
    )
  
  results_list[[as.character(year)]] <- result
}

all_results <- bind_rows(results_list)

saveRDS(all_results, "esa_loss_v1.rds")