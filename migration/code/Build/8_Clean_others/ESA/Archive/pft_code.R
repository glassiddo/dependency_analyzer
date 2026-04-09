rm(list=ls())
pacman::p_load(
  terra, janitor, tidyr, stringr, purrr, sf, ggplot2, dplyr,
  exactextractr, terra
)
setwd("C:/Users/iddo2/Dropbox/ESA")
sf_use_s2(FALSE)
options(scipen=999)

## the idea is to take over 30% forest cover (combined nd+ne)
## in africa in each of the years, 
## and keep the loss between 92 and 00 and then 00 to 10 
## as 'existed in t but not in t+1' 

africa <- ext(-20, 60, -40, 20)

yrs <- c("1992", "2000")
r_combined_list <- vector("list", length(yrs))
names(r_combined_list) <- yrs
for (yr in yrs) {
  file_name <- paste0("pft/ESACCI-LC-L4-PFT-Map-300m-P1Y-", yr, "-v2.0.81.nc")
  r <- rast(file_name, subds = c("TREES-BD", "TREES-BE"))  
  # skip TREES-ND and TREES-NE as they don't exist in SSA
  
  ## crop to africa, sum layers, filter to over 30, and turn into binary
  r_sum_afr <- crop(r, africa) %>% 
    sum(na.rm = TRUE) # sum the two layers

  r_combined_list[[yr]] <- r_sum_afr > 30
}

r92 <- r_combined_list$`1992`
r00 <- r_combined_list$`2000`

loss_92_00 <- (r92 == 1) & (r00 == 0)
forest_92 <- r92 == 1

cell_area_ha <- cellSize(r92, unit = "ha")

forest_92_ha <- forest_92 * cell_area_ha
loss_92_00_ha <- loss_92_00 * cell_area_ha

su <- read_sf("units/sampleUnits.shp") %>% 
  mutate(
    total_area_ha = as.numeric(st_area(geometry)) / 10000,
    forest_92_ha = exact_extract(forest_92_ha, geometry, 'sum'),
    loss_92_00_ha = exact_extract(loss_92_00_ha, geometry, 'sum'),
    share_lost_92_00 = loss_92_00_ha / forest_92_ha
  )

ggplot() + geom_sf(data = su1, aes(fill =share_lost_92_00 ))
