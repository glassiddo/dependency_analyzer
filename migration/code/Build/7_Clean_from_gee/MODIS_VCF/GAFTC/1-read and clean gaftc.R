### this code gets averages for our sample units from the following paper:
## Liu, Y., Liu, R., Chen, J. et al. "A global annual fractional tree cover 
## dataset during 2000–2021 generated from realigned MODIS seasonal data." 
## Sci Data 11, 832 (2024). https://www.nature.com/articles/s41597-024-03671-9

## download the files from here - https://zenodo.org/records/10589730

## given that the files are heavy, it takes some time and requires 
## a large hard drive or gradually running it and deleting the older tif files
rm(list = ls())
pacman::p_load(
  dplyr, tidyr, stringr, janitor, sf,
  ggplot2, data.table, haven, readr, tidyverse, purrr, terra, exactextractr
)

select <- dplyr::select
options(scipen=999)

setwd("C:/Users/iddo2/Dropbox")
data_dir <- "modis"

all_files <- list.files(
  data_dir, pattern = "^GLOBMAPFTC.*\\.tif$", 
  full.names = TRUE, recursive = T
  )

all_file_years <- as.numeric(sub(".*\\.A(\\d{4}).*", "\\1", basename(all_files)))
ref_raster <- rast(all_files[1])
target_crs <- crs(ref_raster)

yr <- 2010
yr_files <- all_files[all_file_years == yr]

sampleunits <- read_sf("Migration Africa/data/Build/Africa/Maps/SampleUnits.shp")
sampleunits_proj <- st_transform(sampleunits, target_crs)
  
# a virtual raster that creates a mosaic in memory w/o/ reading pixel data
vrt_yr <- vrt(yr_files)

# extract a weighted average of the values depending on area of raster 
# that's overlapping with the (projected) sample units. 
# equivalent to using fun = 'sum' and dividing by fun = 'count'
avg_gaftc <- exact_extract(vrt_yr, sampleunits_proj, fun = "mean")

df_yr <- sampleunits %>% 
  select(ipums_id) %>% 
  st_drop_geometry() %>% 
  mutate(
    year = yr,
    gaftc = avg_gaftc
  )

write_csv(df_yr, file.path(
  getwd(),
  "Migration Africa", "data", "Build", "Africa", "MODIS_VCF", "GAFTC", 
  paste0("gaftc", yr, ".csv")
  ))
