#* Project: Migration Africa
#* Author:  Sam Marshall
#* Created: January 20, 2023
#* Title:   Build Master
#* Desc:    Import, clean, and build data
#* NB:      Within each folder, files starting with 01- have no dependencies, 
#*          02- have 01- dependencies and so forth. The builds can all be run
#*          in parallel except for the Bartik file which requires all the other
#*          files to have been built. Files starting with F- contain functions.
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")

# Install packages if user doesn't have them
# install.packages(c("ggplot2", "stringr", "tidyverse", "sf", "broom", 
#                   "magrittr", "ggrepel", "haven", "readxl", "sjlabelled",
#                   "DescTools", "tidyr"))

#*******************************************************************************
# 1a. Census ----
# build country datasets from IPUMS data
source("code/build/Census/1-XC_census_year.R")
# create level 2 data set for South Africa
source("code/build/Census/2-SAF-IPUMS-xwalk.R")
source("code/build/Census/3-SAF-census.R")

#*******************************************************************************
# 1b. Forest ----
# Forest loss (30% cut off)
source("code/build/Forest/01-Forest_build_loss30.R")
source("code/build/Forest/02-Forest_loss30_cons.R")

#*******************************************************************************
# 1c. Urban, distance ----
# locations of cities
source("code/build/Urban/1-import_cities.R")
# centroids and spatial data frames for each country
source("code/build/Urban/2-build_centroids.R")
# distance between geographic units and share of area urban
source("code/build/Urban/3-build_dist_urb.R")

#*******************************************************************************
# 1d. Night light ----
# night light area and intensity
source("code/build/Night Light/1-NL_panel.R")
source("code/build/Night Light/2-NL_ctry_dat.R")

#*******************************************************************************
# 1e. crop area ----
# npix and sharecrop
source("code/build/Crop Area/1-CA_import.R")
source("code/build/Crop Area/2-CA_ctry_dat.R")
source("code/build/Crop Area/3-GAEZ_import.R")
source("code/build/Crop Area/4-GAEZ_ctry_dat.R")

#*******************************************************************************
# 2. Main Files ----
source("code/Final/1-ID.R")
source("code/Final/2-ForestLoss.R")
source("code/Final/3-NightLight.R")
source("code/Final/4-Population.R")
source("code/Final/5-Employment.R")
source("code/Final/6-CropArea.R")
source("code/Final/7-Emigration.R")
source("code/Final/9-Migration.R")
source("code/Final/10-Distance.R")
source("code/Final/11-Urban.R")

# 3. Bartik ----
#* instruments and panel dataset
source("code/Final/12-Bartik.R")
