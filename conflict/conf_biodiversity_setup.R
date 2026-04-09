#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025
#* Title:   Project Set-Up
#* Note:    Run this file first. Adjust global data paths          
#*******************************************************************************

# clean env
rm(list=ls())
# load packages
pacman::p_load(
  dplyr, haven, tidyverse, reshape2, data.table, purrr, janitor, stringr,
  here, # for reading files
  slider, 
  tabulapdf, # for reading the UN tourism yearbook pdfs
  #readxl, plotrix, classInt, # might not be needed
  #stargazer, kableExtra, # for outputs - maybe not needed
  ggplot2, 
  ncdf4, # Read, write, and create netCDF files
  sf, # main one for spatial analysis
  nngeo, # nearest neighbour spatial matching
  geodata, spData, terra, maps, sp, raster, # additional spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  fixest, conleyreg, lmtest # empirical analysis
  #, did, DIDmultiplegtDYN, bacondecomp, lfe, staggered
)

# set working folder
data_dir <- "data"
raw_dir <- here(data_dir, "raw")
int_dir <- here(data_dir, "intermediate")
cln_dir <- here(data_dir, "cln")

## set some functions to always be dplyr
select <- dplyr::select
lag <- dplyr::lag

options(digits=3) # show 3 digits after decimal when printing
options(scipen=999) # avoid scientific numbers when printing
set.seed(as.integer(as.Date("2023-05-04"))) # random seed, start date of the project

last_yr_of_analysis <- 2019 # last year for data processing 
first_yr_of_analysis <- 1997 # first year of acled data

# get all potential years and months to expand grid
years <- seq(first_yr_of_analysis, last_yr_of_analysis)

# set CRS that's suitable for the entire continent
afr_crs <- "ESRI:102022"

