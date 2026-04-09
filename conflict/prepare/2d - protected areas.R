#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   2 - Process protected areas 
#* Note:    Computing the share of each cell covered by PAs in each year          
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

include_points <- TRUE # normally set to TRUE 
# wdpa dataset is composed of polygons, and of points with estimated area
# the polygons are always processed
# if include_points is set to TRUE, then the points are used as centroids
# and buffered around them depending on the area, then merged with polygons
# if include_points is set to FALSE, then only polygons are processed

### WDPA africa -----
# downloaded from https://www.protectedplanet.net/region/AF
generic_file_name <- "WDPA_WDOECM_Jun2024_Public_AF_shp-"
poly_file_name <- paste0(generic_file_name, "polygons.shp")
points_file_name <- paste0(generic_file_name, "points.shp")

wdpa_sf_poly <- rbind(
  read_sf(paste0(raw_dir, "/wdpa/AF1/", poly_file_name)),
  read_sf(paste0(raw_dir, "/wdpa/AF2/", poly_file_name)),
  read_sf(paste0(raw_dir, "/wdpa/AF3/", poly_file_name))
  ) %>% 
  st_transform(crs = afr_crs)

if (include_points == TRUE) {
  wdpa_sf_points <- rbind(
    read_sf(paste0(raw_dir, "/wdpa/AF1/", points_file_name)),
    read_sf(paste0(raw_dir, "/wdpa/AF2/", points_file_name)),
    read_sf(paste0(raw_dir, "/wdpa/AF3/", points_file_name))
    ) %>%
    mutate(
      GIS_AREA = NA, # irrelevant column
      GIS_M_AREA = NA, # same
      buffer_radius_m = sqrt(REP_AREA * 1e6 / pi), # convert to meters
    ) %>% 
    st_transform(crs = afr_crs) %>% 
    # buffer each row individually 
    rowwise() %>%
    mutate(geometry = st_buffer(geometry, dist = buffer_radius_m)) %>%
    ungroup() %>% 
    select(-buffer_radius_m)
  
  wdpa_sf <- rbind(wdpa_sf_poly, wdpa_sf_points)
} else {
  wdpa_sf <- wdpa_sf_poly
}

wdpa_sf <- wdpa_sf %>% 
  #distinct(across(-WDPA_PID), .keep_all = TRUE) %>%  
  ## if all columns except WDPAID are identical then it's a duplicate
  ## in such a case, remove one of them
  ## but in practice no such instance, so just a long and unnecessary function
  filter(
    !NAME %in% c(
      # islands that are not relevant (not in grid) but can be removed...  
      "Saint Helena Island Marine Protected Area",
      "Ascension Island Marine Protected Area",
      "Tristan da Cunha",
      "Gough and Inaccessible Islands"
      # "Prince Edward Island Marine Protected Area" # part of South Africa
      # this is in the data, but makes the plot hard to read so remove for plots
    ) # 
    &
    !ISO3 %in% c(
      "REU", # Reunion, part of France
      "MYT", # Mayotte, part of France
      "SHN" # Saint Helena, part of the UK
      )
    &
    STATUS == "Designated" # only keep PAs with this status!  
  ) 

## expand grid WDPA -----
# each protected area will have rows for its years of existence
# i.e. it a protected area was established in 2010
# it won't have a row in 2009 

### IMPORTANT if any changes are made
### if more columns are added to wdpa_sf_speccol and needs to be integrated into 
### expanded_wdpa_sf, they need to be specified in the function expand_years
expand_years <- function(row, last_year) {
  first_year <- max(1997, min(last_year, row$STATUS_YR))
  years <- seq(first_year, last_year)
  
  expanded_rows <- expand_grid(
    WDPAID = row$WDPAID,
    STATUS_YR = row$STATUS_YR,
    geometry = row$geometry,
    DESIG_ENG = row$DESIG_ENG,
    GOV_TYPE = row$GOV_TYPE,
    OWN_TYPE = row$OWN_TYPE,
    IUCN_CAT = row$IUCN_CAT,
    year = years
  )
  return(expanded_rows)
}

wdpa_sf_speccol <- wdpa_sf %>% 
  select(NAME, WDPAID, STATUS_YR, DESIG_ENG, GOV_TYPE, IUCN_CAT, OWN_TYPE, geometry) 

expanded_wdpa_sf <- wdpa_sf_speccol %>%
  mutate(row_id = row_number()) %>%
  split(.$row_id) %>%
  map_dfr(~expand_years(.x, last_yr_of_analysis)) %>% 
  st_as_sf(crs = afr_crs)  # should already have this crs 

rm(wdpa_sf_speccol)

## merge the data with grid ----
grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid, area_grid)
grid_expanded_years <- fread(here(int_dir, "grid_years.csv")) %>% 
  select(gid, year)

intersected_grid_wdpa_expanded <- st_intersection(
  grid,
  expanded_wdpa_sf
  ) %>% 
  st_make_valid()

rm(expanded_wdpa_sf)
# assumes they are constant
# to my understanding it is not an issue
# because each area applies for all polygon
# https://cran.r-project.org/web/packages/sf/vignettes/sf1.html#how-attributes-relate-to-geometries

### merge into grid - union (slow) ------

# some gid have more than one WDPAID so need to union these areas
# because there are different wdpaid in the same area sometimes, cannot simply be 
# separated into cases and summed
# needs to be done like that and after that without grouping by gov type

### by diff types - old version, its slow and we're not focusing on wdpa so skipping
# # by gov type
# needs_union_gov <- intersected_grid_wdpa_expanded %>%
#   group_by(gid, year, GOV_TYPE) %>%
#   summarize(
#     area_grid = min(area_grid),
#     geom = st_union(geom),
#     .groups = 'drop'
#   ) %>% 
#   mutate(
#     area_intersect = as.numeric(st_area(geom)),
#     share_area_protected = area_intersect / area_grid
#   )
# 
# # by iucn categories
# needs_union_iucn <- intersected_grid_wdpa_expanded %>%
#   group_by(gid, year, IUCN_CAT) %>%
#   summarize(
#     area_grid = min(area_grid), # keep one value - min is unnecessary (should be identical with max/avg) but to be safe
#     geom = st_union(st_combine(geom)), # really have. combine cannot be used because of combination of intersects and separate combos
#     .groups = 'drop'
#   ) %>% 
#   mutate(
#     area_intersect = as.numeric(st_area(geom)),
#     share_area_protected = area_intersect / area_grid
#   )
# 
# the regular data is ready; needs to turn the convert the gov/iucn data into columns
# process_data <- function(df, group_var) {
#   # summarize the data  
#   summarized_data <- df %>%
#     st_set_geometry(NULL) %>% # geometry is unnecessary here for the summary
#     group_by(gid, year, !!sym(group_var)) %>%
#     summarise(
#       area_intersect = max(area_intersect),
#       share_area_protected = max(share_area_protected),
#       .groups = 'drop'
#     )
#   
#   # pivot the data to have each unique group_var (i.e. iucn_cat) as a separate column
#   long_summarized_data <- summarized_data %>%
#     pivot_wider(
#       names_from = !!sym(group_var),
#       values_from = share_area_protected,
#       names_prefix = paste0("share_area_", group_var, "_")
#     ) %>% 
#     mutate(
#       across(everything(), ~ replace_na(., 0))
#     ) %>% 
#     group_by(gid, year) %>% 
#     ungroup() %>% 
#     select(-area_intersect)
#   
#   # get the shares and keep only the relevant columns (shares, gid, year)
#   processed_data <- long_summarized_data %>% 
#     group_by(year, gid) %>% 
#     mutate(across(starts_with("share_area"), max)) %>% 
#     select(year, gid, starts_with("share_area")) %>% 
#     distinct()
#   
#   return(processed_data)
# }

needs_union_all <- intersected_grid_wdpa_expanded %>%
  group_by(gid, year) %>%
  summarize(
    area_grid = min(area_grid),
    geom = st_union(geom),
    .groups = 'drop'
  ) %>% 
  mutate(
    area_intersect = as.numeric(st_area(geom)),
    share_area_protected = area_intersect / area_grid
  )

# 
# # apply the functions
# gov_data_with_cols <- process_data(needs_union_gov, "GOV_TYPE")
# iucn_data_with_cols <- process_data(needs_union_iucn, "IUCN_CAT")
# 
# combine the data
final_wdpa <- needs_union_all %>% 
  st_drop_geometry() 
  # left_join(gov_data_with_cols, by = c("year", "gid")) %>% 
  # left_join(iucn_data_with_cols, by = c("year", "gid"))

### merge into grid - final ----
grid_wdpa_full <- left_join(
  grid_expanded_years, final_wdpa, 
  by = c("gid", "year")) %>% 
  replace(is.na(.), 0) %>% 
  select(gid, year, share_area_protected)

rm(needs_union_all, needs_union_gov, grid_expanded_years, grid, gov_data_with_cols, 
   long_summarized_data_gov, summarized_data_gov, final_wdpa, gov_data_with_cols, 
   iucn_data_with_cols, needs_union_iucn)

fwrite(grid_wdpa_full, here(int_dir, "grid_wdpa.csv"), append = FALSE) 
