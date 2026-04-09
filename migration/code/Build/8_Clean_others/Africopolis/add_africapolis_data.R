# Author:  Iddo Glass (editor, check who wrote originally)
# Date:    Edited on January 19, 2026
# Title:   Add data from africapolis
# Output:  
# Desc:    Returns sample units that intersect with metrpoles from Africapolis
#*******************************************************************************

source("code/SSA_env_SetUp.R")

# load Africapolis data and filter for metropoles in 2015

africapolis_data <- read_sf(here(
  raw.dir, "Africa", "Africapolis", "Africapolis_GIS_2024.gpkg"
  ))

filtered_africapolis <- africapolis_data %>% 
  filter(
    Metropole == "Yes" & 
    Select_Geometry_Year == 2015
  ) %>% 
  select(Agglomeration_Name)  

# load sample units and transform to africapolis crs
# then perform intersection, returning units that intersect with a metropole
sample_units <- read_sf(here(
  build.dir, "Africa", "Maps", "SampleUnits.shp"
)) 
  
units_inters <- sample_units %>% 
  st_transform(st_crs(africapolis_data)) %>% 
  st_intersection(filtered_africapolis)

# there shouldn't be a single ipums_id with more than one agglomeration
# return a warning if there are
dup_check <- units_inters %>%
  st_drop_geometry() %>%
  select(ipums_id, Agglomeration_Name) %>%
  distinct() %>%
  count(ipums_id) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  warning(
    "Some ipums_id intersect with >1 Africapolis agglomeration",
  )
}

# Get all ipums_ids along with country
final_table <- sample_units %>%
  st_drop_geometry() %>%
  left_join(units_inters, by = c("ipums_id", "country")) %>% 
  transmute(
    ipums_id,
    country,
    # clean a unit where kigali is intersected with uganda
    Agglomeration_Name = ifelse(Agglomeration_Name == "Kigali" & country == "UGA",
                                     "", Agglomeration_Name),
    country_name = sapply(country, n_ctry)
    )

# Save the result if needed
write_dta(final_table, here(
  build.dir, "Africa", "Africapolis", "ipums_africapolis_intersection.dta"
  ))