source("code/SSA_env_SetUp.R")
# 1. Read the shapefile
total_adm2 <- st_read("./data/Build/Africa/Maps/SampleUnits.shp")

# 2. Read the Stata file
africapolis_data <- read_dta("./data/Build/Africa/Africapolis/ipums_africapolis_intersection.dta")

# Merge the datasets - join first before modifying any IDs
total_adm2_merged <- left_join(total_adm2, africapolis_data, by = "ipums_id")

# Ensure the country column from total_adm2 is carried into total_adm2_merged
total_adm2_merged$country <- total_adm2$country

# Check Agglomeration_Name field before modification
print("Sample of Agglomeration_Name in merged data:")
print(head(total_adm2_merged$Agglomeration_Name, 10))

# Count non-empty values
non_empty_count <- sum(!is.na(total_adm2_merged$Agglomeration_Name) & 
                         total_adm2_merged$Agglomeration_Name != "")
print("Non-empty Agglomeration_Name values:")
print(non_empty_count)

# Fix mistake: remove Agglomeration_Name where it's "Kigali" in Uganda
total_adm2_merged <- total_adm2_merged %>%
  mutate(Agglomeration_Name = ifelse(
    Agglomeration_Name == "Kigali" & country == "UGA",
    "",
    Agglomeration_Name
  ))

# Now update ipums_id in the merged dataset where Agglomeration_Name is non-empty
total_adm2_merged$ipums_id_original <- total_adm2_merged$ipums_id
total_adm2_merged$ipums_id <- ifelse(
  !is.na(total_adm2_merged$Agglomeration_Name) & total_adm2_merged$Agglomeration_Name != "",
  paste0("00", total_adm2_merged$Agglomeration_Name),
  total_adm2_merged$ipums_id
)

# Check if any values were changed
changed <- sum(total_adm2_merged$ipums_id != total_adm2_merged$ipums_id_original, na.rm = TRUE)
print("Number of ipums_id values changed:")
print(changed)


# Clean geometries before dissolving (fix invalid geometries)
total_adm2_merged_cleaned <- total_adm2_merged %>%
  st_make_valid()

# Dissolve geometries based on ipums_id and keep country
total_adm2_merged_dissolved <- total_adm2_merged_cleaned %>%
  group_by(ipums_id) %>%
  summarize(
    geometry = st_union(geometry),
    Agglomeration_Name = first(Agglomeration_Name), 
    country = first(country),  # <-- Keep the country!
    .groups = "drop"
  ) %>%
  st_as_sf()
# Save as GeoPackage
st_write(total_adm2_merged_dissolved, 
         "./data/Build/Africa/Maps/SampleUnits_w_africapolis.gpkg", 
         delete_dsn = TRUE)

print("Completed successfully!")