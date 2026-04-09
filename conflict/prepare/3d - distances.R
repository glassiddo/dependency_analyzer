#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   Compute distances to capital, large cities, coasts, ports and airports
#* Note:    TBF
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

ctries_grid <- fread(here(int_dir, "country_climatezones.csv")) %>% 
  select(gid, country, iso_a3) 

grid <- st_read(here(int_dir, "grid.gpkg")) %>% 
  select(gid) %>% 
  left_join(
    ctries_grid, by = "gid"
  )

countries_africa <- unique(grid$country)

africa_bbox <- st_bbox(c(xmin = -25, xmax = 55, ymin = -40, ymax = 40))

africa_iso2 <- ne_countries(
    continent = "Africa", scale = "medium", returnclass = "sf"
  ) %>% 
  st_drop_geometry() %>% 
  select(name, country_cod = iso_a2) 

### capitals and coastline ----
capitals_africa <- ne_download(
  scale = "medium", type = "populated_places",  # get capitals
  category = "cultural", returnclass = "sf") %>% 
  st_transform(st_crs(afr_crs)) %>%
  filter(
    (ADM0NAME %in% countries_africa | 
      grepl("africa", TIMEZONE, ignore.case = TRUE) |
      ADM0NAME %in% c("Cape Verde", "Sao Tome and Principe") 
     ) & 
      FEATURECLA == "Admin-0 capital"
  ) %>% 
  rename(Country = ADM0NAME, Capital = NAME) %>% 
  select(Capital, Country) %>% 
  mutate(
    Country = case_when( # FIX NAMES
      Country == "South Sudan" ~ "S. Sudan",
      Country == "Congo (Brazzaville)" ~ "Congo", 
      Country == "Congo (Kinshasa)" ~ "Dem. Rep. Congo",
      Country == "The Gambia" ~ "Gambia",
      Country == "Ivory Coast" ~ "Côte d'Ivoire", 
      Country == "Equatorial Guinea" ~ "Eq. Guinea",
      Country == "Central African Republic" ~ "Central African Rep.",
      Country == "Guinea Bissau" ~ "Guinea-Bissau",
      Country == "Sao Tome and Principe" ~ "São Tomé and Principe",
      Country == "Cape Verde" ~ "Cabo Verde",
      TRUE ~ Country
    )
  ) %>% 
  filter(
    !Capital %in% c("Bloemfontein", "Johannesburg", "Cape Town", "Abidjan")
    ) %>% # only keep pretoria as SA capital and Yamoussoukro as CIV
  rename(country = Country, capital = Capital) 

coastline <- ne_download(
    scale = "medium", type = "coastline", 
    category = "physical", returnclass = "sf"
  ) %>%
  st_crop(africa_bbox) %>% 
  st_transform(st_crs(afr_crs))

compute_capital_distances <- function(grid, capitals) {
  grid_with_capitals <- grid %>%
    left_join(st_drop_geometry(capitals), by = "country") %>%
    rowwise() %>%
    mutate(
      distance_to_capital = ifelse(
        !is.na(capital), 
        as.numeric(st_distance(geom, capitals$geometry[capitals$country == country])), 
        NA_real_
      )
    ) %>%
    ungroup()
  
  return(grid_with_capitals)
}

grid_distances <- compute_capital_distances(grid, capitals_africa) %>% 
  st_drop_geometry() %>% 
  mutate(
    distance_to_capital = ifelse(
      is.na(distance_to_capital), 0, distance_to_capital)
  ) %>% 
  select(gid, distance_to_capital)

grid <- grid %>% 
  left_join(grid_distances, by = "gid") %>% 
# compute distance to nearest coastline
  mutate(
    distance_to_coast = st_distance(geom, coastline) %>% apply(1, min)
  ) 
  

### airports -----

### this reads two files. one which is based on openstreetmap, 
### and another based on user uploads to a website called ourairports
airports <- read_sf(here(raw_dir, "aiports", "osm-world-airports.shp")) 
# from https://data.opendatasoft.com/explore/dataset/osm-world-airports@babel/export/

airports_afr <- airports %>% 
  st_crop(africa_bbox) %>% 
  left_join(africa_iso2, by = "country_cod") %>% 
  filter(!is.na(name.y)) %>% 
  select(name_en, iata, country, country_cod)


aiport_other_source <- fread(here(raw_dir, "aiports", "airport-codes.csv")) %>%
  # downloaded from https://github.com/datasets/airport-codes/blob/main/data/airport-codes.csv
  # source is https://ourairports.com/data/
  filter(
    continent == "AF",
    iata_code != "",
    !iso_country %in% c("ES", "RE", "YT"), # exclude spain and french territories
    ( # one of three conditions - large airport; medium with "international"; 
      # or specificly mentioned airports that are medium and w/o/ int but are relevant
      # less interested in small ones. mediums can also be excluded - maybe for robustness
      type == "large_airport" |
      (
        type == "medium_airport" & 
        grepl("international", name, ignore.case = TRUE) # only if has international in name)
       ) |
      name %in% c(
        "Maputo International Airport", "Beira Airport", # mozambique
        "Nampula Airport", "Pemba Airport", # more mozambique 
        "Cadjehoun Airport", # benin
        "Kumasi Airport", "Tamale Airport" # ghana - both international, tamale was upgraded in 2009...
      )
    ) 
  ) %>% 
  separate(coordinates, into = c("lat", "long"), sep = ", ", convert = TRUE) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  rename(iata = iata_code) 

# keep ones existing in the ourairports dataset only
iata_diff <- setdiff(
  aiport_other_source$iata, 
  airports_afr$iata
) 

additionals_from_other_source <- aiport_other_source %>%
  filter(iata %in% iata_diff) %>%
  select(iata, name, iso_country) %>%
  rename(country_cod = iso_country, name_en = name)

# merge both
all_airports <- bind_rows(airports_afr %>% select(-country), additionals_from_other_source) %>% 
  filter(!is.na(iata)) %>%  # either inactive airport or unknown
  mutate(
    name_en = case_when(
      iata == "DAR" ~ "Julius Nyerere International Airport", 
      iata == "ABJ" ~ "Félix-Houphouët-Boigny International Airport",
      iata == "BOY" ~ "Bobo Dioulasso Airport", # small airport, only int flights to civ
      iata == "KIS" ~ "Kisumu International Airport", # small, only int to uganda
      iata == "ZNZ" ~ "Abeid Amani Karume International Airport", # zanzibar
      iata == "PTG" ~ "Polokwane International Airport", # not actually international
      iata == "SKO" ~ "Sadiq Abubakar III International Airport", # not international
      iata == "JRO" ~ "Kilimanjaro International Airport", 
      iata == "LVI" ~ "Harry Mwaanga Nkumbula International Airport",
      iata == "LLW" ~ "Kamuzu International Airport", # lilongwe, international
      iata == "HLA" ~ "Lanseria International Airport", # no int flights yet and new
      iata == "NTY" ~ "Pilanesberg International Airport", # essentially serving a national park but doesn't seem active
      iata == "MWZ" ~ "Mwanza Airport",
      iata == "MIU" ~ "Maiduguri International Airport", # no international flights
      iata == "BUQ" ~ "Joshua Mqabuko Nkomo International Airport",
      iata == "VFA" ~ "Victoria Falls Airport",
      iata == "BLZ" ~ "Chileka International Airport",
      iata == "QUO" ~ "Akwa Ibom Airport", # no international flights
      # mainly did it to see if the missing values are small airports or not -
      # seems like a mixed bag
    TRUE ~ name_en
    )
  ) %>% 
  filter(
    !iata %in% c("NTY", "MIU", "QUO", "UAL", "NBJ", "HLA") 
    # nty, ual, miu, quo dont seem to have int/any flights (there are more like that)
    # hla and nbj are new so irrelevant for our period of analysis
  ) %>% 
  st_transform(st_crs(afr_crs)) 

rm(additionals_from_other_source, airports, airports_afr, aiport_other_source)

# distance to nearest airport anywhere
grid <- grid %>%
  mutate(
    distance_to_airport = st_distance(geom, all_airports) %>% apply(1, min)
    ) %>% 
  left_join(africa_iso2 %>% rename(country = name), by = "country")

# distance to nearest airport in the country
calculate_nearest_airport_distance <- function(grid_data, airport_data) {
  # Initialize a vector to store distances
  distances <- numeric(nrow(grid_data))
  
  # Loop through each country code
  unique_countries <- unique(grid_data$country_cod)
  
  for (country in unique_countries) {
    # Filter grid cells and airports for current country
    country_grid <- grid_data[grid_data$country_cod == country, ]
    country_airports <- airport_data[airport_data$country_cod == country, ]
    
    if (nrow(country_airports) > 0) {
      # Calculate distance matrix between grid cells and airports
      dist_matrix <- st_distance(country_grid, country_airports)
      
      # Find minimum distance for each grid cell
      min_distances <- apply(dist_matrix, 1, min)
      
      # Store distances in the main vector
      distances[grid_data$country_cod == country] <- min_distances
    } else {
      # If no airports in country, set distance to NA
      distances[grid_data$country_cod == country] <- NA
    }
  }
  
  # Add distances to the original grid data
  grid_data$distance_to_airport_country <- distances
  
  return(grid_data)
}

grid <- calculate_nearest_airport_distance(grid, all_airports) %>% 
  mutate(
    # convert all to km
    distance_to_airport_country = as.numeric(distance_to_airport_country) / 1000,
    distance_to_airport = as.numeric(distance_to_airport) / 1000,
    distance_to_capital = as.numeric(distance_to_capital) / 1000,
    distance_to_coast = as.numeric(distance_to_coast) / 1000
    ) %>% 
  select(-country_cod)

#### distances to cities and ports -----
# uses geodata package
# set a path for the tif files
my_geodata_path <- here(int_dir, "distances to cities")

# Get the travel time raster
travel_time_5mcity <- travel_time(to="city", size=1, up=FALSE, path=my_geodata_path) 
travel_time_1mcity <- travel_time(to="city", size=3, up=TRUE, path=my_geodata_path)
travel_time_port <- travel_time(to="port", size=1, up=FALSE, path=my_geodata_path)
# documentation in the package. 
# up = true -> the size chosen or larger than this (so size 1/2, if size = 3)
# size1 port - large port, size1 city - at least 5m, size2 city - at least 1m

# this takes the mean value for the polygon for distance from a given port/city
library(exactextractr)
grid <- grid %>% 
  mutate(
    distance_to_min5m_city = exact_extract(travel_time_5mcity, st_geometry(.), 'mean'),
    distance_to_min1m_city = exact_extract(travel_time_1mcity, st_geometry(.), 'mean'),
    distance_to_port = exact_extract(travel_time_port, st_geometry(.), 'mean')
  )

#### export and map ----
grid_for_export <- grid %>% 
  select(-country, -iso_a3) %>% 
  st_set_geometry(NULL)

write_csv(
  grid_for_export, 
  here(int_dir, "distances_capital_coast.csv"), 
  append = FALSE
  )
