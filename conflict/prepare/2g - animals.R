#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass and Arnauld 
#* Date:    October 2025 (mostly done in summer 2024)
#* Title:   3 - Process population data
#* Note:    TBF  
#* TODO - code is a bit of a mess
#*******************************************************************************

source("do/conf_biodiversity_setup.R")

area_threshold <- 0.01 # the minimal share of area of the cell 
# that should be covered by the 'territory' of the animal

### brief explanation about the code
### when data about an animal is downloaded, it generates the same naming of files
### so I just extracted each animal to a diff folder with a number, so to add another animal
### create a new folder called "16" or whatever the highest number + 1 is already there
### the name of the animal will be loaded in the data after that

number_of_animals <- 16 ## add more if downloading more

# Create a list to store all animal layers
data_list <- list()

# Loop through folders 1 to number_of_animals, each with a different animal 
for (i in 1:number_of_animals) {
  if (i %in% c(3, 4)) { ## old rhino files - skip them
    next
  }
  
  data <- st_read(here(raw_dir, "animals", i, "/data_0.shp"), quiet = TRUE)
  
  # Handle special case for Rhino (folder 16)
  if (i == 16) {
    # Create SCI_NAME and LEGEND columns manually
    data$SCI_NAME <- "Rhinoceros spp."
    data$LEGEND <- "Extant (resident)"
    data$SEASONAL <- 1
  }
  
  animal_name <- first(data$SCI_NAME)
  print(paste("Animal", i, ":", animal_name))
  
  data <- data %>% select(SCI_NAME, LEGEND, SEASONAL, geometry) 
  
  data_list[[i]] <- data
}

# Merge all species into one dataset
animals_data <- do.call(rbind, data_list) %>% 
  filter(LEGEND == "Extant (resident)", SEASONAL == 1) %>%
  # filter to keep only extant (resident) presence and non seasonal
  select(SCI_NAME) %>% 
  st_transform(st_crs(afr_crs))

rm(data, data_list)

grid <- read_sf(here(int_dir, "grid.gpkg")) %>% 
  select(gid, area_grid) 

gridded_animals <- st_intersection(grid, animals_data) %>% 
  mutate(
    area_intersect = as.numeric(st_area(geom)),
    share_area_animal = area_intersect / area_grid,
    has_animal = ifelse(
      share_area_animal > area_threshold, 1, 0
      ) # if at least 1% has the animal
  ) %>% 
  st_drop_geometry() %>% 
  filter(has_animal == 1) %>% 
  select(gid, SCI_NAME) %>% 
  group_by(gid, SCI_NAME) %>%
  summarise(value = 1, .groups = 'drop') %>% 
  select(gid, SCI_NAME) %>%
  mutate(value = 1) %>%  # Create a value column to use for pivoting
  pivot_wider(
    names_from = SCI_NAME, values_from = value, values_fill = list(value = NA)
    ) %>%
  # Replace NA with 0 in all columns except 'gid'
  mutate(across(-gid, ~replace_na(., 0)))
  
grid_with_animals <- grid %>% # to ensure all grids exist, including ones with 0
  st_drop_geometry() %>% 
  left_join(gridded_animals, by = "gid") %>%
  replace(is.na(.), 0) %>% 
  select(-area_grid) %>% 
  rename(
    "Leopard" = "Panthera pardus",
    "Lion" = "Panthera leo",
    "Rhino" = "Rhinoceros spp.",
    "African Elephant" = "Loxodonta africana",
    "Forest Elephant" = "Loxodonta cyclotis",
    "Cape Buffalo" = "Syncerus caffer",
    "Mountain Gorilla" = "Gorilla beringei",
    "Hippo" = "Hippopotamus amphibius",
    "Giraffe" = "Giraffa camelopardalis", 
    "Plains zebra" = "Equus quagga",
    "Mountain zebra" = "Equus zebra",
    "Grévy's zebra" = "Equus grevyi",
    "Cheeta" = "Acinonyx jubatus",
  ) %>% 
  mutate(
    #Rhino = ifelse((`White Rhino` ==1 | `Black Rhino` == 1), 1, 0), 
    Elephant = ifelse((`African Elephant` == 1 | `Forest Elephant` == 1), 1, 0),
    Zebra = ifelse((`Plains zebra` == 1 | `Mountain zebra` == 1 | `Grévy's zebra` == 1), 1, 0),
    num_big_game = Leopard + Lion + `Cape Buffalo` + Elephant + Rhino 
  )

write_csv(grid_with_animals, file.path(int_dir, "animals2.csv"), append = FALSE)

rm(grid, grid_with_animals_v1, gridded_animals, animals_data, grid_with_animals_for_export)
