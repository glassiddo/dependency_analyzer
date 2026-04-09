#* Project: Conflict and biodiversity in Africa
#* Author:  Iddo Glass
#* Date:    October 2025
#* Title:   Read UN tourism yearbooks and convert to csv
#* Note: Takes a lot of time. It is independent of the other files
#*******************************************************************************

# function to process a given tourism PDF -----
process_tourism_pdf <- function(
    pdf_file, years, 
    #country_area, 
    country_stats_area, 
    table_area, trim_whitespace = FALSE
) {
  
  # get the number of pages in the PDF
  num_pages <- get_n_pages(pdf_file)
  
  ### extract stuff for cleaning
  stats_types <- extract_tables(
    file = pdf_file,
    area = country_stats_area,
    pages = 1:num_pages,
    method = "lattice",
    guess = FALSE
  ) %>%
    unlist()
  
  ## unneccesary but keep for now commented out
  # countries <- extract_tables( # meant just to extract the country names
  #   file = pdf_file,
  #   area = country_area,
  #   pages = 1:num_pages,
  #   method = "stream",
  #   guess = FALSE
  # ) %>% 
  #   unlist() %>% 
  #   unique()
  
  df_stats <- data.frame(
    destination = names(stats_types),
    category = as.character(stats_types),
    stringsAsFactors = FALSE
  ) %>%
    filter(
      !destination %in% c("...21", "...22", "...23"),
      !category %in% c(
        "nationality", "residence", "by nationality", "of residence",
        "country of residence", "by country of residence"
      ),
      !is.na(category)
    ) %>%
    mutate(destination = str_remove(destination, "\\d+$")) %>%
    unique() %>%
    separate(category, into = c("type", "meaning"), sep = "\\.", extra = "merge")
  
  # cleaning for 2000-04
  if (trim_whitespace) {
    df_stats <- df_stats %>%
      mutate(across(c(type, meaning), trimws)) %>% 
      unique()
  }
  
  ### loop through pages to get the data 
  all_pages_data <- list()
  last_known_year_indices <- NULL
  year_pattern <- paste(years, collapse = "|") # Dynamic pattern for grepl
  
  for (i in 1:num_pages) {
    print(paste("Processing Page:", i, "of", pdf_file))
    
    page_table <- tryCatch({
      extract_tables(
        file = pdf_file,
        pages = i,
        area = table_area,
        guess = FALSE,
        method = "stream"
      )[[1]] %>% as.data.frame()
    }, error = function(e) {
      return(data.frame())
    })
    
    if (nrow(page_table) == 0 || ncol(page_table) == 0) next
    
    # search the first few rows for a header containing years
    header_row_index <- 0
    for (r in 1:min(nrow(page_table), 5)) {
      row_content <- as.character(page_table[r, ])
      if (any(grepl(year_pattern, row_content))) {
        header_row_index <- r
        # find indices for all years
        indices <- sapply(as.character(years), 
                          function(y) which(row_content == y)[1], USE.NAMES = FALSE)
        
        if (!any(is.na(indices))) {
          last_known_year_indices <- indices
        }
        break
      }
    }
    
    data_to_clean <- if (header_row_index > 0) page_table %>% 
      slice((header_row_index + 1):n()) else page_table
    
    if (is.null(last_known_year_indices)) next
    
    final_col_indices <- c(1, last_known_year_indices)
    
    # ensure indices are within the bounds of the current table
    if (max(final_col_indices, na.rm = TRUE) > ncol(data_to_clean)) next
    
    # clean the extracted table using the dynamic indices
    # each indice refers to each year, works as there are 5 years in each file
    cleaned_table <- data_to_clean %>%
      select(all_of(final_col_indices)) %>%
      set_names(c("origin", as.character(years))) %>%
      mutate(across(everything(), as.character)) %>%
      filter(
        !is.na(origin), origin != "",
        !str_detect(origin, "Source")
      ) %>%
      mutate(across(all_of(as.character(years)), ~ as.numeric(gsub("[^0-9.-]", "", .)))) %>%
      filter(!if_all(all_of(as.character(years)), is.na))
    
    if (nrow(cleaned_table) > 0) {
      all_pages_data[[i]] <- cleaned_table
    }
  }
  
  # --- combine ---
  data_extracted_df <- bind_rows(all_pages_data)
  
  ## there is information of visitors from country groups etc
  ## some of these are aggregates so not important
  ## perhaps we would be intersted in "other countries of..." as some countries 
  ## that exist in some years may be represented there in other years
  # non_countries <- c(
  #   "AFRICA", "Central Africa", "North Africa", "Southern Africa", "West Africa",
  #   "East Africa", "Other Africa", "Other countries of Africa", "AMERICAS",
  #   "Central America", "North America", "South America", "Other Americas",
  #   "Other countries of the Americas", "EAST ASIA AND THE PACIFIC", "SOUTH ASIA",
  #   "North-East Asia", "South-East Asia", "Australasia", "Caribbean",
  #   "Other East Asia and the Pacific", "Other countries of Asia",
  #   "EUROPE", "Central/Eastern Europe", "Northern Europe", "Southern Europe",
  #   "Western Europe", "East Mediterranean Europe", "Other Europe",
  #   "MIDDLE EAST", "REGION NOT SPECIFIED", "South Asia", "Not Specified",
  #   "Other countries of the Caribbean", "Other countries of Europe",
  #   "All countries of North Africa", "Other countries Central/East Europe",
  #   "All countries of Central America", "Other countries of North America",
  #   "Other countries of South America", "Other countries of Oceania",
  #   "Middle East", "All countries of Oceania", 
  #   "Other countries of Central Africa", "Other countries of Southern Africa",
  #   "Other countries of North-East Asia",
  #   "Other countries Central/East Europ", "Other countries of West Africa",
  #   "Other countries of East Africa", "All countries of the Caribbean",
  #   "MIDDLE EAST", "Other countries of Middle East",
  #   "OTHER EUROPE", "NORTH AMERICA", "SOUTHERN EUROPE", "NORTHERN EUROPE", 
  #   "EAST ASIA AND THE PACIFIC", "SOUTH AMERICA", "CARIBBEAN", "AMERICAS",
  #   "NOT SPECIFIED", "Other countries of the World", 
  #   "Nationals Residing Abroad" # most important to remove
  # )
  
  # countries_only <- data_extracted_df %>%
  #   filter(!origin %in% non_countries)
  
  ## assign the visited country and data type by matching with the "TOTAL" rows
  ## cause everytime a new country/statistic combo starts, there is TOTAL 
  ## the extraction of the country/statistic and values is separate 
  ## as its more accurate this way
  stats_index <- 1
  data_extracted_df$destination <- NA
  data_extracted_df$type <- NA
  
  for (i in 1:nrow(data_extracted_df)) {
    if (data_extracted_df$origin[i] == "TOTAL") {
      if (stats_index <= nrow(df_stats)) {
        data_extracted_df$destination[i] <- df_stats$destination[stats_index]
        data_extracted_df$type[i] <- df_stats$type[stats_index]
        stats_index <- stats_index + 1
      }
    }
  }
  
  ## fill the host country and type information downwards
  data_extracted_df <- data_extracted_df %>%
    fill(destination, type, .direction = "down") %>% # not sure this should exist
    filter(
      # origin != "TOTAL", actually need it to compute the shares
      ## type should be 1/2; 3 and higher refer to hotel visits
      type %in% c("1", "2") 
    )
  
  # take type '2' when a country has both types '1' and '2' (usually more info)
  countries_with_both_types <- data_extracted_df %>%
    distinct(destination, type) %>%
    count(destination) %>%
    filter(n > 1) %>%
    pull(destination)
  
  result_df <- data_extracted_df %>%
    mutate(
      is_duplicate_type1 = ifelse(
        destination %in% countries_with_both_types & type == "1",
        TRUE, FALSE
      )
    ) %>%
    filter(!is_duplicate_type1) %>%
    mutate(destination = str_to_title(destination)) %>% # make names compatible 
    select(
      destination,
      origin,
      all_of(as.character(years))
    )
  
  return(result_df)
}


# execution ===============================================
### necessary to determine the areas
# locate_areas(pdf_file, pages = 11)
# # --- 2010-2014 PDF ---
pdf_file_10_14 <- "data/raw/tourism/UN Tourism yearbooks/Country tables - 2010-14 tourism.pdf"
years_10_14 <- 2010:2014
country_area_10_14 <- list(c(41.3, 56.2, 62.2, 558.8))
country_stats_area_10_14 <- list(c(34.3, 53.4, 87.2, 556.0))
table_area_10_14 <- list(c(95.5, 47.8, 702.3, 571.3))

# --- 2005-2009 PDF ---
pdf_file_05_09 <- "data/raw/tourism/UN Tourism yearbooks/Country tables - 2005-09 tourism.pdf"
years_05_09 <- 2005:2009
# country_area_05_09 <- list(c(29.3, 61.9, 75.0, 531.8))
country_stats_area_05_09 <- list(c(47.6, 61.9, 105.6, 531.7))
table_area_05_09 <- list(c(96.6, 58.9, 813.6, 537.9))

# --- 2000-2004 PDF ---
pdf_file_00_04 <- "data/raw/tourism/UN Tourism yearbooks/Country tables - 2000-04 tourism.pdf"
years_00_04 <- 2000:2004
# country_area_00_04 <- list(c(39.8, 56.4, 72.0, 565.5))
country_stats_area_00_04 <- list(c(44.2, 55.2, 97.0, 553.3))
table_area_00_04 <- list(c(98.3, 52.5, 807.3, 564.2))

df_2010_2014 <- process_tourism_pdf(
  pdf_file = pdf_file_10_14,
  years = years_10_14,
  country_stats_area = country_stats_area_10_14,
  table_area = table_area_10_14
)

df_2005_2009 <- process_tourism_pdf(
  pdf_file = pdf_file_05_09,
  years = years_05_09,
  country_stats_area = country_stats_area_05_09,
  table_area = table_area_05_09
)

df_2000_2004 <- process_tourism_pdf(
  pdf_file = pdf_file_00_04,
  years = years_00_04,
  country_stats_area = country_stats_area_00_04,
  table_area = table_area_00_04,
  trim_whitespace = TRUE # unique cleaning step
)

combined_df <- bind_rows(
  pivot_longer(
    df_2010_2014, 
    cols = all_of(as.character(years_10_14)), 
    names_to = "year", 
    values_to = "visitors"
  ),
  pivot_longer(
    df_2005_2009, 
    cols = all_of(as.character(years_05_09)), 
    names_to = "year", 
    values_to = "visitors"
  ),
  pivot_longer(
    df_2000_2004,
    cols = all_of(as.character(years_00_04)),
    names_to = "year",
    values_to = "visitors"
  )
)

# cleaning names
combined_df <- combined_df %>% 
  mutate(
    origin = case_when(
      origin == "United States of America" ~ "United States",
      TRUE ~ origin
    ),
    destination = case_when(
      destination == "United States Of America" ~ "United States",
      TRUE ~ destination
    )
  )

fwrite(combined_df, here(int_dir, "temp_tourism.csv"))
