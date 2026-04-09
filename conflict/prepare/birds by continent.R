rm(list=ls())
library(duckdb)
library(dplyr)
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

setwd("C:/Users/iddo2/Dropbox/eBird data")

## just for the africa file:
setwd("C:/Users/iddo2/Nextcloud2/conflict and protected areas/data/raw/ebird and inaturalist/birddata")

file <- "ebird_afr.csv" 

## all files were downloaded from https://www.gbif.org/
## requires a API key
## North America is extremely heavy so need to think how to deal with it

###  Dataset EOD – eBird Observation Dataset 
###  Year Between start of 1991 and end of 2020 
###  Occurrence status present 

###  we try to recognize the main origin country of each observer ('recordedBy')
###  the methodology is to count the number of unique months+days of observations
###  in each country, and attribute their main country to the one where the have
###  the highest number of unique months (and in case of tie, days)
###  the idea is that if, someone is from a given country and sometimes travels
###  even if they rarely use ebird in their origin country and often when traveling
###  the origin country should have more unique months...

###  each continent was run separately
###  because the files are extremely heavy, I ran it separately by continent

duck_tbl <- paste0("read_csv('", file, "', delim='\t', ignore_errors=true, quote='')")

query <- paste0("
WITH daily_activity AS (
  SELECT
    recordedBy,
    countryCode,
    year,
    month,
    day
  FROM ", duck_tbl, "
  GROUP BY recordedBy, countryCode, year, month, day
),
country_stats AS (
  SELECT
    recordedBy,
    countryCode,
    COUNT(DISTINCT year || '-' || month) AS unique_months,
    COUNT(*) AS unique_days
  FROM daily_activity
  GROUP BY recordedBy, countryCode
),
ranked AS (
  SELECT
    recordedBy,
    countryCode AS estCountryOfOrigin,
    unique_months,
    unique_days,
    ROW_NUMBER() OVER (
      PARTITION BY recordedBy 
      ORDER BY unique_months DESC, unique_days DESC, RANDOM()
    ) AS rn
  FROM country_stats
)
SELECT
  recordedBy,
  estCountryOfOrigin,
  unique_months,
  unique_days
FROM ranked
WHERE rn = 1
")
country_of_origin <- dbGetQuery(con, query)
saveRDS(country_of_origin, "origin_africa.rds")
