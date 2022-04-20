# -------------------------------------------------------------------------- #
# Desc: Initial Setup File and Global Variables for Primary Care Mapping App #
# File: global.R                                                             #
# -------------------------------------------------------------------------- #

# Default to the UK Bristol CRAN mirror
chooseCRANmirror(ind=94)

# Install required packages
# -------------------------
dtStart <- Sys.time()
cat('\nLoading libraries...')

# shiny - Web Application Framework for R
if (!require(shiny)) {
  install.packages('shiny')
  library(shiny)
}

# leaflet - Create Interactive Web Maps with the JavaScript 'Leaflet' Library
if (!require(leaflet)) {
  install.packages('leaflet')
  library(leaflet)
}

# ini - Read and Write '.ini' Files
if (!require(ini)) {
  install.packages('ini')
  library(ini)
}

# rgdal - Bindings for the 'Geospatial' Data Abstraction Library
if (!require(rgdal)) {
  install.packages('rgdal')
  library(rgdal)
}

# sf - Simple Features for R
if (!require(sf)) {
  install.packages('sf')
  library(sf)
}

# stringr - Extract and replace substrings from a character vector
if (!require(stringr)) {
  install.packages('stringr')
  library(stringr)
}

# tidyverse - Easily Install and Load the 'Tidyverse'
if (!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse)
}

# readxl - Read Excel Files
if (!require(readxl)) {
  install.packages('readxl')
  library(readxl)
}

# htmltools - Tools for HTML
if (!require(htmltools)) {
  install.packages('htmltools')
  library(htmltools)
}

# DT - A Wrapper of the JavaScript Library 'DataTables'
if (!require(DT)) {
  install.packages('DT')
  library(DT)
}

# htmlwidgets - HTML Widgets for R
if (!require(htmlwidgets)) {
  install.packages('htmlwidgets')
  library(htmlwidgets)
}

# mapview - HTML Widgets for R
if (!require(mapview)) {
  install.packages('mapview')
  library(mapview)
}

dtEnd <- Sys.time()
cat(sprintf('\nCompleted in %.2f seconds', difftime(dtEnd, dtStart, units = 'secs')))

# Read the ini file
# -----------------
ini_file_settings <- ini::read.ini('primary_care_mapping_app.ini')

# -----------------------------

dtStart <- Sys.time()
cat('\nCreating LSOA and OA filter lists...')

# Read in the lsoa to ccg lookup and ccg filter list to create lsoa filter list
lsoa_filter_list <- read.csv(ini_file_settings$lsoa_ccg_lookup$filename) %>%
  select(
    as.integer(
      c(
        ini_file_settings$lsoa_ccg_lookup$lsoa11cd,
        ini_file_settings$lsoa_ccg_lookup$ccg21cd,
        ini_file_settings$lsoa_ccg_lookup$ccg21cdh,
        ini_file_settings$lsoa_ccg_lookup$stp21cd
      )
    )
  ) %>%
  rename_with(.fn = function(x){
    c('lsoa11cd','ccg21cd','ccg21cdh','stp21cd')}) %>%
  filter(ccg21cdh %in% unname(unlist(ini_file_settings$ccg_filter))) %>%
  .$lsoa11cd

oa_filter_list <- read.csv(ini_file_settings$oa_lsoa_lookup$filename) %>%
    select(
      as.integer(
        c(
          ini_file_settings$oa_lsoa_lookup$oa11cd,
          ini_file_settings$oa_lsoa_lookup$lsoa11cd
        )
      )
    ) %>%
  rename_with(.fn = function(x){
    c('oa11cd','lsoa11cd')}) %>%
  filter(lsoa11cd %in% lsoa_filter_list) %>%
  .$oa11cd
  
dtEnd <- Sys.time()
cat(sprintf('\nCompleted in %.2f seconds', difftime(dtEnd, dtStart, units = 'secs')))

# -----------------------------

dtStart <- Sys.time()
cat('\nReading national postcode file (this may take some time)...')

df_postcode_lookup <- read.csv(ini_file_settings$postcode_geography$file) %>%
  select(
    as.integer(
      c(
        ini_file_settings$postcode_geography$postcode,
        ini_file_settings$postcode_geography$easting,
        ini_file_settings$postcode_geography$northing,
        ini_file_settings$postcode_geography$oa11cd,
        ini_file_settings$postcode_geography$lsoa11cd,
        ini_file_settings$postcode_geography$latitude,
        ini_file_settings$postcode_geography$longitude
      )
    )
  ) %>%
  rename_with(.fn = function(x){
    c('postcode', 
      'easting', 'northing', 
      'oa11cd', 'lsoa11cd', 
      'latitude', 'longitude')}) %>%
  filter(lsoa11cd %in% lsoa_filter_list)

dtEnd <- Sys.time()
cat(sprintf('\nCompleted in %.2f seconds', difftime(dtEnd, dtStart, units = 'secs')))

# -----------------------------

dtStart <- Sys.time()
cat('\nCreating footprints...\n')

# Load the source file for the lookup up function
source('create_oa_lsoa_practice_pcn_lookup.R')

# Create the oa to lsoa to practice to pcn footprint
df_oa_lsoa_practice_pcn_lookup <- fnCreateLookup(ini_file_settings)
df_oa_lsoa_practice_pcn_lookup <- df_oa_lsoa_practice_pcn_lookup %>% filter(oa11cd %in% oa_filter_list)

dtEnd <- Sys.time()
cat(sprintf('\nCompleted in %.2f seconds', difftime(dtEnd, dtStart, units = 'secs')))

# -----------------------------

dtStart <- Sys.time()
cat('\nReading country shapefile...\n')

# Read in the Country shapefile and transform to WGS84 coordinate system
sf_country <- st_read(dsn = ini_file_settings$country_shapefile$directory, layer = ini_file_settings$country_shapefile$layer)
sf_country <- st_transform(sf_country, CRS("+proj=longlat +datum=WGS84 +no_defs"))

dtEnd <- Sys.time()
cat(sprintf('\nCompleted in %.2f seconds', difftime(dtEnd, dtStart, units = 'secs')))

# -----------------------------

dtStart <- Sys.time()
cat('\nReading output area shapefile...\n')

# Read in the OA shapefile and transform to WGS84 coordinate system
sf_oa <- st_read(dsn = ini_file_settings$oa_shapefile$directory, layer = ini_file_settings$oa_shapefile$layer) %>%
  filter(OA11CD %in% oa_filter_list)
# st_write(
#   sf_oa %>% filter(OA11CD %in% (df_postcode_lookup %>% distinct(oa11cd) %>% .$oa11cd)),
#   dsn = 'D:\\RWorkspace\\Primary_Care_Mapping_App\\OA11BA', 
#   layer = 'OA11BA',
#   driver = 'ESRI Shapefile'
# )
sf_oa <- st_transform(sf_oa, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Add in the lsoa, practice and pcn code
sf_oa <- sf_oa %>% inner_join(df_oa_lsoa_practice_pcn_lookup, by = c('OA11CD' = 'oa11cd'))

dtEnd <- Sys.time()
cat(sprintf('\nCompleted in %.2f seconds', difftime(dtEnd, dtStart, units = 'secs')))

# -----------------------------

dtStart <- Sys.time()
cat('\nReading lower-layer super output area shapefile...\n')

# Read in the LSOA shapefile and transform to WGS84 coordinate system
sf_lsoa <- st_read(dsn = ini_file_settings$lsoa_shapefile$directory, layer = ini_file_settings$lsoa_shapefile$layer) %>%
  filter(LSOA11CD %in% lsoa_filter_list)
# st_write(
#   sf_lsoa %>% filter(LSOA11CD %in% (df_postcode_lookup %>% distinct(lsoa11cd) %>% .$lsoa11cd)),
#   dsn = 'D:\\RWorkspace\\Primary_Care_Mapping_App\\LSOA11BA', 
#   layer = 'LSOA11BA',
#   driver = 'ESRI Shapefile'
# )
sf_lsoa <- st_transform(sf_lsoa, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Add in the practice and pcn code
sf_lsoa <- sf_lsoa %>% 
  inner_join(df_oa_lsoa_practice_pcn_lookup %>% distinct(lsoa11cd, prac_code, pcn_code), by = c('LSOA11CD' = 'lsoa11cd'))

dtEnd <- Sys.time()
cat(sprintf('\nCompleted in %.2f seconds', difftime(dtEnd, dtStart, units = 'secs')))

# -----------------------------

dtStart <- Sys.time()
cat('\nCreating boundary shapefiles...\n')

# Create practice and pcn boundaries
sf_practice <- sf_lsoa %>% group_by(prac_code) %>% summarise()
sf_pcn <- sf_lsoa %>% group_by(pcn_code) %>% summarise()

dtEnd <- Sys.time()
cat(sprintf('\nCompleted in %.2f seconds', difftime(dtEnd, dtStart, units = 'secs')))

# -----------------------------
