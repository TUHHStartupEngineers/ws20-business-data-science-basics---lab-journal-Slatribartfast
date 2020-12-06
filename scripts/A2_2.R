# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(furrr)     # Parallel Processing using purrr (iteration)
library(htm2txt)

#1.1 COLLECT PRODUCT FAMILIES ----
#the bike website category to use
url_mtb_enduro       <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb/trail-/-enduro"
xopen(url_mtb_enduro) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
url_mtb_enduro         <- read_html(url_mtb_enduro)
bike_names <- url_mtb_enduro %>%
  html_nodes(".catalog-category-bikes__title")%>%
  str_remove_all(".*>")%>%
  str_remove_all("\n")
bike_prices <- url_mtb_enduro %>%
  html_nodes(".catalog-category-bikes__price-title")%>%
  str_remove_all(".*>")%>%
  str_remove_all("\n")
  
#the phrases found in the tags are used to filter four bikes and prices 

#bike_names
#bike_prices


bike_data_tbl <- tibble()
bike_data_tbl_names <- as_tibble(bike_names)
bike_data_tbl_names <- tibble::rowid_to_column(bike_data_tbl_names, "ID")
bike_data_tbl_names


bike_data_tbl_prices <- as_tibble(bike_prices)
bike_data_tbl_prices <- tibble::rowid_to_column(bike_data_tbl_prices, "ID")
bike_data_tbl_prices



bike_data_tbl <- bike_data_tbl_names %>%
  left_join(bike_data_tbl_prices, by = c("ID" = "ID"))%>%
  rename("bike_name" = "value.x") %>%
  rename("bike_price" = "value.y")


bike_data_tbl
# saving is not so usefull for the compilation on the website, so i leave it out here
#saveRDS(bike_data_tbl, "bike_data_tbl.rds")