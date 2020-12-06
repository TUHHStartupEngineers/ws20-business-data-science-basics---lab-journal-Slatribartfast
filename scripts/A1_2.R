# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library("writexl")
library(stringr)
library(dplyr)
library(tidyr)

# 2.0 Importing Files ----
bikes_tbl      <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
#glimpse(bikes_tbl)
#glimpse(orderlines_tbl)
#glimpse(bikeshops_tbl)
# 4.0 Joining Data ----
bikeSaleLocation_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
bikeSaleLocation_tbl %>% glimpse()

# 5.0 Wrangling Data ----
bikeSaleLocation_tbl_wrangeled <- bikeSaleLocation_tbl %>%
  separate("location",c("city","state"),", ")  %>%
  mutate(total.price = price * quantity) %>%
  select(-order.line, -gender,-model.year,-frame.material,-weight,-category,-url,-lat,-lng)   

#glimpse(bikeSaleLocation_tbl_wrangeled)


# 6.2 Sales by State and Year

# Step 1 - Manipulate
salesPerStateAndYear_tbl <- bikeSaleLocation_tbl_wrangeled %>%
  select(total.price,state,order.date) %>%
  mutate(year = year(order.date)) %>%
  group_by(year, state) %>%
  summarize(sales = sum(total.price))%>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
#glimpse(salesPerStateAndYear_tbl)

# Step 2 - Visualize
salesPerStateAndYear_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
    subtitle = "nice to see statewise",
    fill = "Main category" # Changes the legend name
  )


# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----
