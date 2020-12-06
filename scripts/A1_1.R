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

###########
###TASK1###
###########

# 6.0 Business Insights ----
# 6.1 Sales by State ---
salesPerState_tbl <- bikeSaleLocation_tbl_wrangeled %>%
  select(total.price,state) %>%
  group_by(state) %>%
  summarize(sales = sum(total.price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
#glimpse(salesPerState_tbl)

# Step 2 - Visualize
salesPerState_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = state, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    subtitle = "A nice to have information",
    x = "", # Override defaults for x and y
    y = "Revenue"
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))