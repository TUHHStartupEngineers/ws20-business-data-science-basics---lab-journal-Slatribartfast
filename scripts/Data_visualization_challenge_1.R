# Chapter 05 - Challenge 1 ----

# import libraries ----

library(tidyverse)
library(lubridate)

# import data ----

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
#make it more nice
covid_data_tbl <- covid_data_tbl%>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " "))


# assamble stuff
assambled_tbl <- covid_data_tbl %>%
  mutate(date = dmy(dateRep)) %>%
  arrange(date) %>%
  filter(countriesAndTerritories %in% c("Germany","Spain", "France", "United Kingdom", "United States of America")) %>%
  filter(year == "2020") %>%
  group_by(countriesAndTerritories) %>%
  mutate(cumulative_cases = cumsum(cases)) %>%
  ungroup()

#plot stuff
assambled_tbl %>%
  ggplot(aes(date, cumulative_cases), color = countriesAndTerritories) +
  geom_line(aes(x     = date,
                y     = cumulative_cases,
                color = countriesAndTerritories)) + 
  
  scale_x_date(breaks = "1 month", minor_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "")) +
  geom_label(aes(label = cumulative_cases),
             size  = 5,
             nudge_x  = -40,
             nudge_y  = 5,
             fill  = "#981fc4",
             color = "white",
             fontface = "italic",
             data = filter(cumulative_cases_tbl,date == max(date) & cumulative_cases == max(cumulative_cases)))+
  theme_light() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic"),
    plot.background = element_blank(),
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "The united States leads the battle nobody wants to win",
    x = "Year 2020",
    y = "Cumulative Cases",
    color = "Continent / Country" # Legend text
  )
