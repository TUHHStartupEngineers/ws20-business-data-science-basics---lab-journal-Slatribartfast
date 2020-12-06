library(vroom)
library(tidyverse)
library(readxl)
library(lubridate)
library("writexl")
library(stringr)
library(dplyr)
library(tidyr)

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)


#load stuff
assignee_tbl <- vroom(
  file       = "./00_data/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
patent_assignee_tbl <- vroom(
  file       = "./00_data/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
patent_tbl <- vroom(
  file       = "./00_data/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

#genearte stuff
Recent_patent_acitivity_tbl <- tibble()

#filter us companys
assignee_tbl <- assignee_tbl %>%
  filter(type == 2)

#assamble it
Recent_patent_acitivity_tbl <- assignee_tbl %>%
  left_join(patent_assignee_tbl, by = c("id" = "assignee_id")) %>%
  left_join(patent_tbl, by = c("patent_id" = "id")) %>%
  mutate(year = year(date)) %>%
  filter(year == 2019)%>%
  group_by(organization) %>%
  summarise(count = n())%>%
  arrange(desc(count))%>%
  slice(1:10)

#show it
glimpse(Recent_patent_acitivity_tbl)