library(tidyverse)
library(rvest)
library(jsonlite)
library(RVerbalExpressions)

#Aim of this script is to scrape available / taken players from either Fantasypros.com or NFL.com

url <- "https://www.fantasypros.com/nfl/rankings/ros-overall.php"

raw <- read_html(url) %>% 
  html_table(fill = TRUE)

player_db <- raw[[1]] %>% 
  as_tibble(.name_repair = "unique") %>% 
  select(c(Rank:ADP), -WSIT)

x <- rx_uppercase() %>% 
  rx_find(". ")

player_db_test<- player_db %>% 
  mutate(name = player_db$`Overall (Team)` %>% str_split_fixed(x, n = 2))

  
player_db$`Overall (Team)` %>% 
  str_split_fixed(x, n = 2) %>% 
  as_tibble(.name_repair = "minimal")


player_db <- player_db %>% 
  rename("name" = `Overall (Team)`) 

player_db$name %>% 
  unnest()

player_d<- player_db$`Overall (Team)` %>% 
  str_split_fixed(x, 2) %>% 
  as_tibble() %>% 
  rename("name" = V1,
         "other" = V2)

split[1]

from_json_to_tibble <- function(json) {
  json %>%
    fromJSON() %>%
    as_tibble()
}

name[1] %>% 
  from_json_to_tibble()

raw %>% 
html_node(".sticky-table mobile-table table-view-wrapper selectorgadget_rejected")
