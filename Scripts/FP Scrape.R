library(tidyverse)
library(rvest)
library(jsonlite)

#Aim of this script is to scrape available / taken players from either Fantasypros.com or NFL.com

url <- "https://www.fantasypros.com/nfl/rankings/ros-overall.php?print=true"

raw <- read_html(url)

name <- raw %>% html_node(".player-label sticky-cell sticky-cell-two")



from_json_to_tibble <- function(json) {
  json %>%
    fromJSON() %>%
    as_tibble()
}

name[1] %>% 
  from_json_to_tibble()

raw %>% 
html_node(".sticky-table mobile-table table-view-wrapper selectorgadget_rejected")
