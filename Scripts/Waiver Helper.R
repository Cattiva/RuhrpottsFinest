library(tidyverse)
library(ffanalytics)
library(XML)
library(RCurl)
library(vroom)
library(fs)

#Insert own Roster
rednecks <- c("Deshaun Watson", "Aaron Jones", "Nick Chubb", "Robby Anderson", "Chris Godwin", "Delanie Walker", "Jaylen Samuels", "Royce Freeman", "Antonio Callaway", "Randall Cobb", "A.J. Brown", "Courtland Sutton", "Joey Slye", "Nick Williams", "Cory Littleton", "Antoine Bethea")

#Scrape Total Data
raw_data <- scrape_data(src = c("ESPN", "FantasyPros", "FantasySharks", "Yahoo", "FantasyFootballNerd", "NFL"),
                        pos = c("QB", "RB", "WR", "TE", "K", "DL", "DB", "LB"),
                        season = 2019,
                        week = 5) #Set correct week

my_projections <- projections_table(raw_data)

#Make tibble for own team
my_team <- my_projections %>% 
  add_player_info() %>% 
  mutate("name" = str_c(first_name, " ", last_name),
         "position" = ifelse(position %in% c("CB", "S"), "DB", ifelse(position %in% c("DT", "DE"), "DL", position))) %>% 
  filter(name %in% rednecks,
         avg_type == "weighted") %>% 
  select(name, position, team, points, pos_rank, floor, ceiling) %>% 
  mutate(type = "my team")

# Waiver Helper
dir <- dir_ls(str_c(getwd(), "/Taken Players Fantasypros"))
available_players <- read_csv(dir) #Searching for a way to get it easier than downloading and saving as .csv
available_players <- available_players %>% 
  select(Overall) %>% 
  rename("Name" = Overall)
available_players <- as_vector(available_players)

waiver_helper <- my_projections %>% 
  add_player_info() %>% 
  mutate(name = str_c(first_name, " ", last_name),
         position = ifelse(position %in% c("CB", "S"), "DB", ifelse(position %in% c("DT", "DE"), "DL", position)),
         type = "waiver") %>% 
  filter(name %in% available_players,
         avg_type == "weighted") %>% 
  select(name, position, team, points, pos_rank, floor, ceiling, type) %>% 
  arrange(desc(points)) 

#Waiver Plot
plottable <- bind_rows(my_team, waiver_helper) %>% 
  head(35)

ggplot(plottable)+
  geom_point(aes(reorder(name, points, mean), points, shape = type, colour = position, size = 2))+
  coord_flip()