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

#Prepare for starting Lineup (apply own league Settings)
rb <- my_team %>% 
  filter(position == "RB") %>%
  arrange(desc(points)) %>% 
  head(2)
  
wr <- my_team %>% 
  filter(position == "WR") %>%
  arrange(desc(points)) %>% 
  head(2)

te <- my_team %>% 
  filter(position == "TE") %>%
  arrange(desc(points)) %>% 
  head(1)

qb <- my_team %>% 
  filter(position == "QB") %>%
  arrange(desc(points)) %>% 
  head(1)

k <- my_team %>% 
  filter(position == "K") %>%
  arrange(desc(points)) %>% 
  head(1)

rbwr <- my_team %>% 
  filter(position %in% c("RB", "WR")) %>% 
  arrange(desc(points)) %>% 
  slice(5)

lb <- my_team %>% 
  filter(position == "LB") %>%
  arrange(desc(points)) %>% 
  head(1)

dl <- my_team %>% 
  filter(position == "DL") %>%
  arrange(desc(points)) %>% 
  head(1) 

db <- my_team %>% 
  filter(position == "DB") %>%
  arrange(desc(points)) %>% 
  head(1)

roster <- bind_rows(qb, rb, wr, rbwr, te, k, lb, dl, db) %>% 
  mutate(type = "Owned")

sum(roster$points)

ggplot(roster, na.rm = TRUE)+
  geom_point(aes(reorder(name, points, mean), points, colour = position), na.rm = TRUE)+
  geom_point(aes(reorder(name, points, mean), floor, colour = position), shape=0, na.rm = TRUE)+
  geom_point(aes(reorder(name, points, mean), ceiling, colour = position), shape = 0, na.rm = TRUE)+
  geom_segment(aes(x = name, xend = name, y = floor, yend = ceiling, colour = position), na.rm = TRUE)+
  geom_text(aes(reorder(name, points, mean), ceiling + 0.5, label = name, colour = position), hjust = 0, vjust = 0.5, check_overlap = FALSE, na.rm = TRUE)+
  coord_flip()+
  scale_y_continuous(limits = c(min(roster$floor, na.rm = TRUE)-2,max(roster$ceiling, na.rm = TRUE) + 5))+
  scale_colour_brewer(palette = "Dark2")+
  labs(y = "Points",
       colour = "Position")+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey60"),
        panel.grid.major.y = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())
