#Load Libraries

library(tidyverse)
library(ffanalytics)

#Scrape Data (Player Data, Projections, ECR, Risk)
raw_data_d <- scrape_data(src = c("CBS", "ESPN", "FantasyData", "FantasyPros", "FantasySharks", "FFToday",
                                   "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL", "RTSports",
                                  "Walterfootball"),
                          pos = c("DL", "DB", "LB"),
                        season = 2021)

projections_d <- projections_table(raw_data_d)

proj_total_d <- projections_d %>% 
  add_player_info()

graph_d <- proj_total_d %>% 
  mutate("name" = str_c(first_name, " ", last_name),
         "position" = ifelse(position %in% c("CB", "S"), "DB", ifelse(position %in% c("DT", "DE"), "DL", "LB"))) %>% 
  select(name, team, position, points, floor, ceiling, avg_type, tier, pos_rank) %>% 
  arrange(desc(points)) %>% 
  filter(avg_type == "weighted",
         pos_rank <= 50) 

graph_d$tier <- as.factor(graph_d$tier)

graph_d %>% 
  filter(position == "DL") %>% 
  ggplot(na.rm = TRUE)+
  geom_point(aes(reorder(name, points, mean), points, colour = position), na.rm = TRUE)+
  geom_point(aes(reorder(name, points, mean), floor, colour = position), shape=0, na.rm = TRUE)+
  geom_point(aes(reorder(name, points, mean), ceiling, colour = position), shape = 0, na.rm = TRUE)+
  geom_segment(aes(x = name, xend = name, y = floor, yend = ceiling, colour = position), na.rm = TRUE)+
  coord_flip()+
  geom_text(aes(reorder(name, points, mean), ceiling +0.1, label = name, colour = position), hjust = 0, vjust = 0.5, check_overlap = FALSE, na.rm = TRUE)+
  #scale_y_continuous(limits = c(min(graph_d$floor, na.rm = TRUE)-25,max(graph_d$ceiling, na.rm = TRUE) + 25))+
  scale_colour_brewer(palette = "Dark2")+
  labs(x = element_blank(), y = "Points", title = str_c("Projections, Floor and Ceiling for ", "DB"))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey60"),
        panel.grid.major.y = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())

ggsave("DL_Projections.pdf", width = 8.2, height = 11.6)

