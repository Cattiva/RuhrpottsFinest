#Load Libraries

library(ffanalytics)

#Scrape Data (Player Data, Projections, ECR, Risk)
raw_data <- scrape_data(src = c("CBS", "ESPN", "FantasyPros", "FantasySharks", "FantasyFootballNerd", "NFL"),
                        pos = c("QB", "RB", "WR", "TE"),
                        season = 2021)

projections <- projections_table(raw_data, vor_baseline = c(QB = 24, RB = 69, WR = 69, TE = 18))

projections <- projections %>%
  add_ecr() %>% 
  add_risk()

proj_total <- projections %>% 
  add_player_info()



#Preprocessing for Graphic Output
graph <- proj_total %>% 
  mutate("name" = str_c(first_name, " ", last_name)) %>% 
  select(name, team, position, points, floor, ceiling, rank, risk, pos_ecr, tier, avg_type) %>% 
  arrange(desc(points))



graph$tier <- as_factor(graph$tier)

################Function for Graphics#####################
# 
#
#

floor_ceiling <- function(pos = "RB", 
                          num = 50,
                          avg = "weighted"){
  
  graph <- graph %>% 
    filter(avg_type == avg,
           position == pos,
           team != "FA",
           !is.na(points)) %>% 
    head(num)
  
    ggplot(graph, na.rm = TRUE)+
    geom_point(aes(reorder(name, points, mean), points, colour = tier), na.rm = TRUE)+
    geom_point(aes(reorder(name, points, mean), floor, colour = tier), shape=0, na.rm = TRUE)+
    geom_point(aes(reorder(name, points, mean), ceiling, colour = tier), shape = 0, na.rm = TRUE)+
    geom_segment(aes(x = name, xend = name, y = floor, yend = ceiling, colour = tier), na.rm = TRUE)+
    geom_text(aes(reorder(name, points, mean), ceiling + 5, label = name, colour = tier), hjust = 0, vjust = 0.5, check_overlap = FALSE, na.rm = TRUE)+
    coord_flip()+
    scale_y_continuous(limits = c(min(graph$floor, na.rm = TRUE)-25,max(graph$ceiling, na.rm = TRUE) + 25))+
    #scale_colour_brewer(palette = "Dark2")+
    labs(x = element_blank(), y = "Points", title = str_c("Projections, Floor and Ceiling for ", pos))+
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.
          = element_blank(),
          panel.grid.major.x = element_line(colour = "grey60"),
          panel.grid.major.y = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank())
}

#
#
################Function Input####################

#Function Input
floor_ceiling(pos = "RB", num = 20)

################Value over Replacement########

vor_proj <- proj_total %>% 
  mutate("name" = str_c(first_name, " ", last_name)) %>% 
  select(name, team, position, points, points_vor, floor_vor, ceiling_vor, rank, risk, pos_ecr, tier, avg_type) %>%
  arrange(desc(points_vor)) %>% 
  filter(avg_type == "weighted",
         points_vor >= 0,
         risk <= 4.5)

vor_proj %>% 
  head(200) %>% 
  write_csv("custom_rankings.csv")

vor_graph <- vor_proj %>% 
  head(108) %>% 
  ggplot(na.rm = TRUE) +
  geom_point(aes(reorder(name, points_vor, mean), points_vor, colour = position), na.rm = TRUE)+
  geom_vline(xintercept = seq(18.5 , 108, by = 18))+
  geom_text(aes(reorder(name, points, mean), ifelse(points_vor > 190, points_vor - 13, points_vor + 2), label = name, colour = position), hjust = 0, vjust = 0.5, size = 2, check_overlap = FALSE, na.rm = TRUE)+
  #geom_point(aes(name, points, colour = position), shape = 0)+
  coord_flip()+
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(
    x = "Name",
    y = "Points VOR",
    colour = "Position"
  )


ggsave("ValueOverReplacement.pdf", width = 11.6, height = 8.2)

vor_proj$tier <- as_factor(vor_proj$tier)


graph_bench <- graph %>% 
  filter(avg_type == "weighted",
         team != "FA",
         !is.na(points)) %>% 
  slice(109:216) %>% 
  arrange(desc(ceiling))

ggplot(graph_bench, na.rm = TRUE)+
  geom_point(aes(reorder(name, ceiling, mean), points, colour = position), na.rm = TRUE)+
  geom_point(aes(reorder(name, ceiling, mean), floor, colour = position), shape=0, na.rm = TRUE)+
  geom_point(aes(reorder(name, ceiling, mean), ceiling, colour = position), shape = 0, na.rm = TRUE)+
  geom_segment(aes(x = name, xend = name, y = floor, yend = ceiling, colour = position), na.rm = TRUE)+
  geom_text(aes(reorder(name, ceiling, mean), ceiling + 5, label = name, colour = position), hjust = 0, vjust = 0.5, check_overlap = FALSE, size = 2, na.rm = TRUE)+
  coord_flip()+
  scale_y_continuous(limits = c(min(graph_bench$floor, na.rm = TRUE)-25,max(graph_bench$ceiling, na.rm = TRUE) + 25))+
  scale_colour_brewer(palette = "Dark2")+
  labs(x = element_blank(), y = "Points", title = "Bench players sorted by highest Ceiling")+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey60"),
        panel.grid.major.y = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())

ggsave("Bench_Players.pdf", width = 8.2, height = 11.6)
