#Load Libraries

library(tidyverse)
library(ffanalytics)
library(ggrepel)

draft <- read_csv(str_c(getwd(), "/draftresults.csv"))

#Scrape Data (Player Data, Projections, ECR, Risk)
raw_data <- scrape_data(src = c("CBS", "ESPN", "FantasyPros", "FantasySharks", "FantasyFootballNerd", "NFL"),
                        pos = c("QB", "RB", "WR", "TE"),
                        season = 2021)

projections <- projections_table(raw_data, vor_baseline = c(QB = 24, RB = 69, WR = 69, TE = 18))

proj_total <- projections %>% 
  add_player_info()



#Preprocessing for Graphic Output
graph <- proj_total %>% 
  mutate("name" = str_c(first_name, " ", last_name)) %>% 
  select(name, team, position, points, floor, ceiling, rank, avg_type) %>% 
  arrange(desc(points))


baseline <- left_join(draft, graph) %>% 
  filter(avg_type == "weighted") %>% 
  select(name, draft_rank, position, points) %>% 
  filter(draft_rank <= 180) %>% #alle spieler, die in den ersrten 10 Runden gedraftet wurden
  group_by(position) %>% 
  count()

base_calc <- left_join(draft, graph) %>% 
  group_by(position) %>%
  filter(avg_type == "weighted") %>% 
  select(name, draft_rank, position, points) %>%
  mutate(pos_rank = seq(1:length(name)))

vor_rb <- base_calc %>% 
  mutate(vor = points - 21)

RB_vor <- base_calc %>% 
  filter(position == "RB",
         name != "Chris Thompson") %>% 
ggplot()+
  geom_point(aes(reorder(name, points, mean), points))+
  geom_hline(data = base_calc %>% filter(pos_rank == 67 & position == "RB"),
              aes(yintercept = points),
             colour = "red")+
  geom_segment(data = base_calc %>% filter(name == "Saquon Barkley"),
               aes(x = name, xend = name, y = 21, yend = points),
               colour = "red",
               size = 2)+
  geom_segment(aes(x = reorder(name, points, mean), xend = reorder(name, points, mean), y = 21, yend = points),
               colour = "grey80")+
  geom_text(data = vor_rb %>% filter(name == "Saquon Barkley"),
            aes(name, 50, label = str_c("Value over replacement = ", round(vor, 2))),
            nudge_x = 1)+
  geom_text_repel(data = base_calc %>% filter(name == "Theo Riddick"),
                  aes(name , points, label = "Baseline Replacement"),
                  nudge_y = 20,
                  point.padding = unit(0.5, "lines"),
                  box.padding = unit(0.5, "lines"),
                  segment.colour = "#CCCCCC")+
  coord_flip()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey60"),
        panel.grid.major.y = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())+
  labs(title = "RB")

WR_vor <- base_calc %>% 
  filter(position == "WR",
         name != "Antonio Brown") %>% 
  ggplot()+
  geom_point(aes(reorder(name, points, mean), points))+
  geom_hline(data = base_calc %>% filter(pos_rank == 71 & position == "WR"),
             aes(yintercept = points),
             colour = "red")+
  geom_segment(data = base_calc %>% filter(name == "DeAndre Hopkins"),
               aes(x = name, xend = name, y = 55.7, yend = points),
               colour = "red",
               size = 2)+
  geom_segment(aes(x = reorder(name, points, mean), xend = reorder(name, points, mean), y = 55.7, yend = points),
               colour = "grey80")+
  geom_text(data = vor_rb %>% filter(name == "DeAndre Hopkins"),
            aes(name, 30, label = str_c("Value over replacement = ", round(vor, 2))),
            nudge_x = 1,
            nudge_y = 30)+
            #point.padding = unit(0.5, "lines"),
            #box.padding = unit(1, "lines"),
            #segment.colour = "#FFFFFFFF")+
  geom_text_repel(data = base_calc %>% filter(name == "Danny Amendola"),
                  aes(name , points, label = "Baseline Replacement"),
                  nudge_y = 20,
                  point.padding = unit(0.5, "lines"),
                  box.padding = unit(0.5, "lines"),
                  segment.colour = "#CCCCCC")+
  coord_flip()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey60"),
        panel.grid.major.y = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())+
  labs(title = "WR")


gridExtra::grid.arrange(RB_vor, WR_vor, nrow = 1)
