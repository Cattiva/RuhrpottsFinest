#League Schedule
library(magrittr)

league <- tibble::tribble(
     ~team, ~division,
      "Rentfort Rentners",1,
            "First Claas",1,
            "Rust Rhinos",1,
         "FAITHFUL 49ers",2,
  "Tremonia Trash Pandas",3,
         "Texas Rattlers",2,
            "Hilden Fins",3,
        "Saints Gladbeck",1,
          "Green Kickers",2,
       "Ragnar's Raiders",3,
            "BWS Bottrop",2,
    "Clearwater Manatees",1,
   "Seahawks Ellinghorst",3,
             "FlyingPats",2,
               "juju 1st",1,
             "Flyingfins",3,
        "Witten Warriors",2,
        "Bottrop Dragons",3
  ) %>% 
  dplyr::arrange(team)



divisions <- league |>
  dplyr::group_by(division) |> 
  dplyr::group_split()


# Round Robin -------------------------------------------------------------

create_divisional_schedule <- function(division){

  div <- divisions[[division]] |> 
    dplyr::add_rownames(var = "id") 
  
fix <- 1 # Gesetztes Team
n_teams <- 6 # Wie viele Teams insgesamt
teams <- c(fix:n_teams) # Liste der Teams
round <- teams[teams!=fix] # Beweglicher Teil der Turnierrunde

output <- c(1:n_teams) # Initialer Output des ersten Spieltages


for (w in (1:(n_teams-2))){ # -2 weil wir die initiale Woche schon haben
  new_round <- rep(0,n_teams-1)
if (w == 1){
for (i in seq_along(round)){
  if(i <= (length(round) - w)){
    new_round[i+1] <- round[i] 
  } else {
    new_round[w] <- round[i]
  } 
}
} else {
  for (i in seq_along(round)){
    if(i <= (length(round)-1)){
      new_round[i+1] <- round[i] 
    } else {
      new_round[1] <- round[i]
    } 
  } 
  
  
}
  round <- new_round
  output <- cbind(output, c(fix, new_round))
}


build_schedule <- function(gameweek){
  
  home <- output[1:(n_teams/2),gameweek]
  away <- rev(output[((n_teams /2)+1):n_teams,gameweek])
  
  tibble::tibble(games = stringr::str_c(home, away, sep = "-"),
                 week = gameweek) |> 
    tidyr::separate(col = games,
                    sep = "-",
                    into = c("home", "away"))
}

schedule_home <- purrr::map_df(1:(n_teams-1), build_schedule)

schedule_away <- schedule_home |> 
  dplyr::mutate(week = week + 7) |> 
  dplyr::rename("home" = away,
                "away" = home)

schedule_intradivision <- dplyr::bind_rows(schedule_home,
                                           schedule_away) |> 
  dplyr::left_join(div[,c("team", "id")], by = c("home" = "id")) |> 
  dplyr::left_join(div[,c("team", "id")], by = c("away" = "id")) |> 
  dplyr::select(week, 
                home = "team.x", 
                away = "team.y") 

return(schedule_intradivision)

}

intradivision_schedule <- purrr::map_df(1:3, create_divisional_schedule) 





# Create Interdivisional --------------------------------------------------


## First Part Week 6 and 13 --------------------------------------------------------------

pairs1 <- tibble::tibble(
  home = unlist(divisions[[1]][1:3,1]),
  away = unlist(divisions[[2]][1:3,1]),
  week = 6)

pairs2 <- tibble::tibble(
  home = unlist(divisions[[1]][4:6,1]),
  away = unlist(divisions[[3]][4:6,1]),
  week = 6
)

pairs3 <- tibble::tibble(
  home = unlist(divisions[[2]][4:6,1]),
  away = unlist(divisions[[3]][1:3,1]),
  week = 6
)

w6 <- rbind(pairs1, pairs2, pairs3)
w13 <- w6 |> 
  dplyr::mutate(week = week + 7) |> 
  dplyr::rename("home" = away,
                "away" = home)


# Second Part Week 7 and 14 -------------------------------------------------------------

pairs1_2 <- tibble::tibble(
  home = unlist(divisions[[1]][1:3,1]),
  away = unlist(divisions[[3]][1:3,1]),
  week = 7)

pairs2_2 <- tibble::tibble(
  home = unlist(divisions[[1]][4:6,1]),
  away = unlist(divisions[[2]][4:6,1]),
  week = 7
)

pairs3_2 <- tibble::tibble(
  home = unlist(divisions[[2]][1:3,1]),
  away = unlist(divisions[[3]][4:6,1]),
  week = 7
)

w7 <- rbind(pairs1_2, pairs2_2, pairs3_2)
w14 <- w7 |> 
  dplyr::mutate(week = week + 7) |> 
  dplyr::rename("home" = away,
                "away" = home)


full_schedule <- rbind(intradivision_schedule, w6, w7, w13, w14) |> 
  dplyr::group_by(week) |> 
  dplyr::group_split()


