#League Schedule
library(magrittr)

league <- tibble::tribble(
     ~team, ~division,
       "Metzkausen Mermaids","Division 3",
            "First Claas","Division 2",
            "Rust Rhinos","Division 3",
         "FAITHFUL 49ers","Division 2",
  "Tremonia Trash Pandas","Division 3",
         "Texas Rattlers","Division 2",
            "Hilden Fins","Division 2",
        "Saints Gladbeck","Division 3",
          "Green Kickers","Division 3",
       "Ragnar's Raiders","Division 1",
            "BWS Bottrop","Division 3",
    "Clearwater Manatees","Division 2",
   "Seahawks Ellinghorst","Division 1",
             "FlyingPats","Division 2",
               "juju 1st","Division 1",
             "Flyingfins","Division 1",
        "Witten Warriors","Division 1",
        "Bottrop Dragons","Division 1"
  ) %>% 
  dplyr::arrange(team)

create_intradivision_schedule <- function(selected_division){
  div <- league %>% 
    dplyr::filter(division %in% selected_division) %>% 
    dplyr::pull(team)
  
  no_of_games <- length(div)-1
  no_of_games_per_week <- length(div)/2
  
  n <- length(div)
  teams <- div
  r <- (length(div)-1)*2
  
  
  rounds <- list()
  for( i in 1:r){
    round <- 
      tibble::tibble(
        round = i,
        team1 = teams[1:(n/2)], 
        team2 = rev(teams)[1:(n/2)])
    rounds[[i]] <- round
    teams <- c( teams[1],  dplyr::last(teams), head(teams[-1],-1) ) 
  }
  
  rr <- dplyr::bind_rows(rounds) %>% 
    dplyr::mutate(division = selected_division)
  return(rr)
  
}


intradivision <- purrr::map_df(unique(teams$division), create_intradivision_schedule)

groups <- list()
available_teams <- league$team

create_interdivision_schedule <- function(team_selection){
  
  #print(team_selection)
  
  if(length(groups)>0){
  check_enough_games <- groups %>% 
    unlist() %>% 
    table() %>% 
    dplyr::as_tibble() %>% 
    dplyr::filter(n >=4) %>% 
    dplyr::pull(.)
  
  if(any(available_teams %in% check_enough_games)){
    available_teams <<- available_teams[available_teams != check_enough_games]
  }
  if(team_selection %in% check_enough_games){
    return()
  }
  
  }
  available_teams <<- available_teams[available_teams != team_selection]
  
  sample_size <- 4 - (groups %>% unlist() %>% .[. == team_selection] %>% length())
  
  if(sample_size <= 1){return()}
  
  main_team <- team_selection
  division_main_team <- league %>% 
    dplyr::filter(team == main_team) %>% 
    dplyr::pull(division)
  available_matchups <- league %>%
    dplyr::filter(team %in% available_teams,
                  !division %in% division_main_team) %>% 
    dplyr::pull(team)
    
  subgroup <- sample(available_matchups, size = sample_size)
  
  groups[[main_team]] <<- subgroup
  
  for(i in names(groups)){
    already_plays <- c()
  if(main_team %in% groups[[i]]){
    already_plays <- c(already_plays, names(groups[i]))
  }
  }
  
  if(any(already_plays %in% subgroup)){
    subgroup <- sample(dplyr::pull(league[!league$team %in% c(main_team, already_plays),1]))
  }
  
  schedule <- tibble::tibble(
    team1 = main_team,
    team2 = subgroup
  )
  
  return(schedule)
}

n <- 0
while(any(n != 4)){
interdivision <- purrr::map(league$team, purrr::safely(create_interdivision_schedule)) 
  results <- purrr::transpose(interdivision)[["result"]] %>% 
  do.call(rbind, .)
n <- results %>% 
  tidyr::pivot_longer(names_to = "team", values_to = "name", cols = c(1,2))%>% 
  dplyr::count(name) %>% 
  dplyr::pull(n)
groups <- list()
available_teams <- league$team
}


full_schedule <-  dplyr::bind_rows(intradivision, results) %>% 
  dplyr::rename("week" = round)

schedule_by_team <- list()

correct_schedule <- function(selection){
  
  print(selection)
  team_schedule <- full_schedule %>% 
  dplyr::filter(team1 %in% selection |
                  team2 %in% selection) %>% 
  dplyr::arrange(week) %>%
  tibble::rowid_to_column() 
  
  print(team_schedule)
  
  missing_weeks <- c(1:14)[!c(1:14) %in% team_schedule$week]
  
  check_enemy_schedule <- full_schedule %>% 
    dplyr::filter(team1 %in% selection |
                    team2 %in% selection) %>% 
    dplyr::filter(is.na(week)) %>% 
    tidyr::pivot_longer(cols = c(2,3),
                        names_to = "what",
                        values_to = "team") %>% 
    dplyr::filter(team != selection) %>% 
    dplyr::pull(team)
  
    team_schedule <- team_schedule %>% 
    dplyr::filter(!is.na(week)) %>% 
    dplyr::bind_rows(., filled_missing_weeks) %>% 
    dplyr::arrange(week) %>% 
    dplyr::select(-rowid)
  
  schedule_by_team[[selection]] <<- team_schedule
#max_week <- team_schedule %>% 
#  dplyr::pull(week) %>% 
#  max(na.rm = T)
#
#missing <- nrow(team_schedule %>% dplyr::filter(is.na(week)))

full_schedule <<- full_schedule %>% 
  dplyr::filter(team1 != selection,
                team2 != selection) %>% 
  dplyr::bind_rows(., team_schedule)
return(full_schedule)
}

purrr::map_df(league$team, correct_schedule)

writexl::write_xlsx(schedule_by_team, "team_schedule.xlsx")
