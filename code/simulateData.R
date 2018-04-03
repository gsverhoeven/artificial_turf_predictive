# my_par_list <- list(nseasons = 12,
#                     nteams = 18,
#                     offense_vec = scale(runif(18, -1, 1))/2,
#                     defense_vec = scale(runif(18, -1, 1))/2,
#                     mixing_proportion = 0.1,
#                     mu_const = -0.3,
#                     home_advantage = 0.43,
#                     art_turf_vec = c(rep(0, 18)),
#                     artificial_turf_advantage = 0
# )


Simulate_static_ZPD_data <- function(seed, par_list){
  #set.seed(seed)
  nseasons <- par_list$nseasons
  nrounds <- par_list$nrounds
  nteams <- par_list$nteams
  offense_vec <- par_list$offense_vec
  defense_vec <- par_list$defense_vec
  mixing_proportion <- par_list$mixing_proportion
  mu_const <- par_list$mu_const
  home_advantage <- par_list$home_advantage
  art_turf_vec <- par_list$art_turf_vec

  home_team_id <- c()
  away_team_id <- c()
  art_turf <- c()
  goal_difference <- c()
  
  zif_count <- 0
  zif_vec <- c()
  ngames <- 0
  
  
  for(s in 1:nseasons){
    for(i in 1:nteams){
      for(j in 1:nteams){
        if(i != j) {
          art_turf_advantage <- 0
          if(runif(1, 0, 1) <= mixing_proportion){ # we voegen "at random" draws toe
            ngames <- ngames + 1
            zif_count <- zif_count + 1
            goal_difference[ngames] <- 0
            home_team_id[ngames] <- i
            away_team_id[ngames] <- j
            art_turf[ngames] <- 0
            zif_vec[ngames] <- 1
            
          } else{
            ngames <- ngames + 1
            home_team_id[ngames] <- i
            away_team_id[ngames] <- j
            art_turf[ngames] <- 0
            if(art_turf_vec[i] == 1 & art_turf_vec[j] == 0) {
              art_turf_advantage <- par_list$art_turf_advantage
              art_turf[ngames] <- 1}
            
            home_exp_goals <- exp(mu_const + home_advantage + art_turf_advantage +
                                    offense_vec[i] + defense_vec[j])
            away_exp_goals <- exp(mu_const + 
                                    offense_vec[j] + defense_vec[i])
            goal_difference[ngames] <- rpois(1, home_exp_goals) - rpois(1, away_exp_goals)
            zif_vec[ngames] <- 0
          }
        }
      }
    }
  }
  model_data <- list(
    n_teams = length(unique(home_team_id)),
    n_games =  length(goal_difference),
    home_team = home_team_id,
    away_team = away_team_id,
    goal_difference = goal_difference,
    zif_count = zif_count,
    offense_vec = offense_vec,
    defense_vec = defense_vec,
    art_turf = art_turf,
    zif_vec = zif_vec
  )
  model_data
}
##############################
# Start Dynamic
Simulate_dynamic_ZPD_data <- function(seed, par_list){
  set.seed(seed)
  nseasons <- par_list$nseasons
  nrounds <- par_list$nrounds
  nteams <- par_list$nteams
  offense_sigma <- par_list$offense_sigma
  defense_sigma <- par_list$defense_sigma
  # const_mu en home_advantage zetten we vast op 0 en 0.4, net als mixing proportion
  mixing_proportion <- par_list$mixing_proportion
  mu_const <- par_list$mu_const
  home_advantage <- par_list$home_advantage
  art_turf_vec <- par_list$art_turf_vec 
  hyper_variance <- par_list$hyper_variance
  
  # heeft elk team een initiele offense en een defense ability
  # die trekken we uit de prior dist
  offense_vec <- rnorm(nteams, mean = 0, sd = offense_sigma)
  defense_vec <- rnorm(nteams, mean = 0, sd = defense_sigma)
  
  # elk team heeft een variance
  # de team variance is getrokken uit een halfnormaal verdeling met vaste hyper_variance (0.1).
  # de team variance geldt zowel voor de offense als de defense
  team_variance_vec <- abs(rnorm(nteams, mean = 0, sd = hyper_variance))
  
  # we trekken voor zowel de offense als de defense een draw, en updaten de abilities
  
  # we hebben nu een matrix a_defense[week][team] en a_offense[week][team]
  
  a_defense <- data.frame()
  a_offense <-  data.frame()
  # generate time series of defense and offense evolution
  for(w in 1:(nseasons*nrounds)){
    for(j in 1:nteams){
      # offense
      eta <- rnorm(1, 0, team_variance_vec[j])
      if(w == 1) a_offense[j, w] <- offense_vec[j] + eta
      else { a_offense[j, w] <- a_offense[j, w-1] + eta}
      # defense
      eta <- rnorm(1, 0, team_variance_vec[j])
      if(w == 1) a_defense[j, w] <- defense_vec[j] + eta
      else { a_defense[j, w] <- a_defense[j, w-1] + eta}       
    }
  }

  a_defense$team_id <- row.names(a_defense)
  a_offense$team_id <- row.names(a_offense)
 
  # generate match outcomes
  home_team_id <- c()
  away_team_id <- c()
  goal_difference <- c()
  home_week <- c()
  away_week <- c()
  
  # samen met mixing proportion kunnen we nu voor elke round (sample without replacement?)
  # voor elke pairup twee poisson draws of een zero draw trekken

  w <- 1 # week number
  g <- 1 # game number
  for(i in 1:nseasons){
    for(r in 1:nrounds){
      all_teams <- 1:nteams
        home_teams <- sample(all_teams, nteams/2, replace = F)
        away_teams <- all_teams[!(all_teams %in% home_teams)]
        for(t in 1:(nteams/2)){
          home_team_id[g] <-   home_teams[t]
          away_team_id[g] <- away_teams[t]
          home_week[g] <- w
          away_week[g] <- w
          if(runif(1, 0, 1) <= mixing_proportion){ 
            goal_difference[g] <- 0
          } else {
            home_exp_goals <- exp(mu_const + home_advantage + 
                                    a_offense[home_teams[t], w] + a_defense[away_teams[t], w])
            away_exp_goals <- exp(mu_const + 
                                    a_offense[away_teams[t], w] + a_defense[home_teams[t], w])
            goal_difference[g] <- rpois(1, home_exp_goals) - rpois(1, away_exp_goals)
          }
          g <- g + 1
        }
        w <- w + 1
    }
  }  
  id_lut <- data.table(1:nteams, paste("Team", 1:nteams))
  setnames(id_lut, "V1", "home_team_id" )
  setnames(id_lut, "V2", "HomeTeam" )
  # prev_perf zetten we gewoon op 0
  # deze dataset kunnen we op gaan fitten

  model_data <- list(
    n_teams = length(unique(home_team_id)),
    n_games =  g-1,
    n_weeks = w-1,
    home_team = home_team_id,
    away_team = away_team_id,
    home_week = home_week,
    away_week = away_week,
    goal_difference = goal_difference,
    prev_perf = rep(0, nteams),
    id_lut = id_lut,
    n_games_pred = 9,
    home_team_pred = home_team_id[1:9],
    away_team_pred = away_team_id[1:9],
    #zif_count = zif_count,
    a_offense = a_offense,
    a_defense = a_defense,
    art_turf = art_turf_vec,
    team_variance_vec = team_variance_vec
  )
  model_data
}


