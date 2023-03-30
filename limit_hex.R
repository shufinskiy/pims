### Исследования крайних значений лимитов для гексагональной диаграммы
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(hoopR)

source("pims.R")
player_info <- nba_commonallplayers(season = "2022-23", is_only_current_season = 0)
player_info <- player_info$CommonAllPlayers

pbp <- data.table::fread("./data/nbastats_2022_on_court.csv")
shots <- data.table::fread("./data/shotdetail_2022.csv")

calc_pims <- calculate_pims(pbp, shots)

fwrite(calc_pims, "./data/pims_2022.csv")

for(i in seq(2016, 2022)){
  df <- fread(paste0("./data/pims_", i, ".csv"))
  df$SEASON = i
  fwrite(df, paste0("./data/pims_", i, ".csv"))
}

list_files <- list.files(path = "./data", pattern = "^pims", full.names = TRUE)

pims <- rbindlist(lapply(list_files, fread))

pims_percentile <- function(pbp, shot, player_id, partner_id, percentile = c(0.01, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.99)){
  player_game <- unique(shot[PLAYER_ID == player_id, .(GAME_ID, TEAM_ID)])
  
  partner_on_court <- function(pbp, partner_id){
    pl1_id <- pbp[PLAYER1_ID == partner_id, .(GAME_ID, PLAYER1_TEAM_ID)]
    colnames(pl1_id) <- c("GAME_ID", "TEAM_ID")
    
    pl2_id <- pbp[PLAYER2_ID == partner_id, .(GAME_ID, PLAYER2_TEAM_ID)]
    colnames(pl2_id) <- c("GAME_ID", "TEAM_ID")
    
    pl3_id <- pbp[PLAYER3_ID == partner_id, .(GAME_ID, PLAYER3_TEAM_ID)]
    colnames(pl3_id) <- c("GAME_ID", "TEAM_ID")
    
    partner_games <- pl1_id %>% bind_rows(pl2_id) %>% bind_rows(pl3_id) %>% distinct()
    return(partner_games)
  }
  
  partner_game <- partner_on_court(pbp, partner_id)
  
  list_games <- player_game %>% 
    inner_join(partner_game, by=c("GAME_ID", "TEAM_ID")) %>% 
    select(GAME_ID) %>% 
    pull()
  
  t1 <- unique(pbp[PLAYER1_ID == player_id & EVENTMSGTYPE %in% c(1, 2) & GAME_ID %in% c(list_games)])
  t2 <- shot[GAME_ID %in% c(list_games) & PLAYER_ID == player_id]
  
  t1$N <- apply(t1[,PLAYER1:PLAYER10], 1, function(x) {partner_id %in% x})
  
  for_hex <- t2 %>% 
    select(GAME_ID, GAME_EVENT_ID, PLAYER_ID, SHOT_TYPE, SHOT_ZONE_BASIC, SHOT_ZONE_AREA, SHOT_ZONE_RANGE, LOC_X, LOC_Y, SHOT_ATTEMPTED_FLAG, SHOT_MADE_FLAG) %>% 
    mutate(
      LOC_X = LOC_X / 10,
      LOC_Y = LOC_Y / 10 + 5.25,
      SHOT_VALUE = if_else(tolower(SHOT_TYPE) == "3pt field goal", 3, 2)
    ) %>% 
    inner_join(
      t1 %>% 
        select(GAME_ID, EVENTNUM, N) %>% 
        mutate(
          N = if_else(N, 'on', 'off')
        ) %>% 
        rename(GAME_EVENT_ID = EVENTNUM),
      by = c("GAME_ID", "GAME_EVENT_ID")
    ) %>% 
    select(LOC_X, LOC_Y, N)
  
  on <- for_hex %>% 
    filter(N == "on")
  
  off <- for_hex %>% 
    filter(N == "off")
  
  on_x <- pull(on, LOC_X)
  on_y <- pull(on, LOC_Y)
  
  off_x <- pull(off, LOC_X)
  off_y <- pull(off, LOC_Y)
  
  x_rng = range(c(-25, 25))
  y_rng = range(c(0, 35))
  
  bandwidth_x <- MASS::bandwidth.nrd(c(on_x, off_x))
  bandwidth_y <- MASS::bandwidth.nrd(c(on_y, off_y))
  
  bandwidth_calc <- c(bandwidth_x, bandwidth_y)
  
  d2_on <- MASS::kde2d(on_x, on_y, h = bandwidth_calc, n=200, lims=c(x_rng, y_rng))
  d2_off <- MASS::kde2d(off_x, off_y, h = bandwidth_calc, n=200, lims=c(x_rng, y_rng))
  
  diff <- d2_on
  diff$z <- d2_on$z - d2_off$z
  colnames(diff$z) <- diff$y
  
  mass_vec <- diff$z %>% 
    as_tibble() %>% 
    mutate(x_coord= diff$x) %>% 
    pivot_longer(-x_coord, names_to = "y_coord", values_to = "z") %>% 
    mutate(y_coord = as.double(y_coord)) %>% 
    select(z) %>% 
    pull()
  
  vec <- quantile(mass_vec, percentile)
  names(vec) <- NULL
  return(vec)
}

season <- 2016
pbp <- fread("./data/nbastats_2016_on_court.csv")
shot <- fread("./data/shotdetail_2016.csv")

m <- matrix(nrow = dim(pims)[1], ncol = 8)

for(i in seq(1, dim(pims)[1])){
  
  cur_season <- pims$SEASON[i]
  
  if(season != cur_season){
    season <- cur_season
    pbp <- fread(paste0("./data/nbastats_", season,"_on_court.csv"))
    shot <- fread(paste0("./data/shotdetail_", season,".csv"))
  }
  
  pims_player <- pims$PLAYER_ID[i]
  pims_partner <- pims$PARTNER_ID[i]

  m[i,] <- pims_percentile(pbp, shot, pims_player, pims_partner)
}

d <- t(as.data.frame(m))
apply(d[1:4,], 1, min)
apply(d[5:8,], 1, max)

### Результаты для процентилей 1, 5, 10, 20, 80, 90, 95
### -0.0059626119 -0.0011732745 -0.0006036679 -0.0002714647 0.0003273740 0.0006278582 0.0013502010 0.0054447895