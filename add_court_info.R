source("utils.R")

setwd("./data/")

get_nba_data(seasons = seq(2022, 2022), data = c("nbastats", "shotdetail"), untar = TRUE)

setwd("../")

df <- read.csv("./data/nbastats_2022.csv")

df_on_court <- data.table::rbindlist(lapply(unique(df$GAME_ID), function(game_id){
  tmp_df <- df[df$GAME_ID == game_id,]
  if(unique(tmp_df$GAME_ID)[1] == 22200234){
    tmp_df <- tmp_df[tmp_df$EVENTNUM != 248,]
  }
  tmp_df <- players_on_court(tmp_df)
  return(tmp_df)
}))

data.table::fwrite(df_on_court, "./data/nbastats_2022_on_court.csv")
