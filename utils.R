### Функция для скачивания pbp данных
get_nba_data <- function(seasons = seq(1996, 2022), data = c("datanba", "nbastats", "pbpstats", "shotdetail"), untar = FALSE){
  df <- expand.grid(data, seasons)
  
  need_data <- paste(df$Var1, df$Var2, sep = "_")
  
  temp <- tempfile()
  download.file("https://raw.githubusercontent.com/shufinskiy/nba_data/main/list_data.txt", temp)
  f <- readLines(temp)
  unlink(temp)
  
  v <- unlist(strsplit(f, "="))
  
  name_v <- v[seq(1, length(v), 2)]
  element_v <- v[seq(2, length(v), 2)]
  
  need_name <- name_v[which(name_v %in% need_data)]
  need_element <- element_v[which(name_v %in% need_data)]
  
  for(i in seq_along(need_element)){
    destfile <- paste0(need_name[i], ".tar.xz")
    download.file(need_element[i], destfile = destfile)
    if(untar){
      untar(destfile, paste0(need_name[i], ".csv"))
      unlink(destfile)
    }
  }  
}

### Функция для добавления игроков на площадке
players_on_court <- function(df, order_fill = TRUE) {
  
  if(inherits(df$PCTIMESTRING, "character")){
    df$PERIOD <- as.numeric(df$PERIOD)
    df <- mutate(df, PCTIMESTRING = convert_time_to_second(df, PCTIMESTRING))
  }
  
  l <- lapply(sort(unique(df$PERIOD)), function(x){
    
    df_period <- filter(df, PERIOD == x)
    all_id <- unique(c(df_period$PLAYER1_ID[!df_period$EVENTMSGTYPE %in% c(9, 18) & !is.na(df_period$PLAYER1_NAME) & !df_period$PERSON1TYPE %in% c(6, 7)], 
                       df_period$PLAYER2_ID[!df_period$EVENTMSGTYPE %in% c(9, 18) & !is.na(df_period$PLAYER2_NAME) & !df_period$PERSON2TYPE %in% c(6, 7)], 
                       df_period$PLAYER3_ID[!df_period$EVENTMSGTYPE %in% c(9, 18) & !is.na(df_period$PLAYER3_NAME) & !df_period$PERSON3TYPE %in% c(6, 7)]))
    
    
    all_id <- all_id[all_id != 0 & all_id < 1610612737]
    
    sub_off <- unique(df_period$PLAYER1_ID[df_period$EVENTMSGTYPE == 8])
    sub_on <- unique(df_period$PLAYER2_ID[df_period$EVENTMSGTYPE == 8])
    
    '%!in%' <- Negate(`%in%`)
    all_id <- all_id[all_id  %!in% setdiff(sub_on, sub_off)]
    
    sub_on_off <- intersect(sub_on, sub_off)
    
    for (i in sub_on_off){
      on <- min(df_period$PCTIMESTRING[df_period$EVENTMSGTYPE == 8 & df_period$PLAYER2_ID == i])
      off <- min(df_period$PCTIMESTRING[df_period$EVENTMSGTYPE == 8 & df_period$PLAYER1_ID == i])
      if (off > on){
        all_id <- all_id[all_id != i]
      } else if (off == on){
        on_event <- min(df_period$EVENTNUM[df_period$EVENTMSGTYPE == 8 & df_period$PLAYER2_ID == i])
        off_event <- min(df_period$EVENTNUM[df_period$EVENTMSGTYPE == 8 & df_period$PLAYER1_ID == i])
        if(off_event > on_event){
          all_id <- all_id[all_id != i]
        }
      }
    }
    
    
    if((length(all_id) == 10) & order_fill){
      ord_all_id <- df_period %>% 
        select(PLAYER1_ID, PERSON1TYPE) %>% 
        filter(PLAYER1_ID != 0 & PERSON1TYPE %in% c(4, 5)) %>% 
        rename(PLAYER_ID = PLAYER1_ID, PERSONTYPE = PERSON1TYPE) %>% 
        bind_rows(df_period %>% 
                    select(PLAYER2_ID, PERSON2TYPE) %>% 
                    filter(PLAYER2_ID != 0 & PERSON2TYPE %in% c(4, 5)) %>% 
                    rename(PLAYER_ID = PLAYER2_ID, PERSONTYPE = PERSON2TYPE)) %>% 
        bind_rows(df_period %>% 
                    select(PLAYER3_ID, PERSON3TYPE) %>% 
                    filter(PLAYER3_ID != 0 & PERSON3TYPE %in% c(4, 5)) %>% 
                    rename(PLAYER_ID = PLAYER3_ID, PERSONTYPE = PERSON3TYPE)) %>% 
        distinct() %>% 
        arrange(desc(PERSONTYPE)) %>% 
        select(PLAYER_ID) %>% 
        pull()
      
      all_id <- ord_all_id[ord_all_id %in% all_id]
    }
    
    if(length(all_id) != 10){
      
      if(inherits(df$GAME_ID[1], "integer")){
        tmp_gameid <- paste0("00", as.character(df$GAME_ID[1]))
      } else{
        tmp_gameid <- df$GAME_ID[1]
      }
      
      tmp_data <- nba_boxscoretraditionalv2(game_id = tmp_gameid, start_period = x, end_period = x, range_type = 1)$PlayerStats
      
      all_id <- as.integer(tmp_data$PLAYER_ID)
      
      sub_off <- unique(df_period$PLAYER1_ID[df_period$EVENTMSGTYPE == 8])
      sub_on <- unique(df_period$PLAYER2_ID[df_period$EVENTMSGTYPE == 8])
      
      '%!in%' <- Negate(`%in%`)
      all_id <- all_id[all_id  %!in% setdiff(sub_on, sub_off)]
      
      sub_on_off <- intersect(sub_on, sub_off)
      
      for (i in sub_on_off){
        on <- min(df_period$PCTIMESTRING[df_period$EVENTMSGTYPE == 8 & df_period$PLAYER2_ID == i])
        off <- min(df_period$PCTIMESTRING[df_period$EVENTMSGTYPE == 8 & df_period$PLAYER1_ID == i])
        if (off > on){
          all_id <- all_id[all_id != i]
        } else if (off == on){
          on_event <- min(df_period$EVENTNUM[df_period$EVENTMSGTYPE == 8 & df_period$PLAYER2_ID == i])
          off_event <- min(df_period$EVENTNUM[df_period$EVENTMSGTYPE == 8 & df_period$PLAYER1_ID == i])
          if(off_event > on_event){
            all_id <- all_id[all_id != i]
          }
        }
      }
    }
    
    columns <- paste0("PLAYER", seq(1, 10))
    df_period[columns] <- NA
    
    for(i in seq(1:10)){
      df_period[columns][i] <- all_id[i]
    }
    
    for(column in paste0("PLAYER", seq(1, 10))){
      i <- 1
      repeat{
        n <- nrow(df_period)
        if(length(which(df_period$EVENTMSGTYPE == 8 & df_period$PLAYER1_ID == df_period[, column])) == 0){
          break
        }
        i <- min(which(df_period$EVENTMSGTYPE == 8 & df_period[, column] == df_period$PLAYER1_ID))
        player_on <- df_period$PLAYER2_ID[i]
        df_period[i:n, column] <- player_on
      }
    }
    return(df_period)
  })
  return(bind_rows(l))  
}

### Конвертация времени игры из строки в секунды с начала игры
convert_time_to_second <- function(data, column){
  column <- enquo(column)
  data %>%
    separate(!!column, c('MIN', 'SEC'), sep = ':') %>%
    mutate_at(c('MIN', 'SEC'), as.numeric) %>%
    mutate(!!column := ifelse(PERIOD < 5, abs((MIN * 60 + SEC) - 720 * PERIOD), abs((MIN * 60 + SEC) - (2880 + 300 * (PERIOD - 4))))) %>%
    select(!!column) %>%
    pull()
}