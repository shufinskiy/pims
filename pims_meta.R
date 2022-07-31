pbp <- data.table::fread("./data/nbastats_2021.csv")
shots <- data.table::fread("./data/shotdetail_2021.csv")
player_info <- data.table::fread("./data/commonal_players.csv")

calculate_pims_meta <- function(pbp_data, shot_data, player_id = NA, partner_id = NA, min_shots=200, n_repeat=100){
  ### Обработка входных данных (удаление дубликатов, удаление ненужных полей и бросков с дальней дистанции, трансформация дистанции броска в футы от кольца)
  transform_pbp <- unique(pbp_data[, .(GAME_ID, EVENTNUM, EVENTMSGTYPE, PERSON1TYPE, PLAYER1_ID, PLAYER1_TEAM_ID, PLAYER1, PLAYER2, PLAYER3, 
                                  PLAYER4, PLAYER5, PLAYER6, PLAYER7, PLAYER8, PLAYER9, PLAYER10)])
  transform_shots <- shot_data[LOC_Y <= 297.5, .(GAME_ID, GAME_EVENT_ID, PLAYER_ID, TEAM_ID, SHOT_TYPE, LOC_X, LOC_Y, SHOT_ATTEMPTED_FLAG, SHOT_MADE_FLAG)]
  transform_shots$LOC_X <- transform_shots$LOC_X / 10
  transform_shots$LOC_Y <- transform_shots$LOC_Y /10 + 5.25
  transform_shots[, "PTS" := data.table::fifelse((transform_shots$SHOT_TYPE == "2PT Field Goal" & transform_shots$SHOT_MADE_FLAG == 1), 2, 
                                                 data.table::fifelse((transform_shots$SHOT_TYPE == "3PT Field Goal" & transform_shots$SHOT_MADE_FLAG == 1), 3, 0))]
  
  ### Находим игроков, которые сделали в два раза больше минимально необходимого количества бросков для одной группы за одну команду в сезоне
  players <- transform_shots[, .(.N), by=.(PLAYER_ID, TEAM_ID)][N >= min_shots*2, .(PLAYER_ID, TEAM_ID)]
  if(!is.na(player_id)){
    players <- players[PLAYER_ID == player_id]
    if(dim(players)[1] == 0){
      return(NA)
    }
  }
  
  ### Переходим на расчёт каждого игрока в отдельности
  pims_season <- lapply(seq(1, dim(players)[1]), function(n){
    player <- players[n, ]

    ### Список всех игр команды в сезоне с указаением хозяева/гости
    team_games <- unique(transform_pbp[PLAYER1_TEAM_ID == player$TEAM_ID, .(GAME_ID, PERSON1TYPE)])
    
    ### Таблица всех игроков команды с GAME_ID, в которых эти партнёры выходили на площадку
    teammates <- unique(data.table::melt(data.table::merge.data.table(transform_pbp[GAME_ID %in% team_games$GAME_ID, 
                                                                                    .(GAME_ID, PLAYER1, PLAYER2, PLAYER3, PLAYER4, PLAYER5, 
                                                                                      PLAYER6, PLAYER7, PLAYER8, PLAYER9, PLAYER10)],
                                                                      team_games, by="GAME_ID"),
                                         id.vars = c("GAME_ID", "PERSON1TYPE"),
                                         measure.vars = c("PLAYER1", "PLAYER2", "PLAYER3", "PLAYER4", "PLAYER5", "PLAYER6", "PLAYER7", "PLAYER8", "PLAYER9", "PLAYER10"),
                                         value.name = "PLAYER_ID")[
                                           (PERSON1TYPE == 4 & variable %in% c("PLAYER6", "PLAYER7", "PLAYER8", "PLAYER9", "PLAYER10")) 
                                           | (PERSON1TYPE == 5 & variable %in% c("PLAYER1", "PLAYER2", "PLAYER3", "PLAYER4", "PLAYER5"))                       
                                         ][, c("variable", "PERSON1TYPE")  := NULL][PLAYER_ID != player$PLAYER_ID])
    
    ### Вектор id всех игроков команды в сезоне
    teammates_id <- unique(teammates[, PLAYER_ID])
    
    ### Получение строк в transform_pbp, когда игрок совершал бросок и объединение их с transform_shots
    row_player_shot <- transform_pbp[PLAYER1_ID == player$PLAYER_ID & PLAYER1_TEAM_ID == player$TEAM_ID & EVENTMSGTYPE %in% c(1, 2)]
    player_shot_data <- data.table::merge.data.table(row_player_shot, transform_shots, by.x = c("GAME_ID", "EVENTNUM"), by.y = c("GAME_ID", "GAME_EVENT_ID"))
    
    ### Преобразование данных wide-to-long, где строка - это данные о событие (бросок player) для каждого из пяти игроков команды, в которой играет player
    team_on_court <- data.table::melt(player_shot_data[, .(GAME_ID, EVENTNUM, PERSON1TYPE, LOC_X, LOC_Y, PTS, PLAYER1, PLAYER2, PLAYER3, PLAYER4, PLAYER5, 
                                                           PLAYER6, PLAYER7, PLAYER8, PLAYER9, PLAYER10)],
                                      id.vars = c("GAME_ID", "EVENTNUM", "PERSON1TYPE", "LOC_X", "LOC_Y", "PTS"),
                                      measure.vars = c("PLAYER1", "PLAYER2", "PLAYER3", "PLAYER4", "PLAYER5", "PLAYER6", "PLAYER7", "PLAYER8", "PLAYER9", "PLAYER10"),
                                      value.name = "PLAYER_ID")[
                                        (PERSON1TYPE == 4 & variable %in% c("PLAYER6", "PLAYER7", "PLAYER8", "PLAYER9", "PLAYER10")) 
                                        | (PERSON1TYPE == 5 & variable %in% c("PLAYER1", "PLAYER2", "PLAYER3", "PLAYER4", "PLAYER5"))                       
                                      ][, c("variable", "PERSON1TYPE")  := NULL]
    
    ### Фильтрация партнёров по partner_id
    if(!is.na(partner_id)){
      teammates_id <- teammates_id[teammates_id == partner_id]
    }
    ### Если нет партнёров, удовлетворяющих условиям, возвращаем NA
    if(length(teammates_id) == 0){
      return(NA)
    }
    ### Переходим на расчёт каждого партнёра в отдельности
    teammates_pims <- sapply(teammates_id, function(teammate){

      ### Проверка партнёра, проходит ли он по условию минимального количества бросков в группе (N >= min_shots & N <= cnt_shots - min_shots)
      cnt_player_shots <- dim(player_shot_data[GAME_ID %in% teammates[PLAYER_ID == teammate, GAME_ID]])[1]
      cnt_shots_on <- dim(team_on_court[PLAYER_ID == teammate])[1]
      
      ### TRUE, если проверка не пройдена
      if(!(cnt_player_shots - cnt_shots_on >= min_shots & cnt_shots_on >= min_shots)){
        return(NA)
      }
      ### Создаём фильтрацию team_on_court с данными только двух игроков. 
      ### Разделяем броски на две категории: on(когда player и teammate на площадке) и off(когда на площадке только player)
      player_teammate_shot <- team_on_court[PLAYER_ID %in% c(player$PLAYER_ID, teammate) & GAME_ID %in% teammates[PLAYER_ID == teammate, GAME_ID]]
      factor_table <- player_teammate_shot[, .(.N), by=c("GAME_ID", "EVENTNUM")][, ("ON_OFF") := data.table::fifelse(N == 2, "on", "off")]
      
      ### Добавляем информацию ON_OFF в таблицу player_teammate_shot, удаляем  дубликаты строк
      player_teammate_factor <- unique(data.table::merge.data.table(player_teammate_shot, factor_table, by=c("GAME_ID", "EVENTNUM"))[
        , "PLAYER_ID" := NULL])[, .(ON_OFF, LOC_X, LOC_Y, PTS)]
      ### Количество бросков в меньшей из двух групп
      min_group <- player_teammate_factor[, .N, by="ON_OFF"][, min(N)]
      
      ### Фиксация сида для воспроизводимости
      set.seed(0)
      
      ### Вектор рандомных значений (для воспроизводимости)
      random_seed <- seq(1, n_repeat)
      raw_pims_median <- t(sapply(random_seed, function(seed){
        set.seed(seed)
        
        ### Деление на группы, обрезка большей группы до размера наименьшей.
        on_shots <- player_teammate_factor[ON_OFF == "on"][sample(.N, min_group)]
        off_shots <- player_teammate_factor[ON_OFF == "off"][sample(.N, min_group)]
        
        ### Разделение на четыре вектора по парам значений (координата, on_off)
        off_x <- off_shots[, LOC_X]
        on_x <- on_shots[, LOC_X]
        off_y <- off_shots[, LOC_Y]
        on_y <- on_shots[, LOC_Y]
        
        ### points per shots по группам
        pps_on <- on_shots[, round(mean(PTS), 2)]
        pps_off <- off_shots[, round(mean(PTS), 2)]
        
        ### Средние координаты выборки по модулю
        mean_x <- mean(abs(c(off_x, on_x)))
        mean_y <- mean(c(off_y, on_y))
        
        ### Расчёт raw_pims (без учёта случайных колебаний на таком количестве бросков)
        raw_pims <- sum(abs(sort(off_x) - sort(on_x))/sqrt(mean_x) + abs(sort(off_y) - sort(on_y))/sqrt(mean_y))/min_group
        return(c(raw_pims, pps_on, pps_off))
      }))
      raw_pims_median <- apply(raw_pims_median, 2, median)
      ### Расчёт default_pims (без учёта какие партнёры находятся на площадке)
      default_pims <- 1.144 - 0.003399*min_group + 0.000005337*min_group^2 - 0.000000002997*min_group^3
      
      ### Расчёт pims (во сколько раз больше/меньше raw_pims_median относительно default_pims)
      pims <- if(raw_pims_median[1] >= default_pims){
        raw_pims_median[1]/default_pims
      } else {
        -default_pims/raw_pims_median[1]
      }
      name_min_group <- if (min_group == cnt_shots_on) 2 else 1
      
      return(c(pims, raw_pims_median[2], raw_pims_median[3], min_group, name_min_group))
    })

    teammates_id <- teammates_id[!is.na(teammates_pims)]
    teammates_pims <- teammates_pims[!is.na(teammates_pims)]
    
    if(length(teammates_pims) == 0){
      return(NA)
    }
    teammates_pims <- t(data.table::as.data.table(teammates_pims))
    rownames(teammates_pims) <- NULL
    colnames(teammates_pims) <- c("PIMS", "PPS_ON", "PPS_OFF", "N_CNT_SHOT", "MIN_GROUP")
    teammates_pims <- data.table::setDT(cbind(data.table::data.table(PLAYER_ID = rep(player$PLAYER_ID, dim(teammates_pims)[1]),
                                 PARTNER_ID = teammates_id),
                                 teammates_pims))
    teammates_pims[, "MIN_GROUP" := data.table::fifelse(MIN_GROUP == 2, "ON", "OFF")]
    return(teammates_pims)
  })
  ### Удаление пустых элементов
  pims_season <- data.table::rbindlist(pims_season[!is.na(pims_season)])
  return(pims_season)
}

pims_meta <- calculate_pims_meta(pbp, shots, player_id = 2544)
