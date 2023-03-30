### Логика: на признаках наличия/отсутствия партнёра на площадке строим бинарный классификатор,
### Смотрим на точность классифифкатора плюс VIP. Складываем два этих показателя и получаем
### оценку казуальности нахождения партнёра на броски игрока

### НЕ РАБОТАЕТ, НЕВЕРНАЯ ГИПОТЕЗА ИЗНАЧАЛЬНО

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(tidymodels)

source("utils_chart.R")

pbp <- data.table::fread("./data/nbastats_2022_on_court.csv")
shots <- data.table::fread("./data/shotdetail_2022.csv")

player_id <- 1628370
partner_id <- 1627734

shots <- shots[LOC_Y <= 297.5]

player_game <- unique(pbp[PLAYER1_ID == player_id & EVENTMSGTYPE %in% c(1, 2), GAME_ID])

partner_on_court <- apply(pbp[GAME_ID %in% player_game, PLAYER1:PLAYER10], 1, function(x) {partner_id %in% x})
partner_game <- unique(pbp[GAME_ID %in% player_game][partner_on_court][, GAME_ID])

list_games <- intersect(player_game, partner_game)

t1 <- unique(pbp[PLAYER1_ID == player_id & EVENTMSGTYPE %in% c(1, 2) & GAME_ID %in% c(list_games)])
t2 <- shots[GAME_ID %in% c(list_games) & PLAYER_ID == player_id]

t1$N <- apply(t1[,PLAYER1:PLAYER10], 1, function(x) {partner_id %in% x})

## Нужно получить список пятёрки на площадке при бросках монка
t3 <- t2 %>% 
  select(GAME_ID, GAME_EVENT_ID, HTM) %>% 
  inner_join(
    t1 %>% 
      select(GAME_ID, EVENTNUM, N, PLAYER1:PLAYER10) %>% 
      mutate(
        N = if_else(N, 'on', 'off')
      ) %>% 
      rename(GAME_EVENT_ID = EVENTNUM),
    by = c("GAME_ID", "GAME_EVENT_ID")
  ) %>% 
  pivot_longer(PLAYER1:PLAYER10) %>% 
  mutate(CHECK = if_else(HTM == "SAC", 
                         if_else(name %in% c("PLAYER6", "PLAYER7", "PLAYER8", "PLAYER9", "PLAYER10"),
                                 1, 0),
                         if_else(name %in% c("PLAYER6", "PLAYER7", "PLAYER8", "PLAYER9", "PLAYER10"),
                                 0, 1))) %>% 
  filter(CHECK != 0 & value != player_id & value != partner_id) %>% 
  pivot_wider(id_cols = c(GAME_ID, GAME_EVENT_ID, N), names_from = value, names_prefix = "H", values_from = CHECK) %>% 
  select(-c(GAME_ID, GAME_EVENT_ID)) %>% 
  mutate(N = as.factor(if_else(N == "on", 1, 0)))

t3[is.na(t3)] <- 0

### CV model случайного леса
set.seed(345)
t_folds <- vfold_cv(t3, v = 10)

rf_mod <- 
  rand_forest(mtry = 5, min_n = 3, trees = 500) %>% 
  set_engine("ranger", num.threads = 1) %>% 
  set_mode("classification")

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_formula(N ~ .)

set.seed(456)
rf_fit_rs <- 
  rf_workflow %>% 
  fit_resamples(t_folds)
