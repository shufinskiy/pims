library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(gt)
library(patchwork)
library(hoopR)
library(cowplot)

source("utils_chart.R")

pbp <- data.table::fread("./data/nbastats_2022_on_court.csv")
shots <- data.table::fread("./data/shotdetail_2022.csv")

player_id <- 1628370
partner_id <- 1627734

player_info <- nba_commonallplayers(season = "2022-23", is_only_current_season = 0)
player_info <- as.data.table(player_info$CommonAllPlayers)

shots <- shots[LOC_Y <= 297.5]

player_game <- unique(pbp[PLAYER1_ID == player_id & EVENTMSGTYPE %in% c(1, 2), GAME_ID])

partner_on_court <- apply(pbp[GAME_ID %in% player_game, PLAYER1:PLAYER10], 1, function(x) {partner_id %in% x})
partner_game <- unique(pbp[GAME_ID %in% player_game][partner_on_court][, GAME_ID])

list_games <- intersect(player_game, partner_game)

t1 <- unique(pbp[PLAYER1_ID == player_id & EVENTMSGTYPE %in% c(1, 2) & GAME_ID %in% c(list_games)])
t2 <- shots[GAME_ID %in% c(list_games) & PLAYER_ID == player_id]

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

### Проверка идеи бутстрапа
bootstrap <- function(vec, n=10000){
  res <- vector(mode="double", length = n)
  for(i in seq(1, n)){
    set.seed(i)
    mean_samp <- mean(sample(vec, size = length(vec), replace = TRUE))
    res[i] <- mean_samp
  }
  return(res)
}

boot_on_y <- bootstrap(on_y)
boot_on_x <- bootstrap(on_x)

boot_off_y <- bootstrap(off_y)
boot_off_x <- bootstrap(off_x)

boot_gg_y <- ggplot() +
  geom_density(aes(boot_on_y), color="red") +
  geom_density(aes(boot_off_y), color="blue") +
  labs(x = "Distance (ft.)") +
  theme(
    text = element_text(color = court_themes$light$text),
    plot.background = element_rect(fill = court_themes$light$court, color = court_themes$light$court),
    panel.background = element_rect(fill = court_themes$light$court, color = court_themes$light$court),
    panel.border = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

boot_gg_x <- ggplot() +
  geom_density(aes(boot_on_x), color="red") +
  geom_density(aes(boot_off_x), color="blue") +
  labs(x="right/left (ft.)") +
  theme(
    text = element_text(color = court_themes$light$text),
    plot.background = element_rect(fill = court_themes$light$court, color = court_themes$light$court),
    panel.background = element_rect(fill = court_themes$light$court, color = court_themes$light$court),
    panel.border = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

boot_gg <- boot_gg_x + boot_gg_y

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

tbl <- diff$z %>% 
  as_tibble() %>% 
  mutate(x_coord= diff$x) %>% 
  pivot_longer(-x_coord, names_to = "y_coord", values_to = "z") %>% 
  mutate(y_coord = as.double(y_coord),
         bandwidth = list(bandwidth_calc))
  
# min_max <- data.frame(x_coord = c(NA, NA), y_coord = c(NA, NA), z = c(-0.0011732745, 0.0013502010)) %>%
#   as_tibble() %>%
#   mutate(bandwidth = list(bandwidth_calc))
# 
# tbl <- rbind(tbl, min_max)
  
  
gg <- tbl %>%
  ggplot(aes(x_coord, y_coord)) +
  geom_raster(aes(x_coord, y_coord, fill=z))  +
  scale_fill_gradient2(low="blue", mid="#fffcf2", high="red", midpoint=0) +
  scale_color_gradient2(low="blue", mid="#fffcf2", high="red", midpoint=0) +
  guides(color="none", fill="none") +
  add_basketball_court() +
  theme(plot.title = ggtext::element_markdown(hjust=0.5))
  
  ### Таблица значений
main_tbl <- t2 %>% 
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
  mutate(MADE_3P = if_else(SHOT_VALUE == 3, SHOT_MADE_FLAG, 0)) %>% 
  group_by(N, SHOT_ZONE_BASIC) %>%
  summarise(FGA = n(),
            FG = sum(SHOT_MADE_FLAG),
            MADE_3P = sum(MADE_3P),
            PTS = sum(SHOT_VALUE*SHOT_MADE_FLAG),
  ) %>% 
  ungroup()

cluster_data <- main_tbl %>% 
  group_by(N) %>% 
  summarise(
    COUNT = sum(FGA),
    eFG = round((sum(FG) + 0.5*sum(MADE_3P))/sum(FGA)*100, 2),
    PPP = round(sum(PTS)/sum(FGA), 2)
  ) %>% 
  ungroup()

for_tbl <- main_tbl %>% 
  group_by(N) %>% 
  mutate(N_SHOT = sum(FGA)) %>% 
  ungroup() %>% 
  mutate(
    SHARE = round(FGA/N_SHOT*100, 2),
    eFG = round((FG + 0.5*MADE_3P)/FGA*100, 2),
    PPP = round(PTS/FGA, 2)
  ) %>% 
  pivot_wider(id_cols = SHOT_ZONE_BASIC, names_from = N, values_from = SHARE:PPP) %>% 
  select(SHOT_ZONE_BASIC, SHARE_on, eFG_on, PPP_on, SHARE_off, eFG_off, PPP_off) %>% 
  mutate(
    SHOT_ZONE_BASIC = if_else(SHOT_ZONE_BASIC == "Left Corner 3", "Left<br>Corner 3",
                              if_else(SHOT_ZONE_BASIC == "Right Corner 3", "Right<br>Corner 3",
                                      if_else(SHOT_ZONE_BASIC == "Above the Break 3", "Above the<br>Break 3",
                                              if_else(SHOT_ZONE_BASIC == "In The Paint (Non-RA)", "In The Paint<br>(Non-RA)",
                                                      if_else(SHOT_ZONE_BASIC == "Mid-Range", "Mid-Range",
                                                              if_else(SHOT_ZONE_BASIC == "Restricted Area", "Restricted<br>Area", SHOT_ZONE_BASIC))))))
  )

gt <- for_tbl %>% 
  gt() %>% 
  tab_spanner(
    label = md(paste0("<span style='color:red'>**ON**</span><br>", 
                      cluster_data %>% filter(N == 'on') %>% select(COUNT) %>% pull(), " FGA<br>", 
                      cluster_data %>% filter(N == 'on') %>% select(eFG) %>% pull(), " eFG<br>", 
                      cluster_data %>% filter(N == 'on') %>% select(PPP) %>% pull()," PPP")),
    columns = SHARE_on:PPP_on
  ) %>% 
  tab_spanner(
    label = md(paste0("<span style='color:blue'>**OFF**</span><br>", 
                      cluster_data %>% filter(N == 'off') %>% select(COUNT) %>% pull(), " FGA<br>", 
                      cluster_data %>% filter(N == 'off') %>% select(eFG) %>% pull(), " eFG<br>", 
                      cluster_data %>% filter(N == 'off') %>% select(PPP) %>% pull()," PPP")),
    columns = SHARE_off:PPP_off
  ) %>% 
  cols_label(
    SHARE_on = "SHARE",
    eFG_on = "eFG",
    PPP_on = "PPP",
    SHARE_off = "SHARE",
    eFG_off = "eFG",
    PPP_off = "PPP"
  ) %>% 
  fmt_markdown(columns = SHOT_ZONE_BASIC) %>% 
  cols_align(columns = SHOT_ZONE_BASIC, align = "center") %>% 
  cols_label(
    SHOT_ZONE_BASIC = ""
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = c("top"),
      weight = px(2)),
    locations = cells_body(
      columns = everything()
    )
  ) %>% 
  tab_options(
    table.background.color = "#fffcf2"
  )

tmp <- tempfile(fileext = '.png') #generate path to temp .png file
gtsave(gt, tmp, expand=0) #save gt table as png
table_png <- png::readPNG(tmp, native = TRUE) # read tmp png file

layout <- c(
  area(t=1, l=1, b=6, r=5), #area(t=1, l=1, b=6, r=5)
  area(t=2, l=6, b=4, r=7), #area(t=2, l=6, b=4, r=7)
  area(t=5, l=6, b=6, r=7)
)

gg1 <- gg + table_png + boot_gg + plot_layout(design = layout) +
  plot_annotation(
    title = paste0(player_info[PERSON_ID == player_id, DISPLAY_FIRST_LAST], " heatmap with ",
                   player_info[PERSON_ID == partner_id, DISPLAY_FIRST_LAST],
                   " <span style='color:red'>**ON**</span>/<span style='color:blue'>**OFF**</span> court"),
    subtitle = "In red zones player made attempts more when partner on court, in blue zones when partner off court",
    caption = "Data: **stats.nba.com**; Twitter: **@vshufinskiy**"
  ) &
  theme(
    plot.background = element_rect(fill = "#fffcf2"),
    plot.title = ggtext::element_markdown(hjust=0.5, size = 15),
    plot.caption = ggtext::element_markdown(size = 10)
  )

ggdraw(gg1) +
  draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", player_id, ".png"),
             x = .70, y = .8, height = .15, width = .15) +
  draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", partner_id, ".png"),
             x = .85, y = .8, height = .15, width = .15)
