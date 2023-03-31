source("utils_chart.R")

pbp <- data.table::fread("./data/nbastats_2022_on_court.csv")
shots <- data.table::fread("./data/shotdetail_2022.csv")

player_id <- 1629673
partner_id <- 201939

plot_pims <- function(pbp, shots, player_id, partner_id){
  player_info <- hoopR::nba_commonallplayers(season = "2022-23", is_only_current_season = 0)
  player_info <- data.table::as.data.table(player_info$CommonAllPlayers)
  
  shots <- shots[LOC_Y <= 297.5]
  
  player_game <- unique(pbp[PLAYER1_ID == player_id & EVENTMSGTYPE %in% c(1, 2), GAME_ID])
  
  partner_on_court <- apply(pbp[GAME_ID %in% player_game, PLAYER1:PLAYER10], 1, function(x) {partner_id %in% x})
  partner_game <- unique(pbp[GAME_ID %in% player_game][partner_on_court][, GAME_ID])
  
  list_games <- dplyr::intersect(player_game, partner_game)
  
  t1 <- unique(pbp[PLAYER1_ID == player_id & EVENTMSGTYPE %in% c(1, 2) & GAME_ID %in% c(list_games)])
  t2 <- shots[GAME_ID %in% c(list_games) & PLAYER_ID == player_id]
  
  t1$N <- apply(t1[,PLAYER1:PLAYER10], 1, function(x) {partner_id %in% x})
  
  for_hex <- t2 %>% 
    dplyr::select(GAME_ID, GAME_EVENT_ID, PLAYER_ID, SHOT_TYPE, SHOT_ZONE_BASIC, SHOT_ZONE_AREA, SHOT_ZONE_RANGE, LOC_X, LOC_Y, SHOT_ATTEMPTED_FLAG, SHOT_MADE_FLAG) %>% 
    dplyr::mutate(
      LOC_X = LOC_X / 10,
      LOC_Y = LOC_Y / 10 + 5.25,
      SHOT_VALUE = dplyr::if_else(tolower(SHOT_TYPE) == "3pt field goal", 3, 2)
    ) %>% 
    dplyr::inner_join(
      t1 %>% 
        dplyr::select(GAME_ID, EVENTNUM, N) %>% 
        dplyr::mutate(
          N = dplyr::if_else(N, 'on', 'off')
        ) %>% 
        dplyr::rename(GAME_EVENT_ID = EVENTNUM),
      by = c("GAME_ID", "GAME_EVENT_ID")
    ) %>% 
    dplyr::select(LOC_X, LOC_Y, N)
  
  on <- for_hex %>% 
    dplyr::filter(N == "on")
  
  off <- for_hex %>% 
    dplyr::filter(N == "off")
  
  on_x <- dplyr::pull(on, LOC_X)
  on_y <- dplyr::pull(on, LOC_Y)
  
  off_x <- dplyr::pull(off, LOC_X)
  off_y <- dplyr::pull(off, LOC_Y)
  
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
    dplyr::as_tibble() %>% 
    dplyr::mutate(x_coord= diff$x) %>% 
    tidyr::pivot_longer(-x_coord, names_to = "y_coord", values_to = "z") %>% 
    dplyr::mutate(y_coord = as.double(y_coord),
                  bandwidth = list(bandwidth_calc))
  
  gg <- tbl %>%
    ggplot2::ggplot(ggplot2::aes(x_coord, y_coord)) +
    ggplot2::geom_raster(ggplot2::aes(x_coord, y_coord, fill=z))  +
    ggplot2::scale_fill_gradient2(low="blue", mid="#fffcf2", high="red", midpoint=0) +
    ggplot2::scale_color_gradient2(low="blue", mid="#fffcf2", high="red", midpoint=0) +
    ggplot2::guides(color="none", fill="none") +
    add_basketball_court() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust=0.5))
  
  ### Таблица значений
  main_tbl <- t2 %>% 
    dplyr::select(GAME_ID, GAME_EVENT_ID, PLAYER_ID, SHOT_TYPE, SHOT_ZONE_BASIC, SHOT_ZONE_AREA, SHOT_ZONE_RANGE, LOC_X, LOC_Y, SHOT_ATTEMPTED_FLAG, SHOT_MADE_FLAG) %>% 
    dplyr::mutate(
      LOC_X = LOC_X / 10,
      LOC_Y = LOC_Y / 10 + 5.25,
      SHOT_VALUE = dplyr::if_else(tolower(SHOT_TYPE) == "3pt field goal", 3, 2)
    ) %>% 
    dplyr::inner_join(
      t1 %>% 
        dplyr::select(GAME_ID, EVENTNUM, N) %>% 
        dplyr::mutate(
          N = dplyr::if_else(N, 'on', 'off')
        ) %>% 
        dplyr::rename(GAME_EVENT_ID = EVENTNUM),
      by = c("GAME_ID", "GAME_EVENT_ID")
    ) %>% 
    dplyr::mutate(MADE_3P = dplyr::if_else(SHOT_VALUE == 3, SHOT_MADE_FLAG, 0)) %>% 
    dplyr::group_by(N, SHOT_ZONE_BASIC) %>%
    dplyr::summarise(
      FGA = dplyr::n(),
      FG = sum(SHOT_MADE_FLAG),
      MADE_3P = sum(MADE_3P),
      PTS = sum(SHOT_VALUE*SHOT_MADE_FLAG),
    ) %>% 
    dplyr::ungroup()
  
  cluster_data <- main_tbl %>% 
    dplyr::group_by(N) %>% 
    dplyr::summarise(
      COUNT = sum(FGA),
      eFG = round((sum(FG) + 0.5*sum(MADE_3P))/sum(FGA)*100, 2),
      PPP = round(sum(PTS)/sum(FGA), 2)
    ) %>% 
    dplyr::ungroup()
  
  for_tbl <- main_tbl %>% 
    dplyr::group_by(N) %>% 
    dplyr::mutate(N_SHOT = sum(FGA)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      SHARE = round(FGA/N_SHOT*100, 2),
      eFG = round((FG + 0.5*MADE_3P)/FGA*100, 2),
      PPP = round(PTS/FGA, 2)
    ) %>% 
    tidyr::pivot_wider(id_cols = SHOT_ZONE_BASIC, names_from = N, values_from = SHARE:PPP) %>% 
    dplyr::select(SHOT_ZONE_BASIC, SHARE_on, eFG_on, PPP_on, SHARE_off, eFG_off, PPP_off) %>% 
    dplyr::mutate(
      SHOT_ZONE_BASIC = dplyr::if_else(SHOT_ZONE_BASIC == "Left Corner 3", "Left<br>Corner 3",
                                       dplyr::if_else(SHOT_ZONE_BASIC == "Right Corner 3", "Right<br>Corner 3",
                                                      dplyr::if_else(SHOT_ZONE_BASIC == "Above the Break 3", "Above the<br>Break 3",
                                                                     dplyr::if_else(SHOT_ZONE_BASIC == "In The Paint (Non-RA)", "In The Paint<br>(Non-RA)",
                                                                                    dplyr::if_else(SHOT_ZONE_BASIC == "Mid-Range", "Mid-Range",
                                                                                                   dplyr::if_else(SHOT_ZONE_BASIC == "Restricted Area", "Restricted<br>Area", SHOT_ZONE_BASIC))))))
    )
  
  gt <- for_tbl %>% 
    gt::gt() %>% 
    gt::tab_spanner(
      label = gt::md(paste0("<span style='color:red'>**ON**</span><br>", 
                        cluster_data %>% dplyr::filter(N == 'on') %>% dplyr::select(COUNT) %>% dplyr::pull(), " FGA<br>", 
                        cluster_data %>% dplyr::filter(N == 'on') %>% dplyr::select(eFG) %>% dplyr::pull(), " eFG<br>", 
                        cluster_data %>% dplyr::filter(N == 'on') %>% dplyr::select(PPP) %>% dplyr::pull()," PPP")),
      columns = SHARE_on:PPP_on
    ) %>% 
    gt::tab_spanner(
      label = gt::md(paste0("<span style='color:blue'>**OFF**</span><br>", 
                        cluster_data %>% dplyr::filter(N == 'off') %>% dplyr::select(COUNT) %>% dplyr::pull(), " FGA<br>", 
                        cluster_data %>% dplyr::filter(N == 'off') %>% dplyr::select(eFG) %>% dplyr::pull(), " eFG<br>", 
                        cluster_data %>% dplyr::filter(N == 'off') %>% dplyr::select(PPP) %>% dplyr::pull()," PPP")),
      columns = SHARE_off:PPP_off
    ) %>% 
    gt::cols_label(
      SHARE_on = "SHARE",
      eFG_on = "eFG",
      PPP_on = "PPP",
      SHARE_off = "SHARE",
      eFG_off = "eFG",
      PPP_off = "PPP"
    ) %>% 
    gt::fmt_markdown(columns = SHOT_ZONE_BASIC) %>% 
    gt::cols_align(columns = SHOT_ZONE_BASIC, align = "center") %>% 
    gt::cols_label(
      SHOT_ZONE_BASIC = ""
    ) %>% 
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top"),
        weight = gt::px(2)),
      locations = gt::cells_body(
        columns = gt::everything()
      )
    ) %>% 
    gt::tab_options(
      table.background.color = "#fffcf2"
    )
  
  tmp <- tempfile(fileext = '.png') #generate path to temp .png file
  gt::gtsave(gt, tmp, expand=0) #save gt table as png
  table_png <- png::readPNG(tmp, native = TRUE) # read tmp png file
  
  layout <- c(
    patchwork::area(t=1, l=1, b=6, r=5),
    patchwork::area(t=2, l=6, b=4, r=7)
  )
  
  gg1 <- gg + table_png + patchwork::plot_layout(design = layout) +
    patchwork::plot_annotation(
      title = paste0(player_info[PERSON_ID == player_id, DISPLAY_FIRST_LAST], " heatmap with ",
                     player_info[PERSON_ID == partner_id, DISPLAY_FIRST_LAST],
                     " <span style='color:red'>**ON**</span>/<span style='color:blue'>**OFF**</span> court"),
      subtitle = "In red zones player made attempts more when partner on court, in blue zones when partner off court",
      caption = "Data: **stats.nba.com**; Twitter: **@vshufinskiy**"
    ) &
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#fffcf2"),
      plot.title = ggtext::element_markdown(hjust=0.5, size = 15),
      plot.caption = ggtext::element_markdown(size = 10)
    )
  
  cowplot::ggdraw(gg1) +
    cowplot::draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", player_id, ".png"),
                        x = .70, y = .8, height = .15, width = .15) +
    cowplot::draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", partner_id, ".png"),
                        x = .85, y = .8, height = .15, width = .15)
}

plot_pims(pbp, shots, player_id, partner_id)