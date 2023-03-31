`%>%` <- magrittr::`%>%`

court_themes = list(
  light = list(
    court = '#fffcf2',
    lines = '#999999',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0.3,
    hex_border_color = "#cccccc"
  ),
  dark = list(
    court = '#000004',
    lines = '#999999',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  )
)

circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(tibble::tibble(x = center[1] + radius * cos(angles),
                        y = center[2] + radius * sin(angles)))
}

calculate_court_points <- function(){
  
  width = 50
  height = 94 / 2
  key_height = 19
  inner_key_width = 12
  outer_key_width = 16
  backboard_width = 6
  backboard_offset = 4
  neck_length = 0.5
  hoop_radius = 0.75
  hoop_center_y = backboard_offset + neck_length + hoop_radius
  three_point_radius = 23.75
  three_point_side_radius = 22
  three_point_side_height = 14
  
  court_points = tibble::tibble(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = dplyr::bind_rows(court_points , tibble::tibble(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = dplyr::bind_rows(court_points , tibble::tibble(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = dplyr::bind_rows(court_points , tibble::tibble(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = dplyr::filter(foul_circle, y > key_height) %>%
    dplyr::mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = dplyr::filter(foul_circle, y < key_height) %>%
    dplyr::mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    dplyr::filter(angle_group %% 2 == 0) %>%
    dplyr::select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    dplyr::mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    dplyr::filter(y >= hoop_center_y) %>%
    dplyr::mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    dplyr::filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = tibble::tibble(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = dplyr::bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  return(court_points)
}

add_basketball_court <- function(court_theme=court_themes$light){
  list(
    ggplot2::geom_path(
      data = calculate_court_points(),
      ggplot2::aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ),
    ggplot2::coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)),
    ggplot2::theme(
      text = ggplot2::element_text(color = court_theme$text),
      plot.background = ggplot2::element_rect(fill = court_theme$court, color = court_theme$court),
      panel.background = ggplot2::element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  )
}