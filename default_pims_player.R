#! /opt/R/4.0.2/bin/Rscript --vanilla

`%>%` <- magrittr::`%>%`
### Загрузка данных сезонов 2016/17 - 2021/22 с Dropbox
download_shotdetail_dropbox <- function(path = "./data/"){
  if(!dir.exists(path)){
    dir.create(path)
  }
  dropbox_link <- c("https://www.dropbox.com/s/7oibz0f6p9jjpxz/shotdetail_2016.tar.xz?dl=1",
                    "https://www.dropbox.com/s/zyos6q35i8po2jy/shotdetail_2017.tar.xz?dl=1",
                    "https://www.dropbox.com/s/3ul5ldxy66ih1ro/shotdetail_2018.tar.xz?dl=1",
                    "https://www.dropbox.com/s/f4dw48a9v8ynhe5/shotdetail_2019.tar.xz?dl=1",
                    "https://www.dropbox.com/s/9cos0l2rat1rela/shotdetail_2020.tar.xz?dl=1",
                    "https://www.dropbox.com/s/de81ie1lj0nvorc/shotdetail_2021.tar.xz?dl=1")
  
  for(link in dropbox_link){
    season <- regmatches(link, regexpr("\\d{4}", link))
    if(!file.exists(paste0(path, season ,".csv"))){
      download.file(link, paste0(path, season ,".tar.xz"))
    }
  }
}

download_shotdetail_dropbox()

### Распаковка файлов и загрузка данных
unpack_and_read <- function(path = "./data", rm_csv=FALSE){
  list_tar <- list.files(path, pattern = "\\.tar\\.xz$", full.names = TRUE)

  for(file in list_tar){
    season <- regmatches(file, regexpr("\\d{4}", file))
    untar(file, exdir = path)
  }
  list_csv <- list.files(path, pattern = "shotdetail", full.names = TRUE)

  df <- data.table::rbindlist(lapply(list_csv, data.table::fread))
  
  if(rm_csv){
    for(file in list_csv){
      unlink(file)
    }  
  }
  return(df)
}

shot_data <- unpack_and_read(rm_csv = TRUE)
### Задача рассчитать среднюю дистанцию между 200 бросками одного игрока без учёта каких-либо факторов
SHOTS <- seq(200, 1600, 10)

shot_data <- shot_data[LOC_Y <= 297.5,][
  , `:=`(YEAR = as.numeric(paste0("20", substr(GAME_ID, 2, 3))), LOC_X = LOC_X/10, LOC_Y = LOC_Y/10 + 5.25)][
    , .(YEAR, PLAYER_ID, LOC_X, LOC_Y)]

### Функция расчёта дистанции
calculate_distance <- function(off_data, on_data, check){
  off_x <- off_data[, LOC_X]
  on_x <- on_data[, LOC_X]
  off_y <- off_data[, LOC_Y]
  on_y <- on_data[, LOC_Y]
  
  mean_x <- mean(abs(c(off_x, on_x)))
  mean_y <- mean(c(off_y, on_y))
  
  diff <- sum(abs(sort(off_x) - sort(on_x))/sqrt(mean_x) + abs(sort(off_y) - sort(on_y))/sqrt(mean_y))/check
}

### Расчёт PIMS у бросков одного игрока в зависимости от величины выборки
pims_default <- function(shots=400, n_sample=100000, plot_step=100, chart=TRUE, chart_path="./charts/pims_endpoints"){
  n_shots <- shot_data[, .(.N), by=c("YEAR", "PLAYER_ID")][N >= shots,]
  
  n_sample <- n_sample
  plot_step <- plot_step
  
  distance <- vector(mode="numeric", length = n_sample)
  plot_point <- vector(mode="numeric", length = n_sample/plot_step)
  
  for(i in seq(n_sample)){
    nrow <- sample(seq(dim(n_shots)[1]), 1)
    player_data <- n_shots[nrow, c(YEAR, PLAYER_ID)]
    
    player_shot <- shot_data[YEAR == player_data[1] & PLAYER_ID == player_data[2]]
    tbl_sample <- player_shot[sample(.N, shots)]
    
    on <- tbl_sample[seq(shots/2)]
    off <- tbl_sample[seq(shots/2+1, shots)]
    
    diff <- calculate_distance(on, off, check = shots/2)
    
    distance[i] <- diff
    
    if(i %% plot_step == 0){
      point <- median(distance[1:i])
      plot_point[i %/% plot_step] <- point
    }
  }
  
  if(chart){
    if(!dir.exists(chart_path)){
      dir.create(chart_path, recursive = TRUE)
    }
    gg <- ggplot2::ggplot(data = data.frame(x=1:length(plot_point), y=plot_point), ggplot2::aes(x, y)) +
      ggplot2::geom_line() +
      ggplot2::labs(title = shots/2)
    
    ggplot2::ggsave(paste0(chart_path, "/avg_dist_", shots/2,".jpeg"), gg, units = "in", width = 7, height = 5)
  }
  
  return(point)
}

pims_endpoint_chart <- function(endpoints, verbose=TRUE, save_endpoint=TRUE){
  pims_vector <- vector(mode="numeric", length = length(endpoints))
  for(i in seq(1, length(endpoints))){
    pims <- pims_default(shots = endpoints[i])
    if(verbose){
      print(paste0("Расчёт выполнен. Значение PIMS для ", endpoints[i]/2," равно: ", pims))
    }
    if(save_endpoint){
      if(file.exists("./save_endpoit.txt")){
        write(paste0("N_SHOT: ", endpoints[i]/2, ", PIMS: ", pims), "./data/save_endpoit.txt", append = TRUE)
      } else {
        write(paste0("N_SHOT: ", endpoints[i]/2, ", PIMS: ", pims), "./data/save_endpoit.txt")
      }
    }
    pims_vector[i] <- pims
  }
  return(pims_vector)
}
set.seed(42)
pims_ <- pims_endpoint_chart(SHOTS)