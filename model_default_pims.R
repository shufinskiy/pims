### Парсинг файла с default_pims, построение графика и линейной модели
library(dplyr)
library(stringr)
library(ggplot2)

set.seed(42)

f <- file("./data/save_endpoit.txt", open="r")
txt <- readLines(f)
close(f)

df <- data.frame(N_SHOTS = sapply(txt, function(text){as.integer(stringr::str_extract(text, "(?<=N_SHOT: )\\d+"))}, USE.NAMES = FALSE),
                 DEFAULT_PIMS = sapply(txt, function(text){as.numeric(stringr::str_extract(text, "(?<=PIMS: )\\d{1}\\.\\d+$"))}, USE.NAMES = FALSE))

df <- df %>% 
  dplyr::distinct(N_SHOTS, .keep_all = TRUE)

ggplot2::ggplot(df, aes(N_SHOTS, DEFAULT_PIMS)) +
  ggplot2::geom_line() +
  ggplot2::geom_point()

### МОдели старт со 100 бросков
lm.DF <- lm(DEFAULT_PIMS ~ N_SHOTS, df)
lm.DF2 <- lm(DEFAULT_PIMS ~ poly(N_SHOTS, 2, raw = TRUE), df)
lm.DF3 <- lm(DEFAULT_PIMS ~ poly(N_SHOTS, 3, raw = TRUE), df)
lm.DF4 <- lm(DEFAULT_PIMS ~ poly(N_SHOTS, 4, raw = TRUE), df)
lm.DF5 <- lm(DEFAULT_PIMS ~ poly(N_SHOTS, 5, raw = TRUE), df)


df[, "PREDICT_BASE"] <- predict(lm.DF, data.frame(N_SHOTS = df$N_SHOTS))
df[, "PREDICT2"] <- predict(lm.DF2, data.frame(N_SHOTS = df$N_SHOTS))
df[, "PREDICT3"] <- predict(lm.DF3, data.frame(N_SHOTS = df$N_SHOTS))
df[, "PREDICT4"] <- predict(lm.DF4, data.frame(N_SHOTS = df$N_SHOTS))
df[, "PREDICT5"] <- predict(lm.DF5, data.frame(N_SHOTS = df$N_SHOTS))
df[, "FORMULA"] <- 1.144 - 0.003399*df$N_SHOTS + 0.000005337*df$N_SHOTS^2 - 0.000000002997*df$N_SHOTS^3

ggplot2::ggplot(df) +
  ggplot2::geom_line(aes(x=N_SHOTS, y=DEFAULT_PIMS)) +
  ggplot2::geom_line(aes(x=N_SHOTS, y=PREDICT_BASE), color="blue") +
  ggplot2::geom_line(aes(x=N_SHOTS, y=PREDICT2), color="orange") +
  ggplot2::geom_line(aes(x=N_SHOTS, y=FORMULA), color="purple") +
  ggplot2::geom_line(aes(x=N_SHOTS, y=PREDICT3), color="red") +
  ggplot2::geom_line(aes(x=N_SHOTS, y=PREDICT4), color="brown") +
  ggplot2::geom_line(aes(x=N_SHOTS, y=PREDICT5), color="green")