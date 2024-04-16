library(tidyverse)
library(ggplot2)

energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date)
str(energy_load)

# Average Load per Hour
energy_load |>
    group_by(hour_int) |>
    summarise(mean_load = mean(load)) |>
    ggplot(aes(x = hour_int, y = mean_load)) +
    geom_point() +
    geom_line()

# Histogramm of the data
ggplot(energy_load, aes(load)) +
    geom_histogram(binwidth = 500)


# Filter for one specific week, for example the first week of the dataset
energy_load |>
    filter(year(date) == 2024 & week(date) == 1) |>
    ggplot(aes(x = date, y = load)) +
    geom_line()
