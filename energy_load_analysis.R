library(tidyverse)
library(ggplot2)
library(plotly)

energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# ---- Average Load per Hour ----
energy_load |>
    group_by(hour_int) |>
    summarise(mean_load = mean(load)) |>
    ggplot(aes(x = hour_int, y = mean_load)) +
    geom_point() +
    geom_line() +
    labs(
        x = "Hour of the day",
        y = "Mean Load",
        title = "Average Load over the years 2022-2024 per day",
    )

# --- Histogramm of the data ---
ggplot(energy_load, aes(load)) +
    geom_histogram(binwidth = 500) +
    labs(
        x = "Load in kWh",
        title = "Histogramm of all data points from 2022 - 2024",
    )


# --- Histogramm of week days/weekend days ---
energy_load <- energy_load |>
    mutate(working_day = !(weekday_int %in% c(6, 7)))

energy_load |>
    filter(working_day == TRUE) |>
    ggplot(aes(load)) +
    geom_histogram(binwidth = 500) +
    labs(
        title = "Histogramm of Loads for 2022 - 2024 for Working Days",
        x = "Load in kWh"
    )

energy_load |>
    filter(working_day == FALSE) |>
    ggplot(aes(load)) +
    geom_histogram(binwidth = 500) +
    labs(
        title = "Histogramm of Loads for 2022 - 2024 for Weekends",
        x = "Load in kWh",
    )

# ---  Filter for one specific week, for example the first week of the dataset ---
energy_load |>
    filter(year(date) == 2024 & week(date) == 1) |>
    ggplot(aes(x = date, y = load)) +
    geom_line() +
    labs(
        x = "Hour of the day",
        y = "Mean Load",
        title = "Load for one exemplary weeks",
    )

# --- Get the time series of loads for a given year and month ---
year <- 2024
month <- 1
# Create the figure
fig <- energy_load |>
    filter(year(date) == year & month_int == month) |>
    ggplot(aes(x = date, y = load)) +
    geom_line() +
    scale_x_datetime(
        date_breaks = "3 hours",
        date_labels = "%a - %R"
    ) +
    labs(
        x = "Hour of the day",
        y = "Load in kWh",
        title = paste("Load per hour for the year ", year, " and the month ", month)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
# Make it interactive
ggplotly(fig)

# --- Look at the load over the whole time period by months and grouped by working day ---
month <- 2
fig2 <- energy_load |>
    filter(month_int == month) |>
    ggplot(aes(hour_int, load, color = working_day)) +
    geom_point() +
    labs(
        x = "Hour of the day",
        y = "Load in kWh",
        title = "Load per hour for the period 2022 - 2024",
        subtitle = paste("grouped by working day and filtered by month ", month)
    )
fig2
# ggplotly(fig2)

energy_load |>
    filter(month_int == 12 & load == 36235.75)

# --- Summary Statistics ---
summary(energy_load)

paste("SD for load: ", sapply(energy_load, sd, na.rm = TRUE)[3], " kWh")


# --- Boxplot of load per day of the week from 2022 - 2024
ggplot(energy_load, aes(x = as.factor(weekday_int), y = load)) +
    geom_boxplot() +
    labs(x = "Weekday", y = "Load in kWh", title = "Load per weekday from 2022 - 2024")
