library(tidyverse)
library(ggplot2)
library(plotly)

energy_load <- read.csv("./data/load_15-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# Plot the whole data
ggplot(energy_load, aes(x = date, y = load)) +
    geom_line() +
    labs(
        x = "Date",
        y = "Load",
        title = "Energy Load from 2015 - 2024"
    ) +
    theme_minimal()

# Plot the distribution of energy load
ggplot(energy_load, aes(x = load)) +
    geom_histogram(bins = 50, fill = "blue", color = "black", alpha = 0.7) +
    geom_density(color = "red", size = 1) +
    ggtitle("Distribution of Energy Load") +
    xlab("Load") +
    ylab("Frequency") +
    theme_minimal()

# ---- Average Load per Hour ----
energy_load |>
    group_by(hour_int) |>
    summarise(mean_load = mean(load)) |>
    ggplot(aes(x = hour_int, y = mean_load)) +
    geom_point() +
    geom_line() +
    labs(
        x = "Hour of the day",
        y = "Mean Load in MWh",
    )

# --- Histogramm of the data ---
ggplot(energy_load, aes(load)) +
    geom_histogram(binwidth = 500) +
    geom_density(color = "red", size = 1) +
    labs(
        x = "Load in MWh",
        title = "Histogramm of all data points from 2022 - 2024",
    ) +
    theme_minimal()


# --- Histogramm of week days/weekend days ---

energy_load |>
    filter(working_day == TRUE) |>
    ggplot(aes(load)) +
    geom_histogram(binwidth = 500) +
    labs(
        title = "Histogramm of Loads for 2022 - 2024 for Working Days",
        x = "Load in MWh"
    )

energy_load |>
    filter(working_day == FALSE) |>
    ggplot(aes(load)) +
    geom_histogram(binwidth = 500) +
    labs(
        title = "Histogramm of Loads for 2022 - 2024 for Weekends",
        x = "Load in MWh",
    )

# ---  Filter for one specific week, for example the first week of the dataset ---
energy_load |>
    filter(year(date) == 2024 & week(date) == 1) |>
    ggplot(aes(x = date, y = load)) +
    geom_line() +
    labs(
        x = "Hour of the day",
        y = "Load",
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
        y = "Load in MWh",
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
        y = "Load in MWh",
        title = "Load per hour for the period 2022 - 2024",
        subtitle = paste("grouped by working day and filtered by month ", month)
    )
fig2
# ggplotly(fig2)

# --- Summary Statistics ---
summary(energy_load)

paste("SD for load: ", sapply(energy_load, sd, na.rm = TRUE)[3], " MWh")



# --- Boxplot of load per day of the week from 2015 - 2024
ggplot(energy_load, aes(x = as.factor(weekday_int), y = load)) +
    geom_boxplot() +
    labs(x = "Month", y = "Load in MWh")


energy_load |>
    filter(year(date) == 2023) |>
    ggplot(aes(x = as.factor(month_int), y = load)) +
    geom_boxplot() +
    labs(x = "Month", y = "Load in MWh")

# ---- 24 Histograms of Energy Load by Hour ----
ggplot(energy_load, aes(x = load)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
    facet_wrap(~hour_int, scales = "free_y") +
    labs(
        x = "Load",
        y = "Frequency",
        title = "Histogram of Energy Load by Hour"
    ) +
    theme_minimal()


# ---- 24 Histograms of Energy Load by Hour ----
ggplot(peaks, aes(x = load)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
    facet_wrap(~weekday_int, scales = "free_y") +
    labs(
        x = "Load",
        y = "Frequency",
        title = "Histogram of Peaks per Day"
    ) +
    theme_minimal()
