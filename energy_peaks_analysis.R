library(tidyverse)
library(ggplot2)
library(plotly)

# --- Load the energy data ---
energy_load <- read.csv("./data/load_15-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# --- Getting the peaks ---
peaks <- energy_load |>
  group_by(as.Date(date)) |>
  slice(which.max(load))

# --- Time Series of the peaks ---
fig <- ggplot(peaks, aes(x = date, y = load)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Time Series of the Peak Load from 2015 to 2024",
    x = "Date",
    y = "Peak Load in MWh"
  )
fig
ggplotly(fig)

# --- Histogramm of the peaks ---
ggplot(peaks, aes(x = load)) +
  geom_histogram(binwidth = 250) +
  labs(
    title = "Histogramm of load peaks from 2022 to 2024",
    x = "Load in MWh"
  )

# Per hour
ggplot(peaks, aes(hour_int)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Histogramm of load peaks from 2022 to 2024 per 15 min",
    x = "Time in 15 min intervalls"
  )

# Grouped by working day
peaks |>
  filter(working_day == FALSE) |>
  ggplot(aes(load)) +
  geom_histogram(binwidth = 250) +
  labs(
    title = "Histogramm of Load peaks for 2022 - 2024 for Weekends",
    x = "Load in MWh",
  )

peaks |>
  filter(working_day == TRUE) |>
  ggplot(aes(load)) +
  geom_histogram(binwidth = 250) +
  labs(
    title = "Histogramm of Load peaks for 2022 - 2024 for Weekdays",
    x = "Load in MWh",
  )

# --- Density of the peaks ---
ggplot(peaks, aes(x = load)) +
  geom_density() +
  labs(
    title = "Density of the load peaks 2022 - 2024",
    x = "Load in MWh"
  )

# --- Scatterplot of the load peaks grouped by working days
ggplot(peaks, aes(x = hour_int, y = load, color = working_day)) +
  geom_point() +
  scale_x_continuous(breaks = seq(min(peaks$hour_int), max(peaks$hour_int), by = 1)) +
  labs(
    title = "Scatterplot of Load peaks from 2022 - 2024",
    x = "Hour of the day",
    y = "Load in MWh"
  )

# --- Bivariate Density of Load peaks ---
library(MASS)
library(plotly)

# --- Density of energy load on hour resolution ---
# Estimation of bivariate density
den3d <- kde2d(energy_load$load, energy_load$hour_int)
plot_ly(x = den3d$x, y = den3d$y, z = den3d$z) |>
  add_surface()

# Density of load peaks over the week
den3d <- kde2d(peaks$weekday_int, peaks$load)
plot_ly(x = den3d$x, y = den3d$y, z = den3d$z) |> add_surface()
