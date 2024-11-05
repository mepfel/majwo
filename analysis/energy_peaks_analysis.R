library(tidyverse)
library(ggplot2)
library(plotly)

# --- Load the energy data ---
energy_load <- read.csv("./data/load-AT_15-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# --- Getting the peaks ---
peaks <- energy_load |>
  group_by(as.Date(date)) |>
  slice(which.max(load)) |>
  filter((year(date) >= 2015) & (year(date) <= 2020) & (load > 5000))

# --- Time Series of the peaks ---
fig <- ggplot(peaks, aes(x = date, y = load)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Peak Load in MWh"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 16))

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
    x = "Hour of the day",
    y = "Count"
  )

# Only for working day
peaks |>
  filter(working_day == FALSE) |>
  ggplot(aes(load)) +
  geom_histogram(binwidth = 250) +
  labs(
    title = "Histogramm of Load peaks for 2022 - 2024 for Weekends",
    x = "Load in MWh",
  )

# Only for weekends
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
    x = "Hour of the day",
    y = "Load in MWh",
    color = "Working day"
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


# --- Boxplot of peaks grouped by month ---
ggplot(peaks, aes(x = as.factor(month_int), y = load)) +
  geom_boxplot() +
  labs(x = "Month", y = "Load in MWh") +
  theme_minimal() +
  theme(text = element_text(size = 16))



# ---- Plot of average peaks per month, one graph per year
# Calculate average monthly peak value
monthly_avg_peaks <- peaks |>
  group_by(year(date), month_int) |>
  summarize(avg_load = mean(load, na.rm = TRUE)) |>
  rename(year = `year(date)`)

# Plot
ggplot(monthly_avg_peaks, aes(x = month_int, y = avg_load, color = as.factor(year), group = year)) +
  geom_line() +
  labs(x = "Month", y = "Average Peak Load in MWh", color = "Year") +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal() +
  theme(text = element_text(size = 16))
