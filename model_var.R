library(tidyverse)
library(scoringRules)
library(vars)
library(ggplot2)
library(plotly)

# --- Load the energy data ---
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# --- Getting the peaks ---
peaks <- energy_load |>
  group_by(as.Date(date)) |>
  slice(which.max(load))


# --- Analysis for one year ---
peaks_2023 <- peaks |>
  filter(year(date) == 2023)

# --- Analysis working day ---
peaks_2023_w <- peaks |>
  filter(year(date) == 2023) |>
  filter(working_day == TRUE)

plot(peaks_2023_w$hour_int)

peaks_2023_nw <- peaks |>
  filter(year(date) == 2023) |>
  filter(working_day == FALSE)

plot(peaks_2023_nw$hour_int)

peaks_2023 <- peaks_2023[, c(2, 3)]

load <- peaks_2023$load

plot(load)
# PROBLEM: Time Series is not stationary!!!!
# Differntiate time series to make it stationary
diff_load <- diff(load, 7)

# Plot the differenced time series
plot(diff_load)
acf(diff_load, lag.max = 21)

plot(hour_int)

peaks_2023_diff <- data.frame(date = peaks_2023$date[-(1:7)], load = diff(peaks_2023$load, 7), hour_int = peaks_2023$hour_int[-(1:7)])

plot(peaks_2023_diff$load)
acf(peaks_2023_diff$load, lag.max = 365)

# A VAR Model here -----
lagselect <- VARselect(peaks_2023_diff[c(2, 3)], lag.max = 84)
lagselect$selection
fit <- VAR(peaks_2023_diff[c(2, 3)], p = 14)

summary(fit)


residuals <- residuals(fit)
fitted_values <- fitted(fit)

ggplot(residuals, aes(x = 1:length(load), y = load)) +
  geom_point() +
  geom_line() +
  labs(title = "Residuals from Load", x = "Time", y = "Residuals")

predict(fit, n.ahead = 1)

# Load
fig <- ggplot() +
  geom_line(aes(x = 1:length(fitted_values[, 1]), y = fitted_values[, 1]), color = "blue") +
  geom_line(aes(x = 1:(length(peaks_2023_diff$load) - 14), y = peaks_2023_diff$load[-(1:14)]), color = "red") +
  labs(title = "Fitted Values of Load and Diff_Load", x = "Time", y = "Fitted Values") +
  scale_color_manual(values = c("blue", "red"), labels = c("Load", "Diff_Load"))
fig

# Hour
fig <- ggplot() +
  geom_line(aes(x = 1:length(fitted_values[, 2]), y = fitted_values[, 2]), color = "blue") +
  geom_line(aes(x = 1:(length(peaks_2023_diff$hour_int) - 14), y = peaks_2023_diff$hour_int[-(1:14)]), color = "red") +
  labs(title = "Fitted Values of Hour_Int and Diff_Hour_Int", x = "Time", y = "Fitted Values") +
  scale_color_manual(values = c("blue", "red"), labels = c("Hour_Int", "Diff_Hour_Int"))
fig

ggplotly(fig)
