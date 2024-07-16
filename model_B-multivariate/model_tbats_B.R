library(tidyverse)
library(scoringRules)
library(ggplot2)
library(forecast)
library(dynlm)
library(fable)

# For the holidays
holidays <- read.csv("./data/holidays_DE_15-24.csv") |>
    mutate_at("Date", as.Date)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_15-24.csv") |>
    mutate_at(c("hour_int", "weekday_int", "month_int"), as.factor) |>
    mutate(is_holiday = if_else(as.Date(date) %in% holidays$Date, 1, 0))

energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# Take approx 2 weeks for training
demand <- msts(energy_load[500:836, ]$load, seasonal.period = c(24, 7 * 24))

# ---- TBATS ----
fit <- tbats(demand)
checkresiduals(fit)
plot(forecast(fit, h = 24))

predictions <- as.numeric(forecast(fit, h = 24)$mean)
