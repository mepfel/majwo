library(tidyverse)
library(scoringRules)
library(ggplot2)
library(forecast)
library(dynlm)

# For the holidays
holidays <- read.csv("./data/holidays_DE_15-24.csv") |>
    mutate_at("Date", as.Date)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_15-24.csv") |>
    mutate_at(c("hour_int", "weekday_int", "month_int"), as.factor) |>
    mutate(is_holiday = if_else(as.Date(date) %in% holidays$Date, 1, 0))

energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")
data <- energy_load |>
    filter(year(date) >= 2022)

train <- msts(data[1:(365 * 24), ]$load, seasonal.period = c(24, 7 * 24))

# ------- Fourier Model -----
z <- fourier(train, K = c(5, 5))
holidays <- data[1:(365 * 24), ]$is_holiday
x_train <- cbind(z, holidays)

# Time duration to fit: 102 sec.
fit <- auto.arima(train, seasonal = FALSE, xreg = cbind(z, holidays))

zf <- fourier(train, K = c(5, 5), h = 24)
holidaysf <- data[((365 * 24) + 1):((365 * 24) + 24), ]$is_holiday

predictions <- as.numeric(predict(fit, newxreg = cbind(zf, holidaysf))$pred)

# Assuming 'date' is the vector of dates corresponding to your test data
# Create a new data frame for plotting
plot_data <- data.frame(date = data[((365 * 24) + 1):((365 * 24) + 24), ]$date, Actual = data[((365 * 24) + 1):((365 * 24) + 24), ]$load, Predicted = predictions)

# Melt the data frame for easier plotting with ggplot
plot_data_long <- reshape2::melt(plot_data, id.vars = "date")

# Plot
ggplot(plot_data_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    labs(title = "Actual vs Predicted Load", y = "Load", x = "Date") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"))
