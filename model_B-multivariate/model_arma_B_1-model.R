library(tidyverse)
library(scoringRules)
library(ggplot2)
library(forecast)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")


# Train/Test Split
# Taking the years 2022 to 2023 for training
df_train <- energy_load |>
    filter((year(date) == 2022) | (year(date) == 2023))
df_test <- energy_load |>
    filter(year(date) == 2024)


weekday_dummies <- model.matrix(~ factor(weekday_int), data = df_train)[, -1]

# Replace the manual ARIMA model with an automatic ARIMA model
# model <- auto.arima(df_train$load, xreg = weekday_dummies)

# ARIMA(3,1,2)

model <- arima(df_train$load, c(3, 1, 2), xreg = weekday_dummies)

summary(model)

weekday_dummies_test <- model.matrix(~ factor(weekday_int), data = df_test)[, -1]
yhat_test <- as.numeric(predict(model, n.ahead = 28 * 24, newxreg = weekday_dummies_test[1:(28 * 24), ])$pred)

yhat_train <- as.numeric(fitted(model))

data <- data.frame(matrix(ncol = 0, nrow = nrow(df_train) + 28 * 24))

# Adjusting the 'data' dataframe
data$ds <- energy_load$date[1:(nrow(df_train) + 28 * 24)] # Ensuring 'ds' is the first column

# Actual values 'y'
data$y <- energy_load$load[1:(nrow(df_train) + 28 * 24)]

# Predicted values 'yhat'
data$yhat <- c(yhat_train, yhat_test)

# Calculating residuals for the training part
data$residuals <- data$y - data$yhat

write.csv(data, file = "./data/forecasts/load_22-24_model-arma.csv", row.names = FALSE)
