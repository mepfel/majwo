library(tidyverse)
library(scoringRules)
library(ggplot2)
library(forecast)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# --- Getting the peaks ---
peaks <- energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load))

# Train/Test Split
# Taking the years 2022 to 2023 for training
df_train <- peaks |>
    filter((year(date) == 2022) | (year(date) == 2023))
df_test <- peaks |>
    filter(year(date) == 2024)


weekday_dummies <- model.matrix(~ factor(weekday_int), data = df_train)[, -1]
# ARIMA(0,1,2)
model <- arima(df_train$load, c(0, 1, 2), xreg = weekday_dummies)

summary(model)

weekday_dummies_test <- model.matrix(~ factor(weekday_int), data = df_test)[, -1]
yhat_test <- as.numeric(predict(model, n.ahead = 28, newxreg = weekday_dummies_test[1:28, ])$pred)

yhat_train <- as.numeric(fitted(model))

data <- data.frame(matrix(ncol = 0, nrow = nrow(df_train) + 28))

# Adjusting the 'data' dataframe
data$ds <- peaks$date[1:(nrow(df_train) + 28)] # Ensuring 'ds' is the first column

# Actual values 'y'
data$y <- peaks$load[1:(nrow(df_train) + 28)]

# Predicted values 'yhat'
data$yhat <- c(yhat_train, yhat_test)

# Calculating residuals for the training part
data$residuals <- data$y - data$yhat

write.csv(data, file = "./data/forecasts/peaks_22-24_model-arma.csv", row.names = FALSE)
