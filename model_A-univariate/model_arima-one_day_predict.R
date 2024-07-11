library(tidyverse)
library(scoringRules)
library(ggplot2)
library(forecast)
library(dynlm)
library(plotly)

# For the holidays
holidays <- read.csv("./data/holidays_DE_15-24.csv") |>
    mutate_at("Date", as.Date)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_15-24.csv") |>
    mutate_at(c("hour_int", "weekday_int", "month_int"), as.factor) |>
    mutate(is_holiday = if_else(as.Date(date) %in% holidays$Date, 1, 0))

energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")


# --- Getting the peaks ---
peaks <- energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load))

predict_arma <- function(data, d) {
    # INPUT:
    # data
    # d ...  day

    # length of the training period is 365 by default

    # Train/Test Split
    train <- data[d:(364 + d), ]
    test <- data[(365 + d), ]
    weekday_dummies <- model.matrix(~ factor(weekday_int), data = data)[, -1]

    # ------- TRAINING ----------
    # Set up the X matrix for training
    x_train <- as.matrix(cbind(weekday_dummies[d:(364 + d), ], train$is_holiday))
    # Set column names, assuming the first columns are from weekday_dummies_test and the last is is_holiday
    colnames(x_train) <- c(paste("weekday", 2:7, sep = "_"), "is_holiday")

    # ARIMA
    model <- arima(train$load, c(1, 1, 1), xreg = x_train)

    # --------- TESTING ------------
    x_test <- as.matrix(c(weekday_dummies[(365 + d), ], test$is_holiday))
    # Set column names, assuming the first columns are from weekday_dummies_test and the last is is_holiday
    rownames(x_test) <- c(paste("weekday", 2:7, sep = "_"), "is_holiday")
    yhat_test <- as.numeric(predict(model, n.ahead = 1, newxreg = t(x_test))$pred)

    test$y_hat <- yhat_test
    return(test)
}


# Run the predictons for some days
predictions <- data.frame(matrix(ncol = 9, nrow = 0))
for (i in 1:365) {
    print(i)
    value <- predict_arma(peaks, i)
    predictions <- rbind(predictions, value)
}


# Reshape the dataframe to long format
predictions_long <- pivot_longer(predictions, cols = c(load, y_hat), names_to = "type", values_to = "value")

# Plot
fig <- ggplot(predictions_long, aes(x = date, y = value, color = type)) +
    geom_line() + # Draw lines
    labs(x = "Date", y = "Value", title = "Load and Predicted Load Over Time") +
    theme_minimal() # Use a minimal theme for aesthetics

ggplotly(fig)

resids <- predictions$load - predictions$y_hat

hist(resids, main = "Histogram of Residuals", xlab = "Residuals")
