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
energy_load <- read.csv("./data/load_22-24.csv") |>
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

    # length of the training period is 365 days by default

    # Train/Test Split
    # Take 365 days from day d on - Remeber we have 24 hours = 1 day
    train <- data[(24 * d - 23):(((364 + d) * 24)), ]
    # Get the next 24 hours after the testing period
    test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]

    weekday_dummies <- model.matrix(~ factor(weekday_int), data = data)[, -1]

    # ------- TRAINING ----------
    # Set up the X matrix for training
    x_train <- as.matrix(cbind(weekday_dummies[(24 * d - 23):(((364 + d) * 24)), ], train$is_holiday))
    # Set column names, assuming the first columns are from weekday_dummies_test and the last is is_holiday
    colnames(x_train) <- c(paste("weekday", 2:7, sep = "_"), "is_holiday")

    # ARIMA - CHANGE MODEL here
    model <- arima(train$load, c(3, 1, 3), xreg = x_train)


    # --------- TESTING ------------
    x_test <- as.matrix(cbind(weekday_dummies[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ], test$is_holiday))
    # Set column names, assuming the first columns are from weekday_dummies_test and the last is is_holiday
    colnames(x_test) <- c(paste("weekday", 2:7, sep = "_"), "is_holiday")

    # Predict new values with MODEL
    yhat_test <- as.numeric(predict(model, n.ahead = 24, newxreg = x_test)$pred)

    test$y_hat <- yhat_test
    return(test)
}


# Run the predictons for some days
predictions <- data.frame(matrix(ncol = 9, nrow = 0))
for (i in 1:2) {
    print(i)
    value <- predict_arma(energy_load, i)
    predictions <- rbind(predictions, value)
}


# Reshape the dataframe to long format
predictions_long <- pivot_longer(predictions, cols = c(load, y_hat), names_to = "type", values_to = "value")

# Plot
fig <- ggplot(predictions_long, aes(x = date, y = value, color = type)) +
    geom_line() + # Draw lines
    labs(x = "Date", y = "Value", title = "Load and Predicted Load Over Time") +
    theme_minimal() # Use a minimal theme for aesthetics
fig
ggplotly(fig)

resids <- predictions$load - predictions$y_hat

hist(resids, main = "Histogram of Residuals", xlab = "Residuals")
