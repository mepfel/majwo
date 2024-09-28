library(tidyverse)
library(ggplot2)
library(forecast)
library(plotly)

# For the holidays
holidays <- read.csv("./data/holidays_DE_15-24.csv") |>
    mutate_at("Date", as.Date)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_15-24.csv") |>
    mutate_at(c("hour_int", "weekday_int", "month_int"), as.factor) |>
    mutate(is_holiday = if_else(as.Date(date) %in% holidays$Date, 1, 0)) |>
    mutate(
        p1 = sin(2 * pi * yday(date) / 366),
        p2 = cos(2 * pi * yday(date) / 366)
    )

energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")


# --- Getting the peaks ---
peaks <- energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load)) |>
    as.data.frame()


data <- peaks |>
    filter(year(date) >= 2015)

# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}
# ---------------------------------------
x_train <- c("is_holiday", "DoW_2", "DoW_3", "DoW_4", "DoW_5", "DoW_6", "DoW_7")
x_reg <- train |>
    select(all_of(x_train)) |>
    as.matrix()

# Auto ARIMA
arima1 <- auto.arima(train$load, xreg = x_reg)
arima1

# ARIMA
model <- arima(train$load, c(1, 1, 1), xreg = x_reg)

summary(model)

checkresiduals(model)

train$resids <- as.numeric(model$residuals)

# -------------------------------------

predict_arma <- function(data, d) {
    # INPUT:
    # data
    # d ...  day (has to be bigger than 0)

    # length of the training period is 365 by default
    #### Data Manipulation
    # Log Transformation
    data <- data |>
        mutate(
            load_origin = load,
            load = log(load)
        )

    # Train/Test Split
    train <- data[d:(364 + d), ]

    # ------- TRAINING ----------
    # Set up the X matrix for training
    x_train <- c("is_holiday", "DoW_2", "DoW_3", "DoW_4", "DoW_5", "DoW_6", "DoW_7", "p1", "p2")
    x_reg <- train |>
        select(all_of(x_train)) |>
        as.matrix()

    # ARIMA
    model <- arima(train$load, c(1, 1, 1), xreg = x_reg)

    # --------- TESTING ------------
    test <- data[(365 + d), ]
    # New test data
    x_reg_new <- test |>
        select(all_of(x_train)) |>
        as.matrix()

    yhat_test <- as.numeric(predict(model, n.ahead = 1, newxreg = x_reg_new)$pred)
    # Transformation could be an issue!!!!
    test$y_hat <- exp(yhat_test)
    return(test)
}


# Run the predictons for some days
predictions <- data.frame(matrix(ncol = 17, nrow = 0))
pred_length <- 1000 # in days
for (i in 1:pred_length) {
    print(i)
    value <- predict_arma(data, i)
    predictions <- rbind(predictions, value)
}

# ---------- Storing the data ------------
store <- data.frame(matrix(ncol = 0, nrow = pred_length))

# Adjusting the 'data' dataframe
store$ds <- predictions$date # Ensuring 'ds' is the first column

# Actual values 'y'
store$y <- predictions$load_origin

# Predicted values 'yhat'
store$yhat <- predictions$y_hat

# Calculating residuals for the training part
store$residuals <- store$y - store$yhat

write.csv(store, file = "./data/forecasts/peaks_16-18_model-arimax.csv", row.names = FALSE)



# Reshape the dataframe to long format
predictions_long <- pivot_longer(predictions, cols = c(load_origin, y_hat), names_to = "type", values_to = "value")

# Plot
fig <- ggplot(predictions_long, aes(x = date, y = value, color = type)) +
    geom_line() + # Draw lines
    labs(x = "Date", y = "Value", title = "Load and Predicted Load Peaks - ARIMA(1,1,1)") +
    theme_minimal() # Use a minimal theme for aesthetics
fig
ggplotly(fig)

resids <- predictions$load_origin - predictions$y_hat

hist(resids, main = "Histogram of Residuals", xlab = "Residuals")
