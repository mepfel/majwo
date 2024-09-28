library(tidyverse)
library(forecast)
library(randomForest)

# ---- Load the data ----
# For the holidays
holidays <- read.csv("./data/holidays_DE_15-24.csv") |>
    mutate_at("Date", as.Date)

# The energy data
energy_load <- read.csv("./data/load_15-24.csv") |>
    mutate_at(c("hour_int", "weekday_int", "month_int"), as.factor) |>
    mutate(is_holiday = if_else(as.Date(date) %in% holidays$Date, 1, 0))

energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# Getting the peaks
data <- data.frame(energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load)))

data <- data |>
    mutate( # calculate the yearly fourier term
        p1 = sin(2 * pi * yday(date) / 366),
        p2 = cos(2 * pi * yday(date) / 366),
        lag_1 = lag(load, 1),
        lag_2 = lag(load, 2),
        lag_3 = lag(load, 3),
        lag_4 = lag(load, 4),
        lag_5 = lag(load, 5),
        lag_6 = lag(load, 6),
        lag_7 = lag(load, 7),
        lag_8 = lag(load, 8),
        lag_9 = lag(load, 9),
        lag_10 = lag(load, 10),
        lag_11 = lag(load, 11),
        lag_12 = lag(load, 12),
        lag_13 = lag(load, 13),
        lag_14 = lag(load, 14),
        lag_15 = lag(load, 15),
        lag_16 = lag(load, 16),
        lag_17 = lag(load, 17),
        lag_18 = lag(load, 18),
        lag_19 = lag(load, 19),
        lag_20 = lag(load, 20),
        lag_21 = lag(load, 21)
    ) |>
    select(-month_int, -working_day, -as.Date.date.)

data <- na.omit(data)

# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}

#######################
# ---- ARIMAX ------- #
#######################
predict_arma <- function(data, d) {
    # INPUT:
    # data
    # d ...  day (has to be greater than 0)

    # length of the training period is 365 by default
    #### Data Manipulation

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

    test$y_hat <- yhat_test
    return(test)
}


# Run the predictons for some days
predictions <- data.frame()
pred_length <- 1400 # in days
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
store$y <- predictions$load

# Predicted values 'yhat'
store$yhat <- predictions$y_hat

# Calculating residuals for the training part
store$residuals <- store$y - store$yhat

write.csv(store, file = "./data/forecasts/thesis/peaks_model_arimax.csv", row.names = FALSE)


#####################
# ----- ARX ------- #
#####################

predict_ar7 <- function(data, d) {
    # INPUT:
    # data
    # d ...  day (has to be bigger than 0)

    # length of the training period is 365 by default
    #### Data Manipulation

    formula <- load ~ lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + weekday_int + is_holiday + p1 + p2

    # Train/Test Split
    train <- data[d:(364 + d), ]

    # ------- TRAINING ----------

    # AR-Model
    model <- lm(formula, data = train)

    # --------- TESTING ------------
    test <- data[(365 + d), ]

    yhat_test <- as.numeric(predict(model, newdata = test))

    # Transformation muss eventuell angepasst werden
    test$y_hat <- yhat_test
    return(test)
}


# Run the predictons for some days
predictions <- data.frame()
pred_length <- 1400 # in days
for (i in 1:pred_length) {
    print(i)
    value <- predict_ar7(data, i)
    predictions <- rbind(predictions, value)
}

# ---------- Storing the data ------------
store <- data.frame(matrix(ncol = 0, nrow = pred_length))

# Adjusting the 'data' dataframe
store$ds <- predictions$date # Ensuring 'ds' is the first column

# Actual values 'y'
store$y <- predictions$load

# Predicted values 'yhat'
store$yhat <- predictions$y_hat

# Calculating residuals for the training part
store$residuals <- store$y - store$yhat

write.csv(store, file = "./data/forecasts/thesis/peaks_model_arx.csv", row.names = FALSE)


#####################
# ------ RF ------- #
#####################

# Data Preprocessing
n <- 400
data <- data[(1:n), ]

# Loop through each row index of data
for (i in 1:n) {
    print(i)
    # Access the current row
    lags <- as.numeric(data[i, paste0("lag_", 1:21)])
    mean <- mean(lags)
    std <- sqrt(var(lags))

    # Subtract mean and divide by std for each lag value
    normalized_lags <- (lags - mean) / std

    # Store the results in new columns x_1 to x_21
    for (j in 1:21) {
        data[i, paste0("x_", j)] <- normalized_lags[j]
    }

    data[i, "y"] <- (data[i, "load"] - mean) / std


    data$mean[i] <- mean
    data$std[i] <- std
}

# Create the formula string
formula <- as.formula(paste("y ~", paste(c(paste0("x_", 1:21), "p1", "p2", "weekday_int", "is_holiday"), collapse = " + ")))

pred_length <- 10
predictions <- data.frame()
for (d in 1:pred_length) {
    print(d)
    train <- data[d:(364 + d), ]
    test <- data[(365 + d), ]
    rf <- randomForest(formula, data = train, ntree = 100)
    y_hat <- as.numeric(predict(rf, test))
    test$load_p <- y_hat * test$std + test$mean
    predictions <- rbind(predictions, test)
}

# ---------- Storing the data ------------
store <- data.frame(matrix(ncol = 0, nrow = (pred_length)))

# Adjusting the 'data' dataframe
store$ds <- predictions$date # Ensuring 'ds' is the first column

# Actual values 'y'
store$y <- predictions$load

# Predicted values 'yhat'
store$yhat <- predictions$load_p

# Calculating residuals for the training part
store$residuals <- store$y - store$yhat

write.csv(store, file = "./data/forecasts/thesis/peaks_model-rf.csv", row.names = FALSE)
