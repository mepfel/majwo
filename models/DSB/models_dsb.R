library(tidyverse)
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

data <- energy_load |>
    mutate( # Encode the season of the year
        p1 = sin(2 * pi * yday(date) / 366),
        p2 = cos(2 * pi * yday(date) / 366),
        lag_1 = lag(load, 1 * 24),
        lag_2 = lag(load, 2 * 24),
        lag_3 = lag(load, 3 * 24),
        lag_4 = lag(load, 4 * 24),
        lag_5 = lag(load, 5 * 24),
        lag_6 = lag(load, 6 * 24),
        lag_7 = lag(load, 7 * 24),
        lag_8 = lag(load, 8 * 24),
        lag_9 = lag(load, 9 * 24),
        lag_10 = lag(load, 10 * 24),
        lag_11 = lag(load, 11 * 24),
        lag_12 = lag(load, 12 * 24),
        lag_13 = lag(load, 13 * 24),
        lag_14 = lag(load, 14 * 24),
        lag_15 = lag(load, 15 * 24),
        lag_16 = lag(load, 16 * 24),
        lag_17 = lag(load, 17 * 24),
        lag_18 = lag(load, 18 * 24),
        lag_19 = lag(load, 19 * 24),
        lag_20 = lag(load, 20 * 24),
        lag_21 = lag(load, 21 * 24)
    ) |>
    select(-month_int, -working_day)

# Remove rows with NA values created by lagging
data <- na.omit(data)

# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}

#########################
# ------ ARIMAX ------- #
#########################
predict_arima <- function(data, d) {
    # INPUT:
    # data
    # d ...  day
    # training length is 365 days

    # ------- TRAINING ----------

    # Fit the regression model
    # Use One year for training
    train <- data[(24 * d - 23):(((364 + d) * 24)), ]

    x_train <- c("is_holiday", "DoW_2", "DoW_3", "DoW_4", "DoW_5", "DoW_6", "DoW_7", "p1", "p2")

    # For every hour, one model
    for (i in seq(0, 23)) {
        print(i)
        train_h <- train |>
            filter(hour(date) == i)

        x_reg <- train_h |>
            select(all_of(x_train)) |>
            as.matrix()

        # model <- auto.arima(train_h$load, xreg = x_reg, allowdrift = FALSE)
        # ARIMA (1,1,2)
        model <- arima(train_h$load, c(1, 1, 2), xreg = x_reg)
        assign(paste0("model_", i), model)
    }

    # --------- TESTING ------------
    # getting the next hour of data
    test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]

    x_reg_new <- test |>
        select(all_of(x_train)) |>
        as.matrix()

    predictions <- numeric(24)
    for (i in 1:24) {
        model <- get(paste0("model_", (i - 1)))
        predictions[i] <- as.numeric(predict(model, newxreg = t(x_reg_new[i, ]))$pred)
    }

    data_test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]
    data_test$y_hat <- predictions
    return(data_test)
}

# Run the predictons for some days
predictions <- data.frame()
pred_length <- 1400 # in days
for (i in 1:pred_length) {
    print(i)
    value <- predict_arima(data, i)
    predictions <- rbind(predictions, value)
}

# ---------- Storing the data ------------
store <- data.frame(matrix(ncol = 0, nrow = (pred_length * 24)))

# Adjusting the 'data' dataframe
store$ds <- predictions$date # Ensuring 'ds' is the first column

# Actual values 'y'
store$y <- predictions$load

# Predicted values 'yhat'
store$yhat <- predictions$y_hat

# Calculating residuals for the training part
store$residuals <- store$y - store$yhat

write.csv(store, file = "./data/forecasts/final/loads_model_arimax.csv", row.names = FALSE)

#####################
# ----- ARX ------- #
#####################
predict_expert <- function(data, d) {
    # INPUT:
    # data
    # d ...  day

    # ------- TRAINING ----------
    # Formula for the regression
    formula <- load ~ lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + weekday_int + is_holiday + p1 + p2

    # Fit the regression model
    # Use One year for training
    train <- data[(24 * d - 23):(((364 + d) * 24)), ]

    # For every hour, one model
    for (i in seq(0, 23)) {
        print(i)
        train_h <- train |>
            filter(hour(date) == i)

        model <- lm(formula, data = train_h)
        assign(paste0("model_", i), model)
    }

    # --------- TESTING ------------
    # getting the next hours of data
    test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]

    predictions <- numeric(24)
    for (i in 1:24) {
        print(i)
        model <- get(paste0("model_", (i - 1)))
        predictions[i] <- as.numeric(predict(model, newdata = test[i, ]))
    }
    # Remove rows with NA values created by lagging
    test <- na.omit(test)

    data_test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]
    data_test$y_hat <- predictions
    return(data_test)
}

# Run the predictons for some days
predictions <- data.frame()
pred_length <- 1400 # in days
for (i in 1:pred_length) {
    print(i)
    value <- predict_expert(data, i)
    predictions <- rbind(predictions, value)
}

# ---------- Storing the data ------------
store <- data.frame(matrix(ncol = 0, nrow = (pred_length * 24)))

# Adjusting the 'data' dataframe
store$ds <- predictions$date # Ensuring 'ds' is the first column

# Actual values 'y'
store$y <- predictions$load

# Predicted values 'yhat'
store$yhat <- predictions$y_hat

# Calculating residuals for the training part
store$residuals <- store$y - store$yhat

write.csv(store, file = "./data/forecasts/final/loads_model_arx.csv", row.names = FALSE)


#####################
# ------ RF ------- #
#####################

# Data Preprocessing
n <- (1400 + 365) * 24
data <- data[(1:n), ]

# Loop through each row index of data
for (i in 1:n) {
    print(i)
    # Access the current row
    lags <- as.numeric(data[i, (7:27)])
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

pred_length <- 1400
predictions <- data.frame(matrix(ncol = 52, nrow = 0))
for (d in 1:pred_length) {
    print(d)
    train <- data[(24 * d - 23):(((364 + d) * 24)), ]
    test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]

    # For every hour, one model
    for (i in seq(0, 23)) {
        print(i)
        train_h <- train |>
            filter(hour(date) == i)

        rf <- randomForest(formula, data = train_h, ntree = 100)

        y_hat <- as.numeric(predict(rf, test[(i + 1), ]))
        test[(i + 1), "load_p"] <- y_hat * test[(i + 1), "std"] + test[(i + 1), "mean"]
    }

    predictions <- rbind(predictions, test)

    if (d %% 500 == 0) {
        write.csv(predictions, file = "./data/forecasts/loads_16-19_model-rf-CACHE.csv", row.names = FALSE)
    }
}

# ---------- Storing the data ------------
store <- data.frame(matrix(ncol = 0, nrow = (pred_length * 24)))

# Adjusting the 'data' dataframe
store$ds <- predictions$date # Ensuring 'ds' is the first column

# Actual values 'y'
store$y <- predictions$load

# Predicted values 'yhat'
store$yhat <- predictions$load_p

# Calculating residuals for the training part
store$residuals <- store$y - store$yhat

write.csv(store, file = "./data/forecasts/final/loads_model_rf.csv", row.names = FALSE)
