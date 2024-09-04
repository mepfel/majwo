library(tidyverse)
library(ggplot2)
library(forecast)

# For the holidays
holidays <- read.csv("./data/holidays_DE_15-24.csv") |>
    mutate_at("Date", as.Date)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_15-24.csv") |>
    mutate_at(c("hour_int", "weekday_int", "month_int"), as.factor) |>
    mutate(is_holiday = if_else(as.Date(date) %in% holidays$Date, 1, 0))

energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# Train/Test Split
# Taking the years 2022 for training
data <- energy_load |>
    filter(year(date) >= 2015)

# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}
# Log Transformation
# data <- data |>
#    group_by(hour_int) |>
#    mutate(
#        load_origin = load,
#        log_load = log(load),
#        mean_log_load = mean(log(load)),
#        load = log_load - mean_log_load
#    ) |>
#    ungroup()


# Fit the regression model
# Use One year for training
length_testing <- 365
train <- data[1:(length_testing * 24), ]

x_train <- c("is_holiday", "DoW_2", "DoW_3", "DoW_4", "DoW_5", "DoW_6", "DoW_7")

mean_h_log <- numeric(24)
# For every hour, one model
for (i in seq(0, 23)) {
    print(i)
    train_h <- train |>
        filter(hour(date) == i)
    mean_h_log[i + 1] <- mean(log(train_h$load))

    train_h$load <- log(train_h$load) - mean_h_log[i + 1]
    x_reg <- train_h |>
        select(all_of(x_train)) |>
        as.matrix()

    model <- auto.arima(train_h$load, xreg = x_reg, allowdrift = FALSE)
    assign(paste0("model_", i), model)
}

# Print the summary of the model
summary(model_1)

checkresiduals(model_13)
# If you want to use robust standard errors
coeftest(model, vcov = vcovHC(model, type = "HC1"))


# ------ TESTING ------
test <- data[((length_testing * 24) + 1):((length_testing * 24) + 24), ]
x_reg_new <- test |>
    select(all_of(x_train)) |>
    as.matrix()


predictions <- numeric(24)
for (i in 1:24) {
    print(i)
    model <- get(paste0("model_", (i - 1)))
    predictions[i] <- exp(as.numeric(predict(model, newxreg = t(x_reg_new[i, ]))$pred) + mean_h_log[i])
}
# Assuming 'date' is the vector of dates corresponding to your test data
# Create a new data frame for plotting
plot_data <- data.frame(date = test$date, Actual = test$load, Predicted = predictions)

# Melt the data frame for easier plotting with ggplot
plot_data_long <- reshape2::melt(plot_data, id.vars = "date")

# Plot
ggplot(plot_data_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    labs(title = "Actual vs Predicted Load", y = "Load", x = "Date") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"))


# -------- TRAIN/TEST/PREDICT -------
# "ds","y","yhat","residuals" are the columns for the final csv

predict_arima <- function(data, d) {
    # INPUT:
    # data
    # d ...  day

    # ------- TRAINING ----------

    # Fit the regression model
    # Use One year for training
    train <- data[(24 * d - 23):(((364 + d) * 24)), ]

    x_train <- c("is_holiday", "DoW_2", "DoW_3", "DoW_4", "DoW_5", "DoW_6", "DoW_7")

    # For every hour, one model
    for (i in seq(0, 23)) {
        print(i)
        train_h <- train |>
            filter(hour(date) == i)


        train_h$load <- log(train_h$load)
        x_reg <- train_h |>
            select(all_of(x_train)) |>
            as.matrix()

        model <- auto.arima(train_h$load, xreg = x_reg, allowdrift = FALSE)
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
        predictions[i] <- exp(as.numeric(predict(model, newxreg = t(x_reg_new[i, ]))$pred))
    }

    data_test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]
    data_test$y_hat <- predictions
    return(data_test)
}

# Run the predictons for some days
predictions <- data.frame(matrix(ncol = 15, nrow = 0))
pred_length <- 10 # in days
for (i in 1:pred_length) {
    print(i)
    value <- predict_arima(data, i)
    predictions <- rbind(predictions, value)
}


# -------- Plotting --------
# Assuming 'date' is the vector of dates corresponding to your test data
# Create a new data frame for plotting
plot_data <- data.frame(date = predictions$date, Actual = predictions$load, Predicted = predictions$y_hat)

# Melt the data frame for easier plotting with ggplot
plot_data_long <- reshape2::melt(plot_data, id.vars = "date")

# Plot
ggplot(plot_data_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    labs(title = "Actual vs Predicted Load", y = "Load", x = "Date") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"))



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

write.csv(store, file = "./data/forecasts/loads_22-24_model-arima-24.csv", row.names = FALSE)
