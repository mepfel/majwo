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



# --- Getting the peaks ---
peaks <- energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load)) |>
    as.data.frame()

data <- peaks |>
    filter(year(date) >= 2022)


# -----------------------------------------
# Log Transformation
data <- data |>
    mutate(
        load_origin = load,
        load = log(load)
    )

# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}


# ------- TRAINING ---------
# Train/Test Split
# Use One year for training
train <- data[1:365, ]

# AR-1
AR7 <- lm(load ~ lag(load, 1) + lag(load, 2) + lag(load, 3) + lag(load, 4) + lag(load, 5) + lag(load, 6) + lag(load, 7) + weekday_int + is_holiday, data = train)
summary(AR7)

coef(AR7)
checkresiduals(AR7)

pacf(AR7$residuals)
AIC(AR7)

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


# --------- TESTING ------------
n <- 7
test <- data[(365 + 1):(365 + n), ]
x_reg_new <- test |>
    select(all_of(x_train)) |>
    as.matrix()

yhat_test <- as.numeric(predict(model, n.ahead = n, newxreg = x_reg_new)$pred)

# Plot

# Assuming yhat_test has been calculated for the first n observations in df_test
# and df_test has at least n rows
plot_data <- data.frame(
    time = 1:n,
    Actual = test$load,
    Predicted = yhat_test
)


data <- data.frame(matrix(ncol = 0, nrow = nrow(df_train) + n))

# Adjusting the 'data' dataframe
data$ds <- peaks$date[1:(nrow(df_train) + n)] # Ensuring 'ds' is the first column

# Actual values 'y'
data$y <- peaks$load[1:(nrow(df_train) + n)]

# Predicted values 'yhat'
data$yhat <- c(yhat_train, yhat_test)

# Calculating residuals for the training part
data$residuals <- data$y - data$yhat

write.csv(data, file = "./data/forecasts/peaks_22-24_model-arma.csv", row.names = FALSE)


# ------ PLOTTING -------
# Melting the data for easier plotting with ggplot
plot_data_long <- reshape2::melt(plot_data, id.vars = "time")

# Plotting
ggplot(plot_data_long, aes(x = time, y = value, color = variable)) +
    geom_line() +
    labs(title = "Actual vs Predicted Load", x = "Time", y = "Load") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))


# TRAIN/TEST/PREDICT

predict_ar7 <- function(data, d) {
    # INPUT:
    # data
    # d ...  day (has to be bigger than 0)

    # length of the training period is 365 by default
    #### Data Manipulation
    # Log Transformation and lagging of load
    data <- data |>
        mutate(
            load_origin = load,
            load = log(load),
            Y_1 = lag(load, 1),
            Y_2 = lag(load, 2),
            Y_3 = lag(load, 3),
            Y_4 = lag(load, 4),
            Y_5 = lag(load, 5),
            Y_6 = lag(load, 6),
            Y_7 = lag(load, 7),
        )
    data <- na.omit(data)

    formula <- load ~ Y_1 + Y_2 + Y_3 + Y_4 + Y_5 + Y_6 + Y_7 + weekday_int + is_holiday

    # Train/Test Split
    train <- data[d:(364 + d), ]

    # ------- TRAINING ----------

    # AR-Model
    model <- lm(formula, data = train)

    # --------- TESTING ------------
    test <- data[(365 + d), ]

    yhat_test <- as.numeric(predict(model, newdata = test))

    test$y_hat <- exp(yhat_test)
    return(test)
}


# Run the predictons for some days
predictions <- data.frame(matrix(ncol = 17, nrow = 0))
pred_length <- 31 # in days
for (i in 1:pred_length) {
    print(i)
    value <- predict_ar7(peaks, i)
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

write.csv(store, file = "./data/forecasts/peaks_22-24_model-ar7.csv", row.names = FALSE)



# Reshape the dataframe to long format
predictions_long <- pivot_longer(predictions, cols = c(load_origin, y_hat), names_to = "type", values_to = "value")

# Plot
fig <- ggplot(predictions_long, aes(x = date, y = value, color = type)) +
    geom_line() + # Draw lines
    labs(x = "Date", y = "Value", title = "Load and Predicted Peak - AR7") +
    theme_minimal() # Use a minimal theme for aesthetics
fig
ggplotly(fig)

resids <- predictions$load_origin - predictions$y_hat

hist(resids, main = "Histogram of Residuals", xlab = "Residuals")
