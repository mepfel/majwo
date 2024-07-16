library(tidyverse)
library(scoringRules)
library(ggplot2)
library(forecast)
library(plotly)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")


# ----- Train/Test Split ------
# Taking the years 2022 for training
df_train <- energy_load |>
    filter((year(date) == 2022))
# Taking 2023 for testing
df_test <- energy_load |>
    filter(year(date) == 2023)


# ----- Preparing weekday dummies -----
weekday_dummies <- model.matrix(~ factor(weekday_int), data = df_train)[, -1]
x_train <- as.matrix(cbind(weekday_dummies, df_train$is_holiday))
# Set column names, assuming the first columns are from weekday_dummies_test and the last is is_holiday
colnames(x_train) <- c(paste("weekday", 2:7, sep = "_"), "is_holiday")

# Auto Arima
model_auto <- auto.arima(df_train$load, xreg = x_train, seasonal = TRUE)
model_auto

checkresiduals(model_auto)
pacf(model_auto$residuals)


# ARIMA(3,1,2)
# TO-DO: Add seasonal component: seasonal = list(order = c(1, 1, 1))
model <- arima(df_train$load, c(3, 1, 3), xreg = weekday_dummies)

summary(model)

checkresiduals(model)
pacf(model$residuals)

# ---- Check for huge errors ---
df_train$resids <- as.numeric(model$residuals)
huge_errors <- df_train |>
    filter(abs(resids) > 4000)

# ----- Plotting the in sample fit ------
# Add predictions to df_train
df_train$predictions <- fitted(model)

# Plot
fig <- ggplot(df_train, aes(x = date)) +
    geom_line(aes(y = load, colour = "red")) +
    geom_line(aes(y = predictions, colour = "blue")) +
    labs(x = "Time", y = "Load", title = "Actual vs Predicted Load") +
    theme_minimal()
fig

ggplotly(fig)


# --------- TESTING ------------
# Preparing weekday dummies for test
weekday_dummies_test <- model.matrix(~ factor(weekday_int), data = df_test)[, -1]
x_test <- as.matrix(cbind(weekday_dummies_test, df_test$is_holiday))
# Set column names, assuming the first columns are from weekday_dummies_test and the last is is_holiday
colnames(x_test) <- c(paste("weekday", 2:7, sep = "_"), "is_holiday")


# Predicting 24 hours into the future
yhat_test <- as.numeric(predict(model, n.ahead = 24, newxreg = x_test[1:n, ])$pred)

# ------ PLOTTING the out of sample fit -------
# Melting the data for easier plotting with ggplot
plot_data_long <- reshape2::melt(plot_data, id.vars = "time")

# Plotting
ggplot(plot_data_long, aes(x = time, y = value, color = variable)) +
    geom_line() +
    labs(title = "Actual vs Predicted Load", x = "Time", y = "Load") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))



# ------ Saving the model -------

yhat_train <- as.numeric(fitted(model))

data <- data.frame(matrix(ncol = 0, nrow = nrow(df_train) + 24))

# Adjusting the 'data' dataframe
data$ds <- energy_load$date[1:(nrow(df_train) + 24)] # Ensuring 'ds' is the first column

# Actual values 'y'
data$y <- energy_load$load[1:(nrow(df_train) + 24)]

# Predicted values 'yhat'
data$yhat <- c(yhat_train, yhat_test)

# Calculating residuals for the training part
data$residuals <- data$y - data$yhat

write.csv(data, file = "./data/forecasts/load_22-24_model-arma.csv", row.names = FALSE)
