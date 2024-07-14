library(tidyverse)
library(scoringRules)
library(ggplot2)
library(forecast)
library(dynlm)

# For the holidays
holidays <- read.csv("./data/holidays_DE_15-24.csv") |>
    mutate_at("Date", as.Date)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_22-24.csv") |>
    mutate_at(c("hour_int", "weekday_int", "month_int"), as.factor) |>
    mutate(is_holiday = if_else(as.Date(date) %in% holidays$Date, 1, 0))

energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")



# Train/Test Split
# Taking the years 2022 to 2023 for training
df_train <- energy_load |>
    filter(year(date) %in% c(2022))

df_test <- energy_load |>
    filter(year(date) == 2024)

sum(df_train$is_holiday)

# ------- TRAINING ---------

# AR-24
AR24 <- dynlm(ts(df_train$load) ~ L(ts(df_train$load)) + L(ts(df_train$load), 2:24) + df_train$weekday_int + df_train$is_holiday)
summary(AR24)

coef(AR24)
checkresiduals(AR24)

pacf(AR24$residuals)
AIC(AR24)

weekday_dummies <- model.matrix(~ factor(weekday_int), data = df_train)[, -1]
x_train <- as.matrix(cbind(weekday_dummies, df_train$is_holiday))
# Set column names, assuming the first columns are from weekday_dummies_test and the last is is_holiday
colnames(x_train) <- c(paste("weekday", 2:7, sep = "_"), "is_holiday")

# Auto ARIMA
arima1 <- auto.arima(df_train$load, xreg = x_train)
arima1

data <- as.tibble(df_train)
str(data)

# ARIMA
model <- arima(df_train$load, c(3, 1, 3), xreg = x_train)

summary(model)

checkresiduals(model)
pacf(model$residuals)

df_train$resids <- as.numeric(model$residuals)


# Check for the huge errors
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

library(plotly)
ggplotly(fig)


# --------- TESTING ------------

weekday_dummies_test <- model.matrix(~ factor(weekday_int), data = df_test)[, -1]

x_test <- as.matrix(cbind(weekday_dummies_test, df_test$is_holiday))
# Set column names, assuming the first columns are from weekday_dummies_test and the last is is_holiday
colnames(x_test) <- c(paste("weekday", 2:7, sep = "_"), "is_holiday")

n <- 62
yhat_test <- as.numeric(predict(model, n.ahead = n, newxreg = x_test[1:n, ])$pred)

# Plot

# Assuming yhat_test has been calculated for the first n observations in df_test
# and df_test has at least n rows
plot_data <- data.frame(
    time = 1:n,
    Actual = df_test$load[1:n],
    Predicted = yhat_test
)




yhat_train <- as.numeric(fitted(model))

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
