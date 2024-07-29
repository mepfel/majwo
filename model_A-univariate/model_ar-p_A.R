library(tidyverse)
library(scoringRules)
library(ggplot2)
library(forecast)
library(dynlm)

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
AR1 <- lm(load ~ lag(load, 1) + weekday_int + is_holiday, data = train)
summary(AR1)

coef(AR1)
checkresiduals(AR1)

pacf(AR1$residuals)
AIC(AR1)

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
