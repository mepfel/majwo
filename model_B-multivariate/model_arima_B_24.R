library(tidyverse)
library(scoringRules)
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
# Taking the years 2023 for training
data <- energy_load |>
    filter(year(date) >= 2022)

# Log Transformation
data <- data |>
    group_by(hour_int) |>
    mutate(
        load_origin = load,
        log_load = log(load),
        mean_log_load = mean(log(load)),
        load = log_load - mean_log_load
    ) |>
    ungroup()

# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}

# Fit the regression model
# Use One year for training
length_testing <- 365
train <- data[1:(length_testing * 24), ]

x_train <- c("is_holiday", "DoW_2", "DoW_3", "DoW_4", "DoW_5", "DoW_6", "DoW_7")

# For every hour, one model
for (i in seq(0, 23)) {
    print(i)
    train_h <- train |>
        filter(hour(date) == i)

    x_reg <- train_h |>
        select(all_of(x_train)) |>
        as.matrix()

    model <- auto.arima(train_h$load, xreg = x_reg)
    assign(paste0("model_", i), model)
}

# Print the summary of the model
summary(model_1)

checkresiduals(model_10)
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
    predictions[i] <- as.numeric(predict(model, newxreg = t(x_reg_new[i, ]))$pred)
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


# Plot the untransformed data
mean <- train[1:24, "mean_log_load"]

predictions_T <- exp(predictions + mean)
# Create a new data frame for plotting
plot_data <- data.frame(date = test$date, Actual = test$load_origin, Predicted = predictions_T)

# Melt the data frame for easier plotting with ggplot
plot_data_long <- reshape2::melt(plot_data, id.vars = "date")

# Plot
ggplot(plot_data_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    labs(title = "Actual vs Predicted Load", y = "Load", x = "Date") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"))
