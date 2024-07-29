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


# Lagged variables for the model
data <- data |>
    mutate(
        Y_d_1_h = lag(load, 1 * 24), # lag by 24 hours
        Y_d_2_h = lag(load, 2 * 24), # lag by 48 hours
        Y_d_7_h = lag(load, 7 * 24), # lag by 168 hours (1 week)
        Y_d_1_min = sapply(1:n(), function(i) ifelse(i > 24, min(load[(i - 24):(i - 1)]), NA)),
        Y_d_1_max = sapply(1:n(), function(i) ifelse(i > 24, max(load[(i - 24):(i - 1)]), NA)),
        Y_d_1_24 = lag(load, 1)
    )

# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}


# Remove rows with NA values created by lagging
data <- na.omit(data)

# Formula for the regression
formula <- load ~ Y_d_1_h + Y_d_2_h + Y_d_7_h + Y_d_1_min + Y_d_1_max + Y_d_1_24 +
    DoW_1 + DoW_6 + DoW_7 + is_holiday

# Fit the regression model
# Use One year for training
train <- data[1:(365 * 24), ]


# For every hour, one model
for (i in seq(0, 23)) {
    print(i)
    train_h <- train |>
        filter(hour(date) == i)

    model <- lm(formula, data = train_h)
    assign(paste0("model_", i), model)
}

# Print the summary of the model
summary(model_1)

checkresiduals(model_11)
# If you want to use robust standard errors
coeftest(model, vcov = vcovHC(model, type = "HC1"))

# ---- Check for huge errors ---
train$resids <- as.numeric(model$residuals)
huge_errors <- train |>
    filter(abs(resids) > 400)


# ------ TESTING ------
test <- data[((365 * 24) + 1):((365 * 24) + 24), ]

predictions <- numeric(24)
for (i in 1:24) {
    print(i)
    model <- get(paste0("model_", (i - 1)))
    predictions[i] <- as.numeric(predict(model, newdata = test[i, ]))
    # Shift Y_d_1_24 to Y_predicted
    test[i + 1, ]$Y_d_1_24 <- predictions[i]
}
# Remove rows with NA values created by lagging
test <- na.omit(test)

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
library(plotly)
ggplotly(fig)
