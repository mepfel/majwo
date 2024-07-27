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
weekday_dummies <- model.matrix(~ factor(weekday_int), data = data)[, -1]

for (i in 2:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i - 1]
}

# Create hour dummy variables
hour_dummies <- model.matrix(~ factor(hour_int), data = data)[, -1]

# Add hour dummy variables to data
for (i in 1:ncol(hour_dummies)) {
    data[[paste0("Hour_", i)]] <- hour_dummies[, i]
}


# Remove rows with NA values created by lagging
data <- na.omit(data)

# Formula for the regression
formula <- load ~ Y_d_1_h + Y_d_2_h + Y_d_7_h + Y_d_1_min + Y_d_1_max + Y_d_1_24 +
    DoW_2 + DoW_3 + DoW_4 + DoW_5 + DoW_6 + DoW_7 + Hour_1 + Hour_2 + Hour_3 + Hour_4 + Hour_5 + Hour_6 + Hour_7 + Hour_8 +
    Hour_9 + Hour_10 + Hour_11 + Hour_12 + Hour_13 + Hour_14 + Hour_15 +
    Hour_16 + Hour_17 + Hour_18 + Hour_19 + Hour_20 + Hour_21 + Hour_22 + Hour_23

# Fit the regression model
# Use One year for training
train <- data[1:(365 * 24), ]
model <- lm(formula, data = train)

# Print the summary of the model
summary(model)

checkresiduals(model)
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
