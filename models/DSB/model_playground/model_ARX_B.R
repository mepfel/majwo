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

# Taking the years 2015 and above
data <- energy_load |>
    filter(year(date) >= 2015) |>
    mutate(
        load_origin = load,
        load = log(load)
    ) |>
    ungroup()

# Lagged variables for the model
data <- data |>
    mutate(
        Y_d_1_h = lag(load, 1 * 24), # lag by 24 hours
        Y_d_2_h = lag(load, 2 * 24), # lag by 48 hours
        Y_d_3_h = lag(load, 3 * 24), # lag by 72 hours
        Y_d_4_h = lag(load, 4 * 24), # lag by 96 hours
        Y_d_5_h = lag(load, 5 * 24), # lag by 120 hours
        Y_d_6_h = lag(load, 6 * 24), # lag by 144 hours
        Y_d_7_h = lag(load, 7 * 24), # lag by 168 hours
        p1 = sin(2 * pi * yday(date) / 366),
        p2 = cos(2 * pi * yday(date) / 366)
    )

# Remove rows with NA values created by lagging
data <- na.omit(data)

# ---------------------------------------
# Formula for the regression
formula <- load ~ Y_d_1_h + Y_d_3_h + weekday_int + is_holiday

# Fit the regression model
# Use One year for training
train <- data[1:(365 * 24), ]
train <- data[(24 * 1 - 23):(((364 + 1) * 24)), ]


# For every hour, one model
for (i in seq(0, 23)) {
    print(i)
    train_h <- train |>
        filter(hour(date) == i)

    model <- lm(formula, data = train_h)
    assign(paste0("model_", i), model)
}

# Print the summary of the model
summary(model_18)

checkresiduals(model_1)

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


# -------- TRAIN/TEST/PREDICT -------
# "ds","y","yhat","residuals" are the columns for the final csv

predict_expert <- function(data, d) {
    # INPUT:
    # data
    # d ...  day

    # ------- TRAINING ----------
    # Formula for the regression
    formula <- load ~ Y_d_1_h + Y_d_2_h + Y_d_3_h + Y_d_4_h + Y_d_5_h + Y_d_6_h + Y_d_7_h + weekday_int + is_holiday + p1 + p2

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
    data_test$y_hat <- exp(predictions)
    return(data_test)
}

# Run the predictons for some days
predictions <- data.frame(matrix(ncol = 15, nrow = 0))
pred_length <- 1000 # in days
for (i in 1:pred_length) {
    print(i)
    value <- predict_expert(data, i)
    predictions <- rbind(predictions, value)
}

# -------------- Plotting ---------------
# Reshape the dataframe to long format
predictions_long <- pivot_longer(predictions, cols = c(load_origin, y_hat), names_to = "type", values_to = "value")

# Plot
fig <- ggplot(predictions_long, aes(x = date, y = value, color = type)) +
    geom_line() + # Draw lines
    labs(x = "Date", y = "Value", title = "Load and Predicted Load Over Time - Expert Model") +
    theme_minimal() # Use a minimal theme for aesthetics
fig

# ---------- Storing the data ------------
store <- data.frame(matrix(ncol = 0, nrow = (pred_length * 24)))

# Adjusting the 'data' dataframe
store$ds <- predictions$date # Ensuring 'ds' is the first column

# Actual values 'y'
store$y <- predictions$load_origin

# Predicted values 'yhat'
store$yhat <- predictions$y_hat

# Calculating residuals for the training part
store$residuals <- store$y - store$yhat

write.csv(store, file = "./data/forecasts/loads_16-18_model-arx.csv", row.names = FALSE)



library(plotly)
ggplotly(fig)
