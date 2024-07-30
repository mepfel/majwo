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
    filter(year(date) >= 2022)


# Lagged variables for the model
data <- data |>
    mutate(
        Y_1_h = lag(load, 1),
        Y_2_h = lag(load, 2),
        Y_3_h = lag(load, 3),
        Y_4_h = lag(load, 4),
        Y_5_h = lag(load, 5),
        Y_6_h = lag(load, 6),
        Y_7_h = lag(load, 7),
        Y_8_h = lag(load, 8),
        Y_9_h = lag(load, 9),
        Y_10_h = lag(load, 10),
        Y_11_h = lag(load, 11),
        Y_12_h = lag(load, 12),
        Y_13_h = lag(load, 13),
        Y_14_h = lag(load, 14),
        Y_15_h = lag(load, 15),
        Y_16_h = lag(load, 16),
        Y_17_h = lag(load, 17),
        Y_18_h = lag(load, 18),
        Y_19_h = lag(load, 19),
        Y_20_h = lag(load, 20),
        Y_21_h = lag(load, 21),
        Y_22_h = lag(load, 22),
        Y_23_h = lag(load, 23),
        Y_24_h = lag(load, 24)
    )

# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}

# Create hour dummy variables - NOT NEEDED -
hour_dummies <- model.matrix(~ factor(hour_int), data = data)[, -1]

# Add hour dummy variables to data
for (i in 1:ncol(hour_dummies)) {
    data[[paste0("Hour_", i)]] <- hour_dummies[, i]
}

# Remove rows with NA values created by lagging
data <- na.omit(data) # Achtung!!! Bei Dokumentation genau auf Training und Test Split achten

# ----------- Model ---------------
# Formula for the regression
formula <- load ~ Y_1_h + Y_2_h + Y_3_h + Y_4_h + Y_5_h + Y_6_h +
    Y_7_h + Y_8_h + Y_9_h + Y_10_h + Y_11_h + Y_12_h +
    Y_13_h + Y_14_h + Y_15_h + Y_16_h + Y_17_h + Y_18_h +
    Y_19_h + Y_20_h + Y_21_h + Y_22_h + Y_23_h + Y_24_h +
    DoW_2 + DoW_3 + DoW_4 + DoW_5 + DoW_6 + DoW_7 + is_holiday

# Fit the regression model
# Use One year for training
train <- data[1:(365 * 24), ]
model <- lm(formula, data = train)

# Print the summary of the model
summary(model)

checkresiduals(model)

train$resids <- as.numeric(model$residuals)
huge_errors <- train |>
    filter(abs(resids) > 2000)

# ------ TESTING ------
# getting the next hour of data
test <- data[((365 * 24) + 1):((365 * 24) + 1), ]

predictions <- numeric(24)
for (i in 1:24) {
    predictions[i] <- as.numeric(predict(model, newdata = test))

    # Shift Y_1_h to Y_24_h one position to the right and insert the new prediction at Y_1_h
    test <- test %>%
        mutate(
            Y_24_h = Y_23_h,
            Y_23_h = Y_22_h,
            Y_22_h = Y_21_h,
            Y_21_h = Y_20_h,
            Y_20_h = Y_19_h,
            Y_19_h = Y_18_h,
            Y_18_h = Y_17_h,
            Y_17_h = Y_16_h,
            Y_16_h = Y_15_h,
            Y_15_h = Y_14_h,
            Y_14_h = Y_13_h,
            Y_13_h = Y_12_h,
            Y_12_h = Y_11_h,
            Y_11_h = Y_10_h,
            Y_10_h = Y_9_h,
            Y_9_h = Y_8_h,
            Y_8_h = Y_7_h,
            Y_7_h = Y_6_h,
            Y_6_h = Y_5_h,
            Y_5_h = Y_4_h,
            Y_4_h = Y_3_h,
            Y_3_h = Y_2_h,
            Y_2_h = Y_1_h,
            Y_1_h = predictions[i]
        )
}


# ------- Plotting the out of sample fit ---------

# Create a new data frame for plotting
data_test <- data[((365 * 24) + 1):((365 * 24) + 24), ]
plot_data <- data.frame(date = data_test$date, Actual = data_test$load, Predicted = predictions)

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

predict_ar24 <- function(data, d) {
    # INPUT:
    # data
    # d ...  day

    # ------- TRAINING ----------
    # Formula for the regression
    formula <- load ~ Y_1_h + Y_2_h + Y_3_h + Y_4_h + Y_5_h + Y_6_h +
        Y_7_h + Y_8_h + Y_9_h + Y_10_h + Y_11_h + Y_12_h +
        Y_13_h + Y_14_h + Y_15_h + Y_16_h + Y_17_h + Y_18_h +
        Y_19_h + Y_20_h + Y_21_h + Y_22_h + Y_23_h + Y_24_h +
        DoW_2 + DoW_3 + DoW_4 + DoW_5 + DoW_6 + DoW_7 + is_holiday

    # Fit the regression model
    # Use One year for training
    train <- data[(24 * d - 23):(((364 + d) * 24)), ]


    model <- lm(formula, data = train)

    # --------- TESTING ------------
    # getting the next hour of data
    test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24 - 23), ]

    predictions <- numeric(24)
    for (i in 1:24) {
        predictions[i] <- as.numeric(predict(model, newdata = test))

        # Shift Y_1_h to Y_24_h one position to the right and insert the new prediction at Y_1_h
        test <- test %>%
            mutate(
                Y_24_h = Y_23_h,
                Y_23_h = Y_22_h,
                Y_22_h = Y_21_h,
                Y_21_h = Y_20_h,
                Y_20_h = Y_19_h,
                Y_19_h = Y_18_h,
                Y_18_h = Y_17_h,
                Y_17_h = Y_16_h,
                Y_16_h = Y_15_h,
                Y_15_h = Y_14_h,
                Y_14_h = Y_13_h,
                Y_13_h = Y_12_h,
                Y_12_h = Y_11_h,
                Y_11_h = Y_10_h,
                Y_10_h = Y_9_h,
                Y_9_h = Y_8_h,
                Y_8_h = Y_7_h,
                Y_7_h = Y_6_h,
                Y_6_h = Y_5_h,
                Y_5_h = Y_4_h,
                Y_4_h = Y_3_h,
                Y_3_h = Y_2_h,
                Y_2_h = Y_1_h,
                Y_1_h = predictions[i]
            )
    }
    data_test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]
    data_test$y_hat <- predictions
    return(data_test)
}

# Run the predictons for some days
predictions <- data.frame(matrix(ncol = 62, nrow = 0))
pred_length <- 470 # in days
for (i in 1:pred_length) {
    print(i)
    value <- predict_ar24(data, i)
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

write.csv(store, file = "./data/forecasts/loads_22-24_model-ar24.csv", row.names = FALSE)


# -------------- Plotting ---------------
# Reshape the dataframe to long format
predictions_long <- pivot_longer(predictions, cols = c(load, y_hat), names_to = "type", values_to = "value")

# Plot
fig <- ggplot(predictions_long, aes(x = date, y = value, color = type)) +
    geom_line() + # Draw lines
    labs(x = "Date", y = "Value", title = "Load and Predicted Load Over Time - AR-24 with hourly Dummies") +
    theme_minimal() # Use a minimal theme for aesthetics
fig
library(plotly)
ggplotly(fig)


resids <- predictions$load - predictions$y_hat

hist(resids, main = "Histogram of Residuals", xlab = "Residuals - AR24")
