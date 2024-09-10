library(tidyverse)
library(ggplot2)
library(reshape2)
library(randomForest)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_15-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# Create data according to R4
data <- energy_load |>
    mutate( # Encode the season of the year
        p1 = sin(2 * pi * yday(date) / 366),
        p2 = cos(2 * pi * yday(date) / 366),
        lag_1 = lag(load, 1 * 24),
        lag_2 = lag(load, 2 * 24),
        lag_3 = lag(load, 3 * 24),
        lag_4 = lag(load, 4 * 24),
        lag_5 = lag(load, 5 * 24),
        lag_6 = lag(load, 6 * 24),
        lag_7 = lag(load, 7 * 24),
        lag_8 = lag(load, 8 * 24),
        lag_9 = lag(load, 9 * 24),
        lag_10 = lag(load, 10 * 24),
        lag_11 = lag(load, 11 * 24),
        lag_12 = lag(load, 12 * 24),
        lag_13 = lag(load, 13 * 24),
        lag_14 = lag(load, 14 * 24),
        lag_15 = lag(load, 15 * 24),
        lag_16 = lag(load, 16 * 24),
        lag_17 = lag(load, 17 * 24),
        lag_18 = lag(load, 18 * 24),
        lag_19 = lag(load, 19 * 24),
        lag_20 = lag(load, 20 * 24),
        lag_21 = lag(load, 21 * 24)
    ) |>
    select(-month_int, -working_day)
data <- na.omit(data)

n <- 8760 * 4
data <- data[(1:n), ]

# Loop through each row index of data
for (i in 1:n) {
    print(i)
    # Access the current row
    lags <- as.numeric(data[i, (7:27)])
    mean <- mean(lags)
    std <- sqrt(var(lags))

    # Subtract mean and divide by std for each lag value
    normalized_lags <- (lags - mean) / std

    # Store the results in new columns x_1 to x_21
    for (j in 1:21) {
        data[i, paste0("x_", j)] <- normalized_lags[j]
    }

    data[i, "y"] <- (data[i, "load"] - mean) / std


    data$mean[i] <- mean
    data$std[i] <- std
}

# Create the formula string
formula <- as.formula(paste("y ~", paste(c(paste0("x_", 1:21), "p1", "p2", "hour_int", "weekday_int"), collapse = " + ")))

pred_length <- 1460
predictions <- data.frame(matrix(ncol = 52, nrow = 0))
for (d in 1:pred_length) {
    print(d)
    train <- data[(24 * d - 23):(((364 + d) * 24)), ]
    test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]
    rf <- randomForest(formula, data = train, ntree = 100)
    y_hat <- as.numeric(predict(rf, test))
    test$load_p <- y_hat * test$std + test$mean
    predictions <- rbind(predictions, test)
    if( d  %% 500) {
        write.csv(predictions, file = "./data/forecasts/loads_16-19_model-rf-CACHE.csv", row.names = FALSE)
    }
}

# ---------- Storing the data ------------
store <- data.frame(matrix(ncol = 0, nrow = (pred_length * 24)))

# Adjusting the 'data' dataframe
store$ds <- predictions$date # Ensuring 'ds' is the first column

# Actual values 'y'
store$y <- predictions$load

# Predicted values 'yhat'
store$yhat <- predictions$load_p

# Calculating residuals for the training part
store$residuals <- store$y - store$yhat

write.csv(store, file = "./data/forecasts/loads_16-19_model-rf.csv", row.names = FALSE)


# Reshape the test data frame to long format
test_long <- melt(predictions, id.vars = "date", measure.vars = c("load", "load_p"), variable.name = "Type", value.name = "Value")

# Create the plot using ggplot2
fig <- ggplot(test_long, aes(x = date, y = Value, color = Type)) +
    geom_line() +
    labs(title = "Actual vs Predicted Load", x = "Date", y = "Load") +
    theme_minimal() +
    scale_color_manual(values = c("blue", "red"), labels = c("Actual Load", "Predicted Load"))

library(plotly)
ggplotly(fig)
