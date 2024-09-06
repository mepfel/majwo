library(tidyverse)
library(ggplot2)
library(forecast)
library(lmtest)

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
    filter(year(date) >= 2015) |>
    mutate(
        load_origin = load,
        load = log(load)
    )

# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}

# --------------------------------------
d <- 5
# Use One year for training
length_testing <- 364

train <- data[(24 * d - 23):(((364 + d) * 24)), ]
test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]


# Tested but brings no improvement
# statio <- lm(load ~ weekday_int + is_holiday + fourier_terms_year, data = train)


x_train <- c("is_holiday", "DoW_2", "DoW_3", "DoW_4", "DoW_5", "DoW_6", "DoW_7")

# For every hour, one model
for (i in seq(0, 23)) {
    print(i)
    train_h <- train |>
        filter(hour(date) == i)

    x_reg <- train_h |>
        select(all_of(x_train)) |>
        as.matrix()

    model <- auto.arima(train_h$load, xreg = x_reg, allowdrift = FALSE)
    assign(paste0("model_", i), model)
}

# Print the summary of the model
summary(model_1)
coeftest(model_1)
checkresiduals(model_19)


# ------ TESTING ------

x_reg_new <- test |>
    select(all_of(x_train)) |>
    as.matrix()

predictions <- numeric(24)
for (i in 1:24) {
    print(i)
    model <- get(paste0("model_", (i - 1)))

    predictions[i] <- exp(as.numeric(predict(model, newxreg = t(x_reg_new[i, ]))$pred))
}

# Assuming 'date' is the vector of dates corresponding to your test data
# Create a new data frame for plotting
plot_data <- data.frame(date = test$date, Actual = test$load_origin, Predicted = predictions)

# Melt the data frame for easier plotting with ggplot
plot_data_long <- reshape2::melt(plot_data, id.vars = "date")

# Plot
ggplot(plot_data_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    labs(title = "Actual vs Predicted Load", y = "Load", x = "Date") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"))



# -------- TRAIN/TEST/PREDICT -----------------
# "ds","y","yhat","residuals" are the columns for the final csv

predict_arima <- function(data, d) {
    # INPUT:
    # data
    # d ...  day
    # training length is 365 days

    # ------- TRAINING ----------

    # Fit the regression model
    # Use One year for training
    train <- data[(24 * d - 23):(((364 + d) * 24)), ]

    x_train <- c("is_holiday", "DoW_2", "DoW_3", "DoW_4", "DoW_5", "DoW_6", "DoW_7")

    # For every hour, one model
    for (i in seq(0, 23)) {
        print(i)
        train_h <- train |>
            filter(hour(date) == i)

        x_reg <- train_h |>
            select(all_of(x_train)) |>
            as.matrix()

        model <- auto.arima(train_h$load, xreg = x_reg, allowdrift = FALSE)
        assign(paste0("model_", i), model)
    }

    # --------- TESTING ------------
    # getting the next hour of data
    test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]

    x_reg_new <- test |>
        select(all_of(x_train)) |>
        as.matrix()

    predictions <- numeric(24)
    for (i in 1:24) {
        model <- get(paste0("model_", (i - 1)))
        predictions[i] <- exp(as.numeric(predict(model, newxreg = t(x_reg_new[i, ]))$pred))
    }

    data_test <- data[(24 * d + 365 * 24 - 23):(24 * d + 365 * 24), ]
    data_test$y_hat <- predictions
    return(data_test)
}

# Run the predictons for some days
predictions <- data.frame(matrix(ncol = 15, nrow = 0))
pred_length <- 2 # in days
for (i in 1:pred_length) {
    print(i)
    value <- predict_arima(data, i)
    predictions <- rbind(predictions, value)
}


# -------- Plotting --------
# Assuming 'date' is the vector of dates corresponding to your test data
# Create a new data frame for plotting
plot_data <- data.frame(date = predictions$date, Actual = predictions$load, Predicted = predictions$y_hat)

# Melt the data frame for easier plotting with ggplot
plot_data_long <- reshape2::melt(plot_data, id.vars = "date")

# Plot
fig <- ggplot(plot_data_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    labs(title = "Actual vs Predicted Load", y = "Load", x = "Date") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"))
fig
library(plotly)
ggplotly(fig)
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

write.csv(store, file = "./data/forecasts/loads_22-24_model-arima-24.csv", row.names = FALSE)
