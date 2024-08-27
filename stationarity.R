library(tidyverse)
library(ggplot2)
library(forecast)
library(tseries)

# For the holidays
holidays <- read.csv("./data/holidays_DE_15-24.csv") |>
    mutate_at("Date", as.Date)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_15-24.csv") |>
    mutate_at(c("hour_int", "weekday_int", "month_int"), as.factor) |>
    mutate(is_holiday = if_else(as.Date(date) %in% holidays$Date, 1, 0))

energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")


# ---------- PEAK ANALYSIS ------
# --- Getting the peaks ---
peaks <- energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load)) |>
    as.data.frame()


data <- peaks |>
    filter((year(date) >= 2022) & (year(date) < 2023))


# -----------------------------------------
# Log Transformation
data <- data |>
    mutate(
        load_origin = load,
        load = log(load)
    )
# ---------------------------


# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}



plot(stl(ts(data$load, frequency = 365), "periodic"))

data_fourier <- ts(data$load, frequency = 365)
fourier_terms <- fourier(data_fourier, K = 2)


# Calculate the sum of the Fourier terms
sum_fourier_terms <- rowSums(fourier_terms)

# Create a data frame for plotting
fourier_data <- data.frame(
    index = 1:length(sum_fourier_terms),
    sum_fourier = sum_fourier_terms
)

# Plot the sum of Fourier terms
p_fourier <- ggplot(fourier_data, aes(x = index, y = sum_fourier)) +
    geom_line() +
    labs(
        title = "Sum of Fourier Terms",
        x = "Index",
        y = "Sum of Fourier Terms"
    )

# Display the plot
print(p_fourier)


statio <- lm(data$load ~ data$DoW_6 + data$DoW_7 + data$is_holiday + fourier_terms)

summary(statio)

checkresiduals(statio)

adf_test <- adf.test(statio$residuals)
print(adf_test)


# ----------- Load Analysis per hour -------

data <- energy_load |>
    filter((year(date) >= 2022) & (year(date) < 2023)) |>
    filter(hour_int == 3)

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



data_fourier <- ts(data$load, frequency = 365)
fourier_terms <- fourier(data_fourier, K = 2)


# Calculate the sum of the Fourier terms
sum_fourier_terms <- rowSums(fourier_terms)

statio <- lm(data$load ~ data$weekday_int + data$is_holiday + fourier_terms)

summary(statio)

checkresiduals(statio)


# Perform the Augmented Dickey-Fuller test for stationarity

adf_test <- adf.test(statio$residuals)
print(adf_test)


# ----------- Load Analysis general -------

data <- energy_load |>
    filter((year(date) >= 2022) & (year(date) < 2023))

# Weekhour
data$weekhour <- (as.numeric(data$weekday_int) - 1) * 24 + as.numeric(data$hour_int)

# Log Transformation
data <- data |>
    mutate(
        load_origin = load,
        load = log(load)
    )

# Difference Smoothing (Trafo)
# set calibration window in weeks
N <- 5
t_0 <- 168 * N + 1
data_ds <- data.frame(x_t = data$load, date = data$date)

data_ds$t1 <- 0
data_ds$t2 <- 0
data_ds$t3 <- 0
data_ds$y <- NaN

for (t in t_0:(t_0 + 1000)) {
    # term_1
    t1 <- 0
    for (i in 1:(N - 1)) {
        t1 <- t1 + data_ds$x_t[t - (i * 168)]
    }
    data_ds$t1[t] <- t1 / (N - 1)

    # term 2
    t2 <- 0
    for (i in 1:7) {
        t2 <- t2 + data_ds$x_t[t - (i * 24)]
    }
    data_ds$t2[t] <- t2 / 7

    # term 3
    t3 <- 0
    for (i in 1:(N - 1)) {
        for (j in 1:7) {
            t3 <- t3 + data_ds$x_t[t - (i * 168) - (j * 24)]
        }
    }
    data_ds$t3[t] <- t3 / (7 * (N - 1))

    data_ds$y[t] <- data_ds$x_t[t] - data_ds$t1[t] - data_ds$t2[t] + data_ds$t3[t]
}
# Remove NaN values created by lagging
data_ds <- na.omit(data_ds)

# Log Transformation per hour mean
data <- data |>
    group_by(hour_int) |>
    mutate(
        load_origin = load,
        load = log(load),
        mean_log_load = mean(log(load)),
        load = load - mean_log_load
    ) |>
    ungroup()

# Create weekday dummy variables
weekday_dummies <- model.matrix(~ factor(weekday_int) - 1, data = data)

for (i in 1:7) {
    data[[paste0("DoW_", i)]] <- weekday_dummies[, i]
}

data_fourier_day <- ts(data$load, frequency = 24)
data_fourier_week <- ts(data$load, frequency = 24 * 7)
data_fourier_year <- ts(data$load, frequency = 24 * 365)
fourier_terms_day <- fourier(data_fourier_day, K = 2)
fourier_terms_week <- fourier(data_fourier_week, K = 2)
fourier_terms_year <- fourier(data_fourier_year, K = 2)


statio <- lm(data$load ~ data$is_holiday + fourier_terms_day + fourier_terms_week + fourier_terms_year + as.numeric(data$weekday_int))

statio <- lm(diff(data$load, 1) ~ data$weekday_int[-1] + data$is_holiday[-1] + data$hour_int[-1] + data$month_int[-1])
statio <- lm(data$load ~ data$weekhour + data$is_holiday + data$month_int)

summary(statio)

checkresiduals(statio)
# Perform the Augmented Dickey-Fuller test for stationarity
library(tseries)
adf_test <- adf.test(first_1000_residuals)
print(adf_test)

# Plot the fist n observations
n <- 1000
# Extract the first n residuals
first_1000_residuals <- statio$residuals[1:n]
first_1000_residuals <- data_ds$y

# Create a data frame for plotting
residuals_data <- data.frame(
    index = data$date[1:n],
    residuals = first_1000_residuals
)

# Plot the first 1000 residuals
p_residuals <- ggplot(residuals_data, aes(x = index, y = residuals)) +
    geom_line(color = "blue") +
    labs(
        title = "First 1000 Residuals from statio",
        x = "Index",
        y = "Residuals"
    )

# Display the plot
print(p_residuals)
mean(first_1000_residuals)
# Make it interactive
library(plotly)
ggplotly(p_residuals)

acf(diff(first_1000_residuals))
