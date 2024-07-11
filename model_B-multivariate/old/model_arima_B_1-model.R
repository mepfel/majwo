library(tidyverse)
library(scoringRules)
library(ggplot2)
library(copula)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

load <- energy_load |>
    filter((year(date) == 2022) | (year(date) == 2023))
#    select(date, load)

# Plot the load and the diff for visual analysis
plot(load)
acf(load$load, lag.max = 170)
pacf(load$load, lag.max = 170)

# Examplary plot of the load distribution per hour
hour <- 2
# Histogramm
load |>
    filter(hour(date) == hour) |>
    ggplot(aes(load)) +
    geom_histogram()
# Time Series
load |>
    filter(hour(date) == hour) |>
    ggplot(aes(x = 1:length(load), y = load)) +
    geom_point() +
    geom_line()

# Create an ar-model

model_new <- arima(load$load, c(12, 0, 0), xreg = cbind(load$working_day, load$month_int))

load["resids"] <- residuals(model_new)
load["load_hat"] <- load$load - load$resids

load_test <- energy_load |>
    filter(year(date) == 2024)
prediction_24h <- as.numeric(predict(model_new, n.ahead = 24, newxreg = cbind(load_test[121:144, ]$working_day, load_test[1:24, ]$month_int))$pred)

ggplot() +
    geom_line(aes(x = 1:length(prediction_24h), y = prediction_24h), color = "blue") +
    geom_line(aes(x = 1:length(prediction_24h), y = load_test[1:24, ]$load))


summary(model_new)

load |>
    filter(year(date) == 2023 & month_int == 10) |>
    ggplot() +
    geom_line(aes(x = date, y = load)) +
    geom_line(aes(x = date, y = load_hat), color = "blue")


#--------------------------------- NOT IMPLEMENTED ---------------
load_diff$resids <- residuals(model_general)

for (i in seq(0, 23)) {
    print(i)
    load_h <- load_diff |>
        filter(hour(date) == i)

    mean <- mean(load_h$resids)
    std <- sqrt(var(load_h$resids))
    print(paste0("Mean: ", mean, "Std: ", std))
}

mu_sigma_arima <- data.frame(hour = seq(0, 23), mu = rep(0, 24), sigma = rep(0, 24))

for (i in seq(0, 23)) {
    print(i)
    load_h <- load_diff |>
        filter(hour(date) == i)
    mu_sigma_arima[i + 1, "mu"] <- mean(load_h$resids)
    mu_sigma_arima[i + 1, "sigma"] <- sqrt(var(load_h$resids))
}


# --------- Error Learning phase ------------
for (i in seq(0, 23)) {
    print(i)
    # get the hour data
    load_h <- load_diff |>
        filter(hour(date) == i)

    # Extract the residuals from the data
    resid <- data.frame(resid = load_h$resids)

    # Standardize the residuals
    resid <- resid |>
        mutate(resid_std = (resid - mu_sigma_arima[i + 1, "mu"]) / mu_sigma_arima[i + 1, "sigma"])

    # Generate the ECDF
    assign(paste0("ecdf_", i), ecdf(resid$resid_std))
}

# Generate the Inverse ECDF
# Input: p... Probabilty , i... hour
# Output: quantile
inverse_ecdf <- function(p, i) {
    quantile(get(paste0("ecdf_", i)), p, names = FALSE)
}

plot(ecdf_11)

# ---- Dependence Learning Phase ----

# Define the length m of the learning phase
m <- 90

# Initialize the training sample of the past m errors for every hour to learn the copula
X <- matrix(nrow = m, ncol = 0)

for (i in seq(0, 23)) {
    print(i)
    # get the hour data
    load_h <- load_diff |>
        filter(hour(date) == i)

    # get the errors
    epsilon <- as.vector(load_h$resids)

    # Select only the last m errors
    epsilon <- epsilon[(length(epsilon) - m):length(epsilon)]

    # standardize the errors
    epsilon_std <- (epsilon - mu_sigma_arima[i + 1, "mu"]) / mu_sigma_arima[i + 1, "sigma"]

    # Apply the ecdf
    ecdf <- get(paste0("ecdf_", i))
    epsilon <- ecdf(epsilon_std)
    X <- cbind(X, epsilon)
}


# Create the Copula and the Rank Matrix

# 1) Parametric Approch
# Fit a Copula
copula <- empCopula(X)
# Draw a sample from the copula
sample_draw <- rCopula(m, copula)
# Apply the rank function to get the Rank Matrix
R_copula <- apply(sample_draw, 2, rank)

# --- Auxilary Copula Functions ---
# Draw a random sample of 23 from the copula
test <- rCopula(m, copula)

# Get the probablilites for u values from the copula
# Alternative: C.n(c(0.5, 0.5), X)
pCopula(c(0.5, 0.5), copula)

# 2) Non-Parametric Approach
# Consider X as the sample from the empirical copula
# Apply the rank function to get the rank matrix
R_emp <- apply(X, 2, rank)

# ------- Prediction phase -------
# Generate the 24 univariate for every hour
# m defines the quantile level i.. 1- m with i/m+1 quantiles
forecast_t <- matrix(nrow = 24, ncol = m)
quantiles <- seq(1 / (m + 1), m / (m + 1), 1 / (m + 1))

prediction_24h <- as.numeric(predict(model_general, n.ahead = 24)$pred)

for (i in seq(0, 23)) {
    # Get the prediction for the next hour of the day and...
    # ...Revert the differencing
    predict_point <- prediction_24h[i + 1] + tail(load$load, n = 1) + tail(load$load, n = 167)[1] - tail(load$load, n = 168)[1]
    forecast_t[i + 1, ] <- predict_point + mu_sigma_arima[i + 1, "mu"] + inverse_ecdf(quantiles, i) * mu_sigma_arima[i + 1, "sigma"]
}
quantiles

# Dependence Learning: Pairing up the Copula
multivariate_forecast <- matrix(nrow = nrow(R_emp), ncol = ncol(R_emp))

for (i in seq(1:24)) {
    j <- 1
    for (r in R_emp[, i]) {
        print(r)
        multivariate_forecast[j, i] <- forecast_t[i, r]
        j <- j + 1
    }
}

plot(multivariate_forecast[5, ], type = "l")


# Plot the result
# Version 1
data <- data.frame(forecast_t_1 = forecast_t[1, ], forecast_t_10 = forecast_t[10, ], forecast_t_18 = forecast_t[18, ], quantiles = quantiles)

ggplot(data) +
    geom_line(aes(x = quantiles, y = forecast_t_1), color = "blue") +
    geom_line(aes(x = quantiles, y = forecast_t_10), color = "red") +
    geom_line(aes(x = quantiles, y = forecast_t_18), color = "green")


# Version 2
# Create a data frame with every second forecast
data <- data.frame(quantiles = quantiles)

# Add every second forecast to the data frame
for (i in seq(1, 24, by = 2)) {
    data[[paste0("forecast_t_", i)]] <- forecast_t[i, ]
}

# Convert the data frame to long format for plotting
library(reshape2)
data_long <- reshape2::melt(data, id.vars = "quantiles", variable.name = "forecast", value.name = "value")
# Plot the result
library(plotly)
fig <- ggplot(data_long, aes(x = quantiles, y = value, color = forecast)) +
    geom_line() +
    labs(color = "Forecast")
ggplotly(fig)

# --- Getting the peaks ---
peaks <- energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load))
