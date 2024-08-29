library(tidyverse)
library(scoringRules)
library(ggplot2)
library(tseries)

# --- Load the energy data ---
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# --- Getting the peaks ---
peaks <- energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load))

# peak_load <- peaks |>
# filter(year(date) == 2023)

peak_load <- peaks$load

# Plot the peak for visual analysis
plot(peak_load)
acf(peak_load, lag.max = 21)
pacf(peak_load, lag.max = 21)

# Build an arima model
model <- arima(peak_load, c(7, 0, 1), method = "ML")

# Plot the residuals
plot(residuals(model))

# --------- Error Learning phase ------------
# Extract the residuals from the model
resid <- data.frame(resid = as.vector(residuals(model)))
# Standardize the residuals
mu_resid <- model$coef["intercept"]
sigma_resid <- sqrt(model$sigma2)

resid <- resid |>
    mutate(resid_std = (resid - mu_resid) / sigma_resid)

# Plot the standardized resids
ggplot(resid, aes(resid_std)) +
    geom_histogram(bins = 100)

# Generate the Empirical Distribution Function for the resids
ecdf_arima <- ecdf(resid$resid_std)

plot(ecdf_arima)

# ------- Prediction phase --------

# Define the inverse ECDF
# Input p is the probability and the return is the quantile
inverse_ecdf_arima <- function(p) {
    quantile(ecdf_arima, p, names = FALSE)
}

quantiles <- seq(0.01, 1, by = 0.01)
peak_load_predict_dist <- data.frame(quantiles = quantiles, values = rep(0, length(quantiles)))

# Get the point forecast for the next day and add the peak load from 7 days before (bc of differencing)
point_forecast_arima <- as.numeric(predict(model, n.ahead = 1)$pred) + tail(peak_load, n = 7)[1]

for (i in quantiles) {
    # Destandardize the error for the qunatile i
    quantile_resid <- (inverse_ecdf_arima(i) * sigma_resid) + mu_resid
    peak_load_predict_dist[peak_load_predict_dist$quantiles == i, "values"] <- point_forecast_arima + quantile_resid
}

# Plot the distribution
peak_load_predict_dist |>
    ggplot(aes(values, quantiles)) +
    geom_line()

peak_load_predict_dist[peak_load_predict_dist$quantiles == 0.9, "values"]
peak_load_predict_dist[peak_load_predict_dist$quantiles == 0.1, "values"]
