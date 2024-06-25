library(tidyverse)
library(scoringRules)
library(ggplot2)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# --- Getting the peaks ---
peaks <- energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load))

peaks_np <- read.csv("./data/forecasts/peaks_22-24_model-neuralprophet.csv")

str(peaks_np)
peaks_np$ds <- as.POSIXct(peaks_np$ds, tz = "UTC")

# Train/Test Split
# Taking the years 2022 to 2023 for training
df_train <- peaks_np |>
    filter((year(ds) == 2022) | (year(ds) == 2023))
df_test <- peaks_np |>
    filter(year(ds) == 2024)

# --------- Error Learning Phase ---------

# Standardize the residuals
mu_resid <- mean(df_train$residuals)
sigma_resid <- sqrt(var(df_train$residuals))

df_train$residuals_std <- (df_train$residuals - mu_resid) / sigma_resid

# hist(df_train$residuals_std, main = "Histogram of Standardized Residuals", xlab = "Standardized Residuals")

# Plot the standardized resids
ggplot(df_train, aes(residuals_std)) +
    geom_histogram(bins = 100)

# Generate the Empirical Distribution Function for the resids
ecdf_np <- ecdf(df_train$residuals_std)

plot(ecdf_np)

# ------- Prediction phase -------

# Define the inverse ECDF
# Input p is the probability and the return is the quantile
inverse_ecdf_np <- function(p) {
    quantile(ecdf_np, p, names = FALSE)
}

quantiles <- seq(0.01, 1, by = 0.01)
peaks_dis_A <- data.frame(quantiles = quantiles, values = rep(0, length(quantiles)))


for (i in quantiles) {
    # Destandardize the error for the qunatile i
    quantile_resid <- (inverse_ecdf_np(i) * sigma_resid) + mu_resid
    peaks_dis_A[peaks_dis_A$quantiles == i, "values"] <- df_test$yhat[1] + quantile_resid
}

# Plot the distribution
peaks_dis_A |>
    ggplot(aes(values, quantiles)) +
    geom_line()

# Histogram
hist(peaks_dis_A[, "values"], main = "Histogram of Peaks Distribution A", xlab = "Peaks", breaks = "Sturges")

peaks_dis_A[peaks_dis_A$quantiles == 0.9, "values"]

# CRPS-Score
crps_sample(as.numeric(df_test[1, 2]), peaks_dis_A[, "values"])
# First Run: CRPS = 16216.24
