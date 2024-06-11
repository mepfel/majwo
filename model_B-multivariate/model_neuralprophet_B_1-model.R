library(tidyverse)
library(scoringRules)
library(ggplot2)
library(copula)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

load_test <- energy_load |>
    filter((year(date) == 2024)) |>
    select(date, load)

model_np <- read.csv("./data/forecasts/load_22-24_model-neuralprophet_2024IsForecasted.csv")
str(model_np)
model_np$ds <- as.POSIXct(model_np$ds, tz = "UTC")

# Train/Test Split
# Taking the years 2022 to 2023 for training
df_train <- model_np |>
    filter((year(ds) == 2022) | (year(ds) == 2023))
df_test <- model_np |>
    filter(year(ds) == 2024)

# --------- Error Learning Phase ---------
mu_sigma_np <- data.frame(hour = seq(0, 23), mu = rep(0, 24), sigma = rep(0, 24))

for (i in seq(0, 23)) {
    print(i)
    load_h <- df_train |>
        filter(hour(ds) == i)

    mean <- mean(load_h$residuals)
    std <- sqrt(var(load_h$residuals))

    mu_sigma_np[i + 1, "mu"] <- mean
    mu_sigma_np[i + 1, "sigma"] <- std

    # Extract the residuals from the data
    resid <- data.frame(resid = load_h$residuals)

    # Standardize the residuals
    resid <- resid |>
        mutate(resid_std = (resid - mu_sigma_np[i + 1, "mu"]) / mu_sigma_np[i + 1, "sigma"])

    # Generate the ECDF
    assign(paste0("ecdf_", i), ecdf(resid$resid_std))
}


# Generate the Inverse ECDF
# Input: p... Probabilty , i... hour
# Output: quantile
inverse_ecdf <- function(p, i) {
    quantile(get(paste0("ecdf_", i)), p, names = FALSE)
}

plot(ecdf_23)

# ---- Dependence Learning Phase ----
# NOT IMPLEMENTED
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
m <- 90
forecast_t <- matrix(nrow = 24, ncol = m)
quantiles <- seq(1 / (m + 1), m / (m + 1), 1 / (m + 1))

predict_point <- df_test[1:24, "y_hat"]

for (i in seq(0, 23)) {
    # Get the prediction for the next hour of the day and the the errors to get distribution
    forecast_t[i + 1, ] <- predict_point[i + 1] + mu_sigma_np[i + 1, "mu"] + inverse_ecdf(quantiles, i) * mu_sigma_np[i + 1, "sigma"]
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


# -------- Plotting the Results ----------
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
