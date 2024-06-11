library(tidyverse)
library(scoringRules)
library(ggplot2)
library(copula)

# --- Load the energy data ----
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

load <- energy_load |>
    filter((year(date) == 2022)) |>
    select(date, load)

# Plot the load and the diff for visual analysis
plot(load)
# Question: What do I want to model here? -> Right now it is only White Noise?
acf(load$load, lag.max = 170)
pacf(load$load, lag.max = 170)

load_diff <- data.frame(date = load$date[-(1:169)], load = diff(diff(load$load, 1), 168))

plot(load_diff$load)
acf(load_diff$load, lag.max = 336)
pacf(load_diff$load, lag.max = 336)


# Examplary plot of the load distribution per hour
hour <- 2
# Histogramm
load_diff |>
    filter(hour(date) == hour) |>
    ggplot(aes(load)) +
    geom_histogram()
# Time Series
load_diff |>
    filter(hour(date) == hour) |>
    ggplot(aes(x = 1:length(load), y = load)) +
    geom_point() +
    geom_line()

# ---- Specify arima model (NOT USED)---
min.aic <- Inf

choose.arma <- function(data) {
    print("Possible Models -------------------------------------------------------------")
    for (p in 0:5) {
        for (q in 0:5) {
            temp.model <- arima(data, c(p, 0, q), method = "ML")
            temp.aic <- temp.model$aic

            print(paste0("ARMA(", p, ",", q, ")     AIC = ", temp.aic))
            if (temp.aic < min.aic) {
                opt.p <- p
                opt.q <- q
                min.aic <- temp.aic
                opt.model <- temp.model
            }
        }
    }

    print("")
    print("Optimal Model -------------------------------------------------------------")
    print(paste0("ARMA(", opt.p, ",", opt.q, ")     AIC = ", min.aic))
    return(opt.model)
}

# choose.arma(load_diff$load)
# choose.arma(load_1$load)

mu_sigma_arima <- data.frame(hour = seq(0, 23), mu = rep(0, 24), sigma = rep(0, 24))

# Build an arima model for every hour
# COMMENT: The original paper creates one model for the whole load time series and another one for the errors!!!
# Try this approach as well!

for (i in seq(0, 23)) {
    print(i)
    load_h <- load_diff |>
        filter(hour(date) == i)

    model <- arima(load_h$load, c(1, 0, 4), method = "ML")
    mu_sigma_arima[i + 1, "mu"] <- model$coef["intercept"]
    mu_sigma_arima[i + 1, "sigma"] <- sqrt(model$sigma2)
    assign(paste0("model_", i), model)
}

# Plot the residuals
plot(residuals(model_4))

# --------- Error Learning phase ------------
for (i in seq(0, 23)) {
    print(i)
    # Get the model
    model <- get(paste0("model_", i))

    # Extract the residuals from the model
    resid <- data.frame(resid = as.vector(residuals(model)))

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

plot(ecdf_5)

# ---- Dependence Learning Phase ----

# Define the length m of the learning phase
m <- 90

# Initialize the training sample of the past m errors for every hour to learn the copula
X <- matrix(nrow = m, ncol = 0)

for (i in seq(0, 23)) {
    print(i)
    # Get the model
    model <- get(paste0("model_", i))

    # get the errors
    epsilon <- as.vector(residuals(model))

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
for (i in seq(0, 23)) {
    # Get the prediction for the next day and...
    # ...Revert the differencing
    model <- get(paste0("model_", i))
    predict_point <- as.numeric(predict(model, n.ahead = 1)$pred) + tail(load$load, n = 1) + tail(load$load, n = 167)[1] - tail(load$load, n = 168)[1]
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
