library(tidyverse)
library(scoringRules)
library(ggplot2)
library(copula)

# --- Load the energy data ---
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

load <- energy_load |>
    filter((year(date) == 2022) & (month_int == 6)) |>
    select(date, load)

# Plot the load and the diff for visual analysis
plot(load)
acf(load$load, lag.max = 170)

load_diff <- data.frame(date = load$date[-(1:169)], load = diff(diff(load$load, 1), 168))

plot(load_diff$load)
acf(load_diff$load, lag.max = 336)

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

    # Generate the Inverse ECDF
    inverse_ecdf <- function(p) {
        quantile(ecdf_arima, p, names = FALSE)
    }
    assign(paste0("Iecdf_", i), inverse_ecdf)
}

plot(ecdf_0)

# ---- Dependence Learning Phase ----   NOT FULLY IMPLEMENTED
# TO-FIX: 0 is missing because it has not enough observations
X <- rep(1, 23)
for (i in seq(1, 23)) {
    print(i)
    # Get the model
    model <- get(paste0("model_", i))

    # get the errors
    epsilon <- as.vector(residuals(model))

    # standardize the errors
    epsilon_std <- (epsilon - mu_sigma_arima[i + 1, "mu"]) / mu_sigma_arima[i + 1, "sigma"]

    # Apply the ecdf
    ecdf <- get(paste0("ecdf_", i))
    epsilon <- ecdf(epsilon_std)
    X <- cbind(X, epsilon)
}
# Datenmatrix mit d x 24, d ... Länge der Dependence Learning Phase
# X <- cbind(epsilon_1, epsilon_2)

X <- X[, -1]
# Create the Empirical Copula
copula <- empCopula(X)

# --- Auxilary Copula Functions ---
# Draw a random sample of 23 from the copula
rCopula(23, copula)

# Get the probablilites for u values from the copula
# Alternative: C.n(c(0.5, 0.5), X)
pCopula(c(0.5, 0.5), copula)

# ------- Prediction phase -------- NOT FULLY IMPLEMENTED YET


# --- Getting the peaks ---
peaks <- energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load))
