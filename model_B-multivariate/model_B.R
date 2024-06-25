library(tidyverse)
library(scoringRules)
library(ggplot2)
library(copula)
# This is a first general implementation of version B

# --- Load the energy data ----
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# Load the model data
# Here: NeuralProphet
model <- read.csv("./data/forecasts/load_22-24_model-neuralprophet_2024IsForecasted.csv")
model$ds <- as.POSIXct(model$ds, tz = "UTC")

# specify the length for the error learning phase in days
length <- 365
getCRPS <- function(d) {
    # --------- Error Learning Phase ---------
    # Getting the test data: starting from day i get the next 365 days
    df_train <- model[((d - 1) * 24 + 1):(((364 + d) * 24) - 1), ]

    mu_sigma <- data.frame(hour = seq(0, 23), mu = rep(0, 24), sigma = rep(0, 24))

    for (i in seq(0, 23)) {
        load_h <- df_train |>
            filter(hour(ds) == i)

        mean <- mean(load_h$residuals)
        std <- sqrt(var(load_h$residuals))

        mu_sigma[i + 1, "mu"] <- mean
        mu_sigma[i + 1, "sigma"] <- std

        # Extract the residuals from the data
        resid <- data.frame(resid = load_h$residuals)

        # Standardize the residuals
        resid <- resid |>
            mutate(resid_std = (resid - mu_sigma[i + 1, "mu"]) / mu_sigma[i + 1, "sigma"])

        # Generate the ECDF
        assign(paste0("ecdf_", i), ecdf(resid$resid_std))
    }


    # Generate the Inverse ECDF
    # Input: p... Probabilty , i... hour
    # Output: quantile
    inverse_ecdf <- function(p, i) {
        quantile(get(paste0("ecdf_", i)), p, names = FALSE)
    }
    print("Error Learning DONE...")

    # ---- Dependence Learning Phase ----
    # Define the length m of the learning phase
    m <- 90

    # Initialize the training sample of the past m errors for every hour to learn the copula
    X <- matrix(nrow = m, ncol = 0)

    for (i in seq(0, 23)) {
        # get the hour data
        load_h <- df_train |>
            filter(hour(ds) == i)

        # get the errors
        epsilon <- as.vector(load_h$residuals)

        # Select only the last m errors
        epsilon <- epsilon[(length(epsilon) - m):length(epsilon)]

        # standardize the errors
        epsilon_std <- (epsilon - mu_sigma[i + 1, "mu"]) / mu_sigma[i + 1, "sigma"]

        # Apply the ecdf
        ecdf <- get(paste0("ecdf_", i))
        epsilon <- ecdf(epsilon_std)
        X <- cbind(X, epsilon)
    }


    # Create the Copula and the Rank Matrix

    # 2) Non-Parametric Approach
    # Consider X as the sample from the empirical copula
    # Apply the rank function to get the rank matrix
    R_emp <- apply(X, 2, rank)
    print("Dependence Learning DONE...")

    # ------- Prediction phase -------

    # Generate the 24 univariate for every hour
    # m also defines the quantile level i.. 1- m with i/m+1 quantiles
    univariate_forecast_t <- matrix(nrow = 24, ncol = m)
    quantiles <- seq(1 / (m + 1), m / (m + 1), 1 / (m + 1))

    # Get the next 24 hours after the testing period
    df_test <- model[((d + length - 1) * 24):((d + length) * 24 - 1), ]

    predict_point <- df_test[, "y_hat"]

    for (i in seq(0, 23)) {
        # Get the prediction for the next hour of the day and the the errors to get distribution
        univariate_forecast_t[i + 1, ] <- predict_point[i + 1] + mu_sigma[i + 1, "mu"] + inverse_ecdf(quantiles, i) * mu_sigma[i + 1, "sigma"]
    }


    # Dependence Learning: Pairing up the Copula
    multivariate_forecast <- matrix(nrow = nrow(R_emp), ncol = ncol(R_emp))

    for (i in seq(1:24)) {
        j <- 1
        for (r in R_emp[, i]) {
            multivariate_forecast[j, i] <- univariate_forecast_t[i, r]
            j <- j + 1
        }
    }
    print("Prediction DONE...")

    # Extracting the peaks from the multivariate distribution
    peaks_dis_B <- apply(multivariate_forecast, MARGIN = 1, FUN = max)

    # Extracting the peak from the test day
    peak <- max(df_test[, "y"])

    hist(peaks_dis_B, main = "Histogram of Peaks Distribution B", xlab = "Peaks", breaks = "Sturges")

    return(crps_sample(peak, peaks_dis_B))
}


getCRPS(30)

# specify the length for rolling iterations
len_test <- 2
for (d in seq(1, len_test)) {
    getCRPS(d)
}
