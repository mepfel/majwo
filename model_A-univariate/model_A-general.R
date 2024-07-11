library(tidyverse)
library(scoringRules)
library(ggplot2)

# Load the model data

# Here: NeuralProphet
model <- read.csv("./data/forecasts/peaks_22-24_model-neuralprophet.csv")
model$ds <- as.POSIXct(model$ds, tz = "UTC")

# Here: Arma
# model <- read.csv("./data/forecasts/peaks_22-24_model-arma.csv")
# model$ds <- as.POSIXct(model$ds, tz = "UTC")

# specify the length for the error learning phase in days
length <- 365
# Filter only for the years 2023 (Training) and 2024 (Testing)
model <- model |>
    filter((year(ds) == 2023) | (year(ds) == 2024))

# Daten mit übergeben!!!
getCRPS_A <- function(d) {
    # Parameter: d day to predict
    print(d)
    # --------- Error Learning Phase ---------

    # Getting the test data: starting from day i get the next 365 days
    # Achtung: Hier dürfen nur Training oder Testing data verwendet werden
    df_train <- model[d:(364 + d), ]

    # Standardize the residuals
    mu_resid <- mean(df_train$residuals)
    sigma_resid <- sqrt(var(df_train$residuals))

    df_train$residuals_std <- (df_train$residuals - mu_resid) / sigma_resid

    # hist(df_train$residuals_std, main = "Histogram of Standardized Residuals", xlab = "Standardized Residuals")

    # Generate the Empirical Distribution Function for the resids
    ecdf <- ecdf(df_train$residuals_std)

    # Define the inverse ECDF
    # Input p is the probability and the return is the quantile
    inverse_ecdf <- function(p) {
        quantile(ecdf, p, names = FALSE)
    }


    print("Error Learning DONE...")

    # ------- Prediction phase -------

    m <- 90
    # Define the quantiles
    quantiles <- seq(1 / (m + 1), m / (m + 1), 1 / (m + 1))
    peaks_dis_A <- data.frame(quantiles = quantiles, values = rep(0, length(quantiles)))

    # Get the next 24 hours after the testing period
    # Hier nur Testing data vom Model!!!
    df_test <- model[(d + length), ]


    for (i in quantiles) {
        # Destandardize the error for the qunatile i
        quantile_resid <- (inverse_ecdf(i) * sigma_resid) + mu_resid
        peaks_dis_A[peaks_dis_A$quantiles == i, "values"] <- df_test$yhat[1] + quantile_resid
    }
    print("Prediction DONE...")

    # Histogram
    hist(peaks_dis_A[, "values"], main = "Histogram of Peaks Distribution A", xlab = "Peaks", breaks = "Sturges")

    # Extracting the peak from the test day
    peak <- df_test[, "y"]

    # CRPS-Score
    crps_sample(peak, peaks_dis_A[, "values"])
}

# specify the length for rolling iterations in days
len_test <- 28
crps_scores <- list()
for (d in seq(1, len_test)) {
    crps_score <- getCRPS_A(d)
    # Append the CRPS score to the list
    crps_scores[[d]] <- crps_score
}

print("Mean CRPS for 28 days in 2024")
print(mean(unlist(crps_scores)))
