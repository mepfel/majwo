library(tidyverse)
library(scoringRules)
library(copula)
# This is a first general implementation of the Shaake Shuffle algoithm

# Load the model data

# model <- read.csv("./data/forecasts/final/loads_model_rf.csv")
# model <- read.csv("./data/forecasts/final/loads_model_arx.csv")
# model <- read.csv("./data/forecasts/final/loads_model_arimax.csv")

# Check if hour 0 is missing and then add it
# model$ds <- ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", model$ds), paste(model$ds, "00:00:00"), model$ds)
# write.csv(model, "./data/forecasts/final/loads_model_rf.csv", row.names = FALSE)

model_list <- c("arimax", "arx", "rf")

for (m in model_list) {
    model <- read.csv(paste0("./data/forecasts/final/loads_model_", m, ".csv"))

    model$ds <- as.POSIXct(model$ds, tz = "UTC")


    # specify the length for the error learning phase in days
    length <- 365

    # data als Parameter übergeben
    getDIS_ss <- function(d, data) {
        print(d)
        # --------- Error Learning Phase ---------
        # Getting the test data: starting from day i get the next 365 days
        # Achtung: Hier dürfen nur Training oder Testing data verwendet werden
        df_train <- data[((d - 1) * 24 + 1):(((length + d - 1) * 24)), ]

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
        # m also defines the quantile level i.. 1 - m with i/m+1 quantiles
        univariate_forecast_t <- matrix(nrow = 24, ncol = m)
        quantiles <- seq(1 / (m + 1), m / (m + 1), 1 / (m + 1))

        # Get the next 24 hours after the testing period
        df_test <- data[(((d + length - 1) * 24) + 1):((d + length) * 24), ]

        predict_point <- df_test[, "yhat"]

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
        peaks_dis <- apply(multivariate_forecast, MARGIN = 1, FUN = max)

        # Extracting the row with the maximum load value from df_test
        max_load_row <- df_test[which.max(df_test[, "y"]), ]

        # Extracting the peak load value
        peak <- as.numeric(max_load_row["y"])

        # Extracting the date corresponding to the peak load value
        date <- max_load_row["ds"]

        return(data.frame(date = date, peak = peak, peak_dis = t(peaks_dis)))
    }

    # max possible length
    length(model$y) / 24 - length
    # specify the length for rolling iterations in days
    len_test <- 1035

    peak_dis <- data.frame()
    for (d in seq(1, len_test)) {
        dis <- getDIS_ss(d, model)
        peak_dis <- rbind(peak_dis, dis)
    }


    write.csv(peak_dis, file = paste0("./evaluation/dsb_ss_", m, ".csv"), row.names = FALSE)
}

# get the crps score
crps_scores <- crps_sample(peak_dis$peak, as.matrix(peak_dis[, 3:ncol(peak_dis)]))
print("Mean CRPS")
print(mean(crps_scores))


plot(crps_scores)
