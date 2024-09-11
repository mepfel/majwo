library(tidyverse)
library(scoringRules)


# Load the model data
# Change the name to change the model
model <- read.csv("./data/forecasts/peaks_16-18_model-rf.csv")
model$ds <- as.POSIXct(model$ds, tz = "UTC")

# specify the length for the error learning phase in days
length <- 182 # According to recent advantages paper...


# Daten mit übergeben!!!
getDIS_errors <- function(d, data) {
    # Parameter: d day to predict
    # Parameter: data [y ... peak load, yhat ... point prediction, residuals ... residial peak load]
    print(d)
    # --------- Error Learning Phase ---------

    # Getting the test data: starting from day s get the next length days
    df_train <- data[d:(length + d - 1), ]

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
    df_test <- data[(d + length), ]


    for (i in quantiles) {
        # Destandardize the error for the qunatile i
        quantile_resid <- (inverse_ecdf(i) * sigma_resid) + mu_resid
        peaks_dis_A[peaks_dis_A$quantiles == i, "values"] <- df_test$yhat[1] + quantile_resid
    }
    print("Prediction DONE...")

    # Extracting the peak from the test day
    peak <- df_test[, "y"]
    date <- df_test[, "ds"]

    # Return peak and their distribution
    # CRPS Version: crps_sample(peak, peaks_dis_A[, "values"])
    return(data.frame(date = date, peak = peak, peak_dis = t(peaks_dis_A$values)))
}

# Calculate the maximal possible length
length(model[, "residuals"]) - length


# specify the length for testing period in days
len_test <- 818

peak_dis <- data.frame()
for (d in seq(1, len_test)) {
    dis <- getDIS_errors(d, model)
    peak_dis <- rbind(peak_dis, dis)
}

write.csv(peak_dis, file = "./evaluation/pb_error-rf-16-18.csv", row.names = FALSE)


# get the crps score
crps_scores <- crps_sample(peak_dis$peak, as.matrix(peak_dis[, 3:ncol(peak_dis)]))
print("Mean CRPS")
print(mean(crps_scores))

plot(crps_scores)
# Histogram
# hist(peaks_dis_A[, "values"], main = "Histogram of Peaks Distribution A", xlab = "Peaks", breaks = "Sturges")
