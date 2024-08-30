library(tidyverse)
library(scoringRules)
library(ggplot2)
library(quantreg)

# length of the calibration period
clength <- 364


getCRPS_qra <- function(d, data) {
    # Parameter: d day to predict
    # Parameter: data [y ... Peak load, x ... avg. point prediction ]
    print(d)

    # Define the quantiles
    m <- 90
    quantiles <- seq(1 / (m + 1), m / (m + 1), 1 / (m + 1))

    # --------- Calibration Phase ---------

    # Getting the test data: starting from day i get the next clength days
    df_train <- data[d:(clength + d), ]

    qreg <- rq(y ~ x, tau = quantiles, data = df_train)


    print("Calibration DONE...")

    # ------- Prediction phase -------

    # Get the next data for prediction
    df_test <- data[(d + clength + 1), ]

    peaks_dis <- data.frame(predict.rq(qreg, df_test))[, 1]

    print("Prediction DONE...")


    # Extracting the peak from the test day
    peak <- df_test[, "y"]

    # CRPS-Score
    crps_sample(peak, peaks_dis)
}

# specify the length for rolling iterations in days
len_test <- 100
crps_scores <- list()
for (d in seq(1, len_test)) {
    crps_score <- getCRPS_qra(d, model)
    # Append the CRPS score to the list
    crps_scores[[d]] <- crps_score
}

print("Mean CRPS for 100 days in 2024")
print(mean(unlist(crps_scores)))
