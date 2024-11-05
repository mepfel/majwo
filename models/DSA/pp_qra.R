library(tidyverse)
library(quantreg)
library(scoringRules)
library(ggplot2)


# ---- Getting the data ----

model1 <- read.csv("./data/forecasts/final/peaks_model_arx.csv")
model2 <- read.csv("./data/forecasts/final/peaks_model_arimax.csv")
model3 <- read.csv("./data/forecasts/final/peaks_model_rf.csv")

# Merge the data frames based on the ds and y columns and select only the yhat columns
data <- model1 |>
    inner_join(model2, by = c("ds", "y"), suffix = c("_model1", "_model2")) |>
    inner_join(model3, by = c("ds", "y"), suffix = c("", "_model3")) |>
    select(ds, y, x1 = yhat_model1, x2 = yhat_model2, x3 = yhat)

# length of the calibration period
clength <- 365


getDIS_qra <- function(d, data) {
    # Parameter: d day to predict
    # Parameter: data [y ... Peak load, x ... avg. point prediction ]
    print(d)

    # Define the quantile set
    m <- 90 # Total number of quantiles = Models to fit
    quantiles <- seq(1 / (m + 1), m / (m + 1), 1 / (m + 1))

    # --------- Calibration Phase ---------

    # Getting the test data: starting from day i get the next clength days
    df_train <- data[d:(clength - 1 + d), ]

    # Run the quantile regression
    qreg <- rq(y ~ x1 + x2 + x3, tau = quantiles, data = df_train)


    print("Calibration DONE...")

    # ------- Prediction phase -------

    # Get the next data point for prediction
    df_test <- data[(d + clength), ]

    peaks_dis <- data.frame(predict.rq(qreg, df_test))[, 1]

    print("Prediction DONE...")


    # Extracting the peak from the test day
    peak <- df_test[, "y"]
    date <- df_test[, "ds"]

    # Return peak and their distribution
    # CRPS Version: crps_sample(peak, peaks_dis)
    return(data.frame(date = date, peak = peak, peak_dis = t(peaks_dis)))
}

# Calculate the maximal possible length
length(data[, "y"]) - clength

# specify the length for rolling iterations in days
len_test <- 1035

peak_dis <- data.frame()
for (d in seq(1, len_test)) {
    dis <- getDIS_qra(d, data)
    peak_dis <- rbind(peak_dis, dis)
}

# Store the results in a csv file
write.csv(peak_dis, file = "./evaluation/dsa_qra.csv", row.names = FALSE)

# Get the crps score
crps_scores <- crps_sample(peak_dis$peak, as.matrix(peak_dis[, 3:ncol(peak_dis)]))
print("Mean CRPS")
print(mean(crps_scores))

plot(crps_scores)

# ----- CRPS Score for a specific peak distribution i --------
i <- 1
# CRPS-Score
crps_sample(peak_dis[i, 2], as.numeric(peak_dis[i, 3:ncol(peak_dis)]))
# Histogramm
hist(as.numeric(peak_dis[i, 3:ncol(peak_dis)]))
