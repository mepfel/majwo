library(tidyverse)
library(scoringRules)
library(ggplot2)
library(quantreg)

# ---- Getting the data ----

model1 <- read.csv("./data/forecasts/peaks_16_model-ar7.csv")
model2 <- read.csv("./data/forecasts/peaks_16_model-arima(1,1,1).csv")
model3 <- read.csv("./data/forecasts/peaks_16_model-neuralprophet.csv")

data <- model1[1:(nrow(model1) - 6), 1:2]
data$x1 <- model1[1:(nrow(model1) - 6), ]$yhat
data$x2 <- model2[8:nrow(model2), ]$yhat
data$x3 <- model3[8:nrow(model3), ]$yhat
# length of the calibration period
clength <- 182


getDIS_qra <- function(d, data) {
    # Parameter: d day to predict
    # Parameter: data [y ... Peak load, x ... avg. point prediction ]
    print(d)

    # Define the quantiles
    m <- 90
    quantiles <- seq(1 / (m + 1), m / (m + 1), 1 / (m + 1))

    # --------- Calibration Phase ---------

    # Getting the test data: starting from day i get the next clength days
    df_train <- data[d:(clength - 1 + d), ]

    qreg <- rq(y ~ x1 + x2 + x3, tau = quantiles, data = df_train)


    print("Calibration DONE...")

    # ------- Prediction phase -------

    # Get the next data for prediction
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
len_test <- 177

peak_dis <- data.frame()
for (d in seq(1, len_test)) {
    dis <- getDIS_qra(d, data)
    peak_dis <- rbind(peak_dis, dis)
}

write.csv(peak_dis, file = "./evaluation/db_qra.csv", row.names = FALSE)

# get the crps score
crps_scores <- crps_sample(peak_dis$peak, as.matrix(peak_dis[, 3:ncol(peak_dis)]))
print("Mean CRPS")
print(mean(crps_scores))

plot(crps_scores)

i <- 1
# CRPS-Score
crps_sample(peak_dis[i, 2], as.numeric(peak_dis[i, 3:ncol(peak_dis)]))
# Histogramm
hist(as.numeric(peak_dis[i, 3:ncol(peak_dis)]))
