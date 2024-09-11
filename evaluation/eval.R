library(tidyverse)
library(forecast)
library(scoringRules)
library(scoringutils)
library(ggplot2)
library(plotly)
# R Script for evaluation

# Read in the peak distributions
# List all CSV files in the folder
file_paths <- list.files(path = "./evaluation/16-18", pattern = "*.csv", full.names = TRUE)
# Remove the .csv extension from the basenames
file_names <- tools::file_path_sans_ext(basename(file_paths))

# Read each CSV file into a data frame and assign to separate variables
dates <- list()
for (i in seq_along(file_paths)) {
    assign(file_names[i], read.csv(file_paths[i]))
    dates[[file_names[i]]] <- get(file_names[i])[, 1]
}
common_dates <- Reduce(intersect, dates)

# Filter each model to keep only the rows in common_dates
for (i in file_names) {
    model <- get(i)
    filtered_model <- model[model[, 1] %in% common_dates, ]
    assign(i, filtered_model)
}


# Create an empty dataframe with the strings in file_names as column names
variances <- list()
crps <- list()
# ------------ CRPS ------------
# get the crps score and the variances from the peak distributions
for (i in file_names) {
    peak_dis <- get(i)
    peaks <- peak_dis$peak
    dis <- as.matrix(peak_dis[, 3:ncol(peak_dis)])
    crps_scores <- crps_sample(peaks, dis)
    variances[[i]] <- apply(dis, 1, var)
    crps[[i]] <- crps_scores
    print(paste0("Mean CRPS ", i))
    print(mean(crps_scores))
    plot(crps_scores, main = i)
}

# ------- VARIANCE -------
# Create a boxplot from the variances list with rotated x-axis labels
# Adjust the bottom margin to make space for the rotated labels
par(mar = c(8, 4, 4, 2) + 0.1)
boxplot(variances, main = "Variances of peak distribution", xaxt = "n")
axis(1, at = 1:length(file_names), labels = FALSE)
mtext(side = 1, at = 1:length(file_names), text = file_names, line = 1, las = 2)

# -------- DM Test ---------
# H1: method 2 is better than method 1
dm.test(crps[["db_ar1"]], crps[["db_hist_sim"]], alternative = "greater")


# ----------- PIT ----------

# Iterate through the rows of db_hist_sim
# Initialize an empty vector to store quantile_peak values
model <- "db_qra"
data <- get(model)
quantiles <- c()

for (i in 1:nrow(data)) {
    peak <- data[i, 2]
    dis <- as.numeric(data[i, 3:92])
    # Compute the ECDF of dis
    ecdf_dis <- ecdf(dis)
    # Calculate the quantile of peak
    quantiles <- c(quantiles, ecdf_dis(peak))
}

# Convert the quantiles vector to a data frame
quantiles_df <- data.frame(quantiles = quantiles)

# Create a histogram of quantiles using ggplot2
ggplot(quantiles_df, aes(x = quantiles)) +
    geom_histogram(binwidth = 0.07, fill = "blue", color = "black") +
    labs(title = paste0("PIT ", model), x = "Quantiles", y = "Frequency") +
    theme_minimal()



# ------------ Unconditional Coverage -----------
model <- "db_hist_sim"
data <- get(model)
n <- nrow(data)

hits <- 0
c <- 0.5 # coverage rate should be between 0 and 1

for (i in 1:nrow(data)) {
    peak <- data[i, 2]
    dis <- as.numeric(data[i, 3:92])
    # Compute the ECDF of dis
    ecdf_dis <- ecdf(dis)
    # Evaluate if it is a hit
    if (ecdf_dis(peak) <= c) {
        hits <- hits + 1
    }
}
coverage_rate <- hits / n
print(paste0("PICP for ", c))
print(coverage_rate)
