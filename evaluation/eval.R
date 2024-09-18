library(tidyverse)
library(forecast)
library(scoringRules)
library(ggplot2)
library(knitr)
# R Script for evaluation

# Helper Functions
q_distance <- function(dis) {
    q <- as.numeric(quantile(dis, probs = c(0.25, 0.75)))
    return(q[2] - q[1])
}

# Read in the peak distributions
# List all CSV files in the folder
file_paths <- list.files(path = "./evaluation/log", pattern = "*.csv", full.names = TRUE)
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
quant_distance <- list() # 0.75 - 0.25 Quantile
crps <- list()
# ------------ CRPS ------------
# get the crps score and the quant_distance from the peak distributions
for (i in file_names) {
    peak_dis <- get(i)
    peaks <- peak_dis$peak
    dis <- as.matrix(peak_dis[, 3:ncol(peak_dis)])
    crps_scores <- crps_sample(peaks, dis)
    quant_distance[[i]] <- apply(dis, 1, q_distance)
    crps[[i]] <- crps_scores
    print(paste0("Mean CRPS ", i))
    print(mean(crps_scores))
    # plot(crps_scores, main = i)
}

mean_crps <- data.frame(matrix(ncol = length(file_names), nrow = 1))
colnames(mean_crps) <- file_names
means <- c()
for (element in crps) {
    means <- c(means, mean(element))
}
mean_crps[1, ] <- means

# write.csv(mean_crps, file = "./plots/results/crps_means.csv", row.names = FALSE)
# Plain latex output
kable(t(mean_crps), "latex")


# ------- Quantile Distances -------
# Create a boxplot from the variances list with rotated x-axis labels
# Adjust the bottom margin to make space for the rotated labels
par(mar = c(8, 4, 4, 2) + 0.1)
boxplot(quant_distance[-9], main = "Quantile distances (0.75-0.25) of peak distribution", xaxt = "n")
axis(1, at = 1:(length(file_names) - 1), labels = FALSE)
mtext(side = 1, at = 1:(length(file_names) - 1), text = file_names[-9], line = 1, las = 2)

# -------- DM Test ---------
# H1: method 2 is better than method 1
dm.test(crps[["db_ar1"]], crps[["db_hist_sim"]], alternative = "greater")


# ----------- PIT ----------

# Iterate through the rows of db_hist_sim
# Initialize an empty vector to store quantile_peak values
model <- "dsb_ss_rf"
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

# Create a QQ-Plot of the quantiles compared to the uniform distribution
ggplot(quantiles_df, aes(sample = quantiles)) +
    stat_qq(distribution = stats::qunif) +
    stat_qq_line(distribution = stats::qunif) +
    labs(
        title = paste0("QQ Plot of ", model, " Quantiles vs Uniform Distribution"),
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
    ) +
    theme_minimal()

# Create a histogram of quantiles using ggplot2
ggplot(quantiles_df, aes(x = quantiles)) +
    geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
    labs(title = paste0("PIT ", model), x = "Quantiles", y = "Frequency") +
    theme_minimal()



# ------------ Unconditional Coverage -----------
# For one model
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

# For multiple models
cr <- c(0.25, 0.5, 0.75)
coverage_rates <- data.frame(matrix(ncol = length(file_names), nrow = 1))
colnames(coverage_rates) <- file_names

for (j in 1:length(cr)) {
    rates <- c()
    c <- cr[j]
    print(c)
    for (i in file_names) {
        data <- get(i)
        n <- nrow(data)
        hits <- 0
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
        rates <- c(rates, (hits / n))
    }
    coverage_rates[j, ] <- rates
}

rownames(coverage_rates) <- cr
# Round every element in coverage_rates to 4 decimal places
coverage_rates <- round(coverage_rates, 4)
# Calculate ACE
coverage_rates[1, ] <- coverage_rates[1, ] - 0.25
coverage_rates[2, ] <- coverage_rates[2, ] - 0.5
coverage_rates[3, ] <- coverage_rates[3, ] - 0.75

kable(t(coverage_rates), "latex")

write.csv(coverage_rates, file = "./plots/results/uc.csv", row.names = TRUE)

# -------- Kupiec Test -------
# x ... Number of hits
# n ... Total numbers of observation
# c ... Coverage rate to test (0,1)
# alpha ... For the test
kupiec_test <- function(x, n, c, alpha) {
    test <- -2 * log(((1 - c)^(x - n) * c^x) / ((1 - x / n)^(x - n) * (x / n)^x))

    cr_value <- qchisq(1 - alpha, df = 1)

    return(paste0(test >= cr_value, " | Critical value for alpha=", alpha, " is:", cr_value, " and the test statistic is: ", test))
}
