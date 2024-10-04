library(tidyverse)
library(forecast)
library(scoringRules)
library(ggplot2)
library(reshape2)
library(knitr)
# R Script for evaluation

# Helper Functions
q_distance <- function(dis) {
    q <- as.numeric(quantile(dis, probs = c(0.25, 0.75)))
    return(q[2] - q[1])
}

# Read in the peak distributions
# List all CSV files in the folder
file_paths <- list.files(path = "./evaluation/m_tuning/365", pattern = "*.csv", full.names = TRUE)
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

write.csv(mean_crps, file = "./plots/results/crps_means_DE_365.csv", row.names = FALSE)
# Plain latex output
kable(t(mean_crps), "latex")

# ------------- CRPS | Boxplot --------
# get the crps score and the quant_distance from the peak distributions
crps_series <- data.frame(date = filtered_model$date)

for (i in file_names) {
    peak_dis <- get(i)
    peaks <- peak_dis$peak
    dis <- as.matrix(peak_dis[, 3:ncol(peak_dis)])
    crps_scores <- crps_sample(peaks, dis)
    crps_series[[i]] <- crps_scores
}

# Reshape crps_series from wide to long format
crps_long <- crps_series %>%
    pivot_longer(cols = -date, names_to = "model", values_to = "crps_score")

# General Boxplot
ggplot(crps_long, aes(x = model, y = crps_score)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 16)) +
    labs(
        x = "Models",
        y = "CRPS Score"
    )

# ------- Quantile Distances -------
# Convert quant_distance to a data frame
quant_distance_df <- data.frame(
    file_names = rep(file_names[-9], each = length(quant_distance[[1]])),
    distance = unlist(quant_distance[-9])
)

# Create the boxplot using ggplot2
ggplot(quant_distance_df, aes(x = file_names, y = distance)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 16)) +
    labs(
        x = "Model",
        y = "Quantile Distance (0.75 - 0.25)"
    )

# -------- DM Test ---------
# H1: method 2 is better than method 1
dm.test(crps[["hist_sim"]], crps[["hist_sim"]], alternative = "greater")

# Assuming crps is already defined
n <- length(crps)
results <- matrix(NA, n, n, dimnames = list(names(crps), names(crps)))

for (i in 1:n) {
    for (j in 1:n) {
        if (i != j) {
            test_result <- dm.test(crps[[i]], crps[[j]], alternative = "greater")
            results[i, j] <- test_result$p.value
        }
    }
}

# Plot a heatmap
melted_results <- melt(results)
# Create the heatmap
ggplot(data = melted_results, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "#efefef", high = "#3098de") +
    geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
    labs(
        x = "Model 2",
        y = "Model 1",
        fill = "p-value"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 16))

# ----------- PIT ----------
# Define the models
# c("dsa_error_arimax", "dsb_ss_arimax")
# c("dsa_error_arx", "dsb_ss_arx")
# c("dsa_error_rf", "dsb_ss_rf")
# c("dsb_ar1") c("hist_sim") c("dsa_qra")
models <- c("hist_sim")
quantiles_list <- list()

# Iterate through the models
for (model in models) {
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

    # Store the quantiles in the list
    quantiles_list[[model]] <- quantiles
}

# Combine the quantiles into a single data frame
quantiles_df <- data.frame(
    quantiles = unlist(quantiles_list),
    model = rep(models, times = sapply(quantiles_list, length))
)

# Create a QQ-Plot of the quantiles compared to the uniform distribution
ggplot(quantiles_df, aes(sample = quantiles, color = model)) +
    stat_qq(distribution = stats::qunif) + # color = "#1FBFC3" color_red = "#F6766F"
    geom_abline(slope = 1, intercept = 0) + # Add identity line
    labs(
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
    ) +
    theme_minimal() +
    theme(
        text = element_text(size = 15),
        legend.position = c(0.97, 0.05), # Position legend in bottom right corner
        legend.justification = c(1, 0), # Justify legend to bottom right corner,
        legend.title = element_blank(),
        aspect.ratio = 1
    )

# Create a histogram of quantiles using ggplot2
ggplot(quantiles_df, aes(x = quantiles)) +
    geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
    labs(title = paste0("PIT ", model), x = "Quantiles", y = "Frequency") +
    theme_minimal()



# ------------ Unconditional Coverage -----------
# For one model
model <- "hist_sim"
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

# ----- For multiple models ------
cr <- c(0.05, 0.25, 0.5, 0.75, 0.95)
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
            dis <- as.numeric(data[i, 3:ncol(data)])
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
coverage_rates[1, ] <- coverage_rates[1, ] - 0.05
coverage_rates[2, ] <- coverage_rates[2, ] - 0.25
coverage_rates[3, ] <- coverage_rates[3, ] - 0.5
coverage_rates[4, ] <- coverage_rates[4, ] - 0.75
coverage_rates[5, ] <- coverage_rates[5, ] - 0.95

kable(t(coverage_rates), "latex")

write.csv(coverage_rates, file = "./plots/results/uc.csv", row.names = TRUE)

# Make a plot of the coverage rates
coverage_rates <- coverage_rates * 100

# Assuming coverage_rates is a matrix or data frame
# Convert to data frame if necessary
coverage_rates_df <- as.data.frame(coverage_rates)

# Add rownames as a column to represent nominal coverages
coverage_rates_df$NominalCoverage <- rownames(coverage_rates_df * 100)

# Reshape the data frame to long format
coverage_rates_long <- gather(coverage_rates_df, key = "Model", value = "CoverageRate", -NominalCoverage)

# Convert NominalCoverage to a factor for better readability
coverage_rates_long$NominalCoverage <- factor(coverage_rates_long$NominalCoverage, levels = unique(coverage_rates_long$NominalCoverage))

# Define custom colors for the models
custom_colors <- c(
    "#c80000", "#bf004c", "#972677", "#604285",
    "#00c800", "#00b35d", "#009a82", "#007f8c", "grey"
)

# Create the bar chart with facet_wrap
ggplot(coverage_rates_long, aes(x = Model, y = CoverageRate, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(y = "ACE in %", fill = NULL) +
    theme_minimal() +
    facet_wrap(~NominalCoverage, scales = "fixed", nrow = 1) +
    theme(
        aspect.ratio = 1,
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 15),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = "bottom"
    ) +
    scale_fill_manual(values = custom_colors)

# -------- Kupiec Test -------
# x ... Number of hits
# n ... Total numbers of observation
# c ... Coverage rate to test (0,1)
# alpha ... For the test
kupiec_test <- function(x, n, c, alpha = 0.05) {
    test <- -2 * log(((1 - c)^(n - x) * c^x) / ((1 - x / n)^(n - x) * (x / n)^x))

    # Critical value
    # cr_value <- qchisq(1 - alpha, df = 1)
    return(test)
}

# For multiple models - Kupiec Test
cr <- c(0.05, 0.25, 0.5, 0.75, 0.95)
coverages <- data.frame(matrix(ncol = length(file_names), nrow = 1))
colnames(coverages) <- file_names

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
            dis <- as.numeric(data[i, 3:ncol(data)])
            # Compute the ECDF of dis
            ecdf_dis <- ecdf(dis)
            # Evaluate if it is a hit
            if (ecdf_dis(peak) <= c) {
                hits <- hits + 1
            }
        }
        rates <- c(rates, hits)
    }
    coverages[j, ] <- rates
}
rownames(coverages) <- cr

coverage_test <- data.frame(matrix(ncol = length(file_names), nrow = length(cr)))
colnames(coverage_test) <- file_names
rownames(coverage_test) <- cr

for (c in cr) {
    for (j in 1:length(file_names)) {
        coverage_test[paste0(c), j] <- kupiec_test(coverages[paste0(c), j], n, c)
    }
}
coverage_test$cr <- as.factor(cr)
# Melt the data frame to long format
coverage_test_long <- pivot_longer(coverage_test, cols = -cr, names_to = "model", values_to = "value")

# Replace values above 30 or invalid numbers with 30
coverage_test_long <- coverage_test_long %>%
    mutate(value = ifelse(is.na(value) | value > 30, 30, value))


# Create the scatter plot
ggplot(coverage_test_long, aes(x = model, y = value, shape = cr)) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = qchisq(1 - 0.05, df = 1), linetype = "dashed", color = "red") +
    labs(x = "Model", y = "Test Statistic", shape = "CR") +
    scale_shape_manual(values = c(6, 0, 5, 1, 2)) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 15),
    )


# Comparing mean CRPS DE
mean_crps_DE <- read.csv("./plots/results/crps_means_DE.csv")
# mean_crps_AT <- read.csv("./plots/results/crps_means_AT.csv")
mean_crps_m45 <- read.csv("./plots/results/crps_means_DE_m45.csv")
mean_crps_m365 <- read.csv("./plots/results/crps_means_DE_m365.csv")

# Normalize by Hist Sim
# mean_crps_DE <- mean_crps_DE[, 1:8] / mean_crps_DE[1, 9]
# mean_crps_AT <- mean_crps_AT[, 1:8] / mean_crps_AT[1, 9]

mean_crps_DE <- mean_crps_DE[, 1:8]
mean_crps_m45 <- mean_crps_m45[, 1:8]
mean_crps_m365 <- mean_crps_m365[, 1:8]
# Add a column to indicate the country
mean_crps_DE$m <- "90"
mean_crps_m45$m <- "45"
mean_crps_m365$m <- "365"

# Combine the data frames
mean_crps_combined <- rbind(mean_crps_DE, mean_crps_m45, mean_crps_m365)

# Pivot the combined data frame to long format
mean_crps_long <- pivot_longer(mean_crps_combined, cols = -m, names_to = "model", values_to = "mean_crps")


# Create the line plot
ggplot(mean_crps_long, aes(x = factor(model, level = c("dsa_qra", "dsb_ss_arx", "dsb_ss_rf", "dsb_ss_arimax", "dsa_error_rf", "dsa_error_arx", "dsa_error_arimax", "dsb_ar1")), y = mean_crps, color = m, group = m)) +
    geom_line() +
    geom_point() +
    labs(x = "Model", y = "Mean CRPS", color = "m") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 15)
    )

(mean_crps[, 1:8] / mean_crps[1, 9])
