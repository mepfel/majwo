library(tidyverse)
library(scoringRules)

# --- Load the energy data ---
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# --- Getting the peaks ---
peaks <- energy_load |>
    group_by(as.Date(date)) |>
    slice(which.max(load))

# ---- Rolling Window Methods ----
# Setting the window and the days to go away from the latest observation (starting from the newest one in
# the dataset)

window <- 100
days <- 1

# ----- Rolling Window Method #A for Peaks -----
# Getting the last 100 - 1 observation from the peaks
get_peaks_rolling <- function(days) {
    peaks_rolling <- peaks[(nrow(peaks) - window - days + 1):(nrow(peaks) - days), c(1, 2, 3)] |>
        arrange(desc(date))

    return(as.matrix(cbind(peaks_rolling$hour_int, peaks_rolling$load)))
}


# ----- Rolling Window Method #B based on Load Forecast ----

get_peaks_rolling_loads <- function(days) {
    # Step 1: Getting the empirical distribution
    # -

    # Step 2: Generating time traces per hour
    loads_rolling <- energy_load |>
        slice(1:(nrow(energy_load) - days * 24)) |>
        group_by(hour_int) |>
        arrange(desc(date)) |>
        slice_head(n = window)

    # Step 3: Extracting the peaks to get the distribution of peaks
    loads_rolling_peaks <- loads_rolling[, c(1, 2, 3)] |>
        group_by(as.Date(date)) |>
        slice(which.max(load)) |>
        arrange(desc(date))

    return(as.matrix(cbind(loads_rolling_peaks$hour_int, loads_rolling_peaks$load)))
}

# ---- Calculate the CRPS-Scores

# Initialize an empty data frame to store the results
results <- data.frame(day = integer(), crps_a = numeric(), crps_b = numeric(), es_a = numeric(), es_b = numeric())

# Loop over the days
for (day in 1:10) {
    # Update the observations
    obs_uni <- as.numeric(unlist(peaks[nrow(peaks) - day + 1, 3]))
    obs_bi <- as.numeric(unlist(peaks[nrow(peaks) - day + 1, 2:3]))

    # Update the forecasts
    uni_forecast_a <- get_peaks_rolling(day)[, 2]
    uni_forecast_b <- get_peaks_rolling_loads(day)[, 2]
    bi_forecast_a <- get_peaks_rolling(day)
    bi_forecast_b <- get_peaks_rolling_loads(day)

    # Calculate the CRPS and ES samples
    crps_a <- crps_sample(obs_uni, uni_forecast_a)
    crps_b <- crps_sample(obs_uni, uni_forecast_b)
    es_a <- es_sample(obs_bi, t(bi_forecast_a))
    es_b <- es_sample(obs_bi, t(bi_forecast_b))

    # Add the results to the data frame
    results <- rbind(results, data.frame(day = day, crps_a = crps_a, crps_b = crps_b, es_a = es_a, es_b = es_b))
}

# Print the results
print(results)
