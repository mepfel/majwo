library(tidyverse)


# Read in the peak distributions
# List all CSV files in the folder
file_paths <- list.files(path = "./data/forecasts/final", pattern = "*.csv", full.names = TRUE)
# Remove the .csv extension from the basenames
file_names <- tools::file_path_sans_ext(basename(file_paths))

# Read each CSV file into a data frame and assign to separate variables
dates <- list()
for (i in seq_along(file_paths)) {
    assign(file_names[i], read.csv(file_paths[i]))
    dates[[file_names[i]]] <- get(file_names[i])[, 1]
}

# Find the common dates across all models
common_dates <- Reduce(intersect, dates)

# Filter each model to keep only the rows in common_dates
for (i in file_names) {
    model <- get(i)
    filtered_model <- model[model[, 1] %in% common_dates, ]
    assign(i, filtered_model)
}

# ------------ CRPS ------------
# get the crps score and the quant_distance from the peak distributions
for (i in file_names) {
    data <- get(i)
    rmse <- sqrt(mean((data$residuals)^2))
    print(paste0("RMSE ", i))
    print(rmse)
    # plot(crps_scores, main = i)
}
