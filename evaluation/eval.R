library(tidyverse)
library(scoringRules)
library(ggplot2)
library(plotly)
# R Script for evaluation

# Read in the peak distributions
# List all CSV files in the folder
file_paths <- list.files(path = "./evaluation/16-18", pattern = "*.csv", full.names = TRUE)
# Remove the .csv extension from the basenames
file_names <- tools::file_path_sans_ext(basename(file_paths))

# Read each CSV file into a data frame and assign to separate variables
for (i in seq_along(file_paths)) {
    assign(file_names[i], read.csv(file_paths[i]))
}
