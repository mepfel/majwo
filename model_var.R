library(tidyverse)
library(scoringRules)
library(fable)


# --- Load the energy data ---
energy_load <- read.csv("./data/load_22-24.csv")
energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

# --- Getting the peaks ---
peaks <- energy_load |>
  group_by(as.Date(date)) |>
  slice(which.max(load))

# --- Predicting the next peak based on past ---
window <- 100
peaks[1:window, c(1, 2, 3)]

# A VAR Model here: Problem! Only a point forecast!!!
