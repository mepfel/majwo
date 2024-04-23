# Load required library
library(tidyverse)

# Data Preparation --------------------------------------------------------
prepare_data <- function(path_to_data) {
  # Read CSV file and select specific columns,
  # rename columns, and convert date and time columns
  data <- read.csv(path_to_data, sep = ";") |>
    select(c("Datum", "Anfang", "Gesamt..Netzlast...MWh..Berechnete.Auflösungen")) |>
    rename(date = "Datum", hour_int = "Anfang", load = "Gesamt..Netzlast...MWh..Berechnete.Auflösungen") |>
    mutate(date = dmy(date), hour_int = hour(hm(hour_int))) |>
    # Add hour to date object
    mutate(date = make_datetime(year = year(date), month = month(date), day = day(date), hour = hour_int)) |>
    mutate(weekday_int = wday(date, week_start = 1), month_int = month(date)) |>
    # Add working days/weekends
    mutate(working_day = !(weekday_int %in% c(6, 7)))
  # Remove thousand separator and replace decimal separator as .
  data$load <- as.numeric(gsub(",", ".", gsub("\\.", "", data$load)))

  return(data)
}

# Prepare data for the first CSV file
energy_load <- prepare_data("./data/raw/Realisierter_Stromverbrauch_2022-2024_stuendlich.csv")

# Save dataframes as CSV files in the Data folder
write.csv(energy_load, file = "./data/load_22-24.csv", row.names = FALSE)
