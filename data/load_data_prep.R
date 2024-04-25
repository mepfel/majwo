# Load required library
library(tidyverse)

# Data Preparation --------------------------------------------------------
prepare_data_1 <- function(path_to_data) {
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

prepare_data_2 <- function(path_to_data) {
  # Read CSV file and select specific columns,
  # rename columns, and convert date and time columns
  data <- read.csv(path_to_data, sep = ";") |>
    # Select columns load and date
    select(c("Datum.von", "Gesamt..Netzlast...MWh..Berechnete.Auflösungen")) |>
    rename(date = "Datum.von", load = "Gesamt..Netzlast...MWh..Berechnete.Auflösungen") |>
    # Convert the date column into the right format
    mutate(date = dmy_hm(date)) |>
    # Generate weekday_int, hour_int and month_int
    mutate(hour_int = hour(date), weekday_int = wday(date, week_start = 1), month_int = month(date)) |>
    # Add working days/weekends
    mutate(working_day = !(weekday_int %in% c(6, 7)))
  # Remove thousand separator and replace decimal separator as .
  data$load <- as.numeric(gsub(",", ".", gsub("\\.", "", data$load)))

  return(data)
}

prepare_data_3 <- function(path_to_data) {
  # Read CSV file and select specific columns,
  # rename columns, and convert date and time columns
  data <- read.csv(path_to_data, sep = ";") |>
    # Select columns load and date
    select(c("Datum.von", "Gesamt..Netzlast...MWh..Originalauflösungen")) |>
    rename(date = "Datum.von", load = "Gesamt..Netzlast...MWh..Originalauflösungen") |>
    # Convert the date column into the right format
    mutate(date = dmy_hm(date)) |>
    # Generate weekday_int, hour_int and month_int
    mutate(hour_int = hour(date), weekday_int = wday(date, week_start = 1), month_int = month(date)) |>
    # Generate the hour_quarter time stamp
    mutate(hour_quarter_int = hour_int + minute(date) / 60) |>
    # Add working days/weekends
    mutate(working_day = !(weekday_int %in% c(6, 7)))
  # Remove thousand separator and replace decimal separator as .
  data$load <- as.numeric(gsub(",", ".", gsub("\\.", "", data$load)))

  return(data)
}

energy_load <- read.csv("./data/raw/2015-2024_Realisierter-Stromverbrauch_viertelstunde.csv", sep = ";")

# --- Prepare data from 2022 - 2024 for the first CSV file ---
energy_load <- prepare_data_1("./data/raw/2022-2024_Realisierter-Stromverbrauch_stunde.csv")
# Save dataframes as CSV files in the Data folder
write.csv(energy_load, file = "./data/load_22-24.csv", row.names = FALSE)

# --- Prepare data from 2015 - 2024 ---
energy_load <- prepare_data_2("./data/raw/2015-2024_Realisierter-Stromverbrauch_stunde.csv")
# Save dataframes as CSV files in the Data folder
write.csv(energy_load, file = "./data/load_15-24.csv", row.names = FALSE)
