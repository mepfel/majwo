
# Set working directory
setwd("~/RStudio/Master_Thesis")

# Load required library
library(tidyverse)

# Data Preparation --------------------------------------------------------
# ERROR: DOWNLOADED DATASETS DO ONLY CONTAIN YEARS 15 and 16
prepare_data <- function(csv_path) {
  # Read CSV file and select specific columns, rename columns, and convert date and time columns
  dat <- read.csv("Data/DE1516.csv", sep = ";") %>% 
    select(c("Datum", "Anfang", "Gesamt..Netzlast...MWh..Berechnete.Auflösungen")) %>% 
    rename(date_time = "Datum", hour_int = "Anfang", load = "Gesamt..Netzlast...MWh..Berechnete.Auflösungen") %>% 
    mutate(date_time = dmy(date_time), hour_int = hour(hm(hour_int))) %>% 
    mutate(date_time = make_datetime(year = year(date_time), month = month(date_time), day = day(date_time),hour = hour_int)) %>% 
    mutate(weekday_int = wday(date_time), month_int = month(date_time), time_trend = seq(1, nrow(.)))

# Remove thousand separator and replace decimal separator as .
dat$load <- as.numeric(gsub(",", ".", gsub("\\.", "", dat$load)))

return(dat)
}

# Prepare data for the first CSV file
DE <- prepare_data("Data/DE1516.csv")
AU <- prepare_data("Data/AU1617.csv")
LU <- prepare_data("Data/LU1718.csv")

# Save dataframes as CSV files in the Data folder
write.csv(DE, file = "Data/DE.csv", row.names = FALSE)
write.csv(AU, file = "Data/AU.csv", row.names = FALSE)
write.csv(LU, file = "Data/LU.csv", row.names = FALSE)



# DE1718 <- read.csv("Data/DE1718.csv", sep = ";")
# DE1920 <- read.csv("Data/DE1920.csv", sep = ";")

# AU <- read.csv("Data/AU1516.csv", sep = ";")
# AU1718 <- read.csv("Data/AU1718.csv", sep = ";")
# AU1920 <- read.csv("Data/AU1920.csv", sep = ";")

# LUX <- read.csv("Data/LU1516.csv", sep = ";")
# LU1718 <- read.csv("Data/LU1718.csv", sep = ";")
# LU1920 <- read.csv("Data/LU1920.csv", sep = ";")

# Combine dataframes for each country
# DE <- DE1516 %>%
#   bind_rows(DE1718) %>%
 # bind_rows(DE1920)

#AU <- AU1516 %>%
#  bind_rows(AU1718) %>%
#  bind_rows(AU1920)

#LU <- LU1516 %>%
#  bind_rows(LU1718) %>%
#  bind_rows(LU1920)

# Rename rows and columns
