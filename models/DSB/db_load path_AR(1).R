library(tidyverse)
library(scoringRules)
library(forecast)
library(MASS)

# This is a first general implementation of the AR-1 approach
# -------- Functions ------
generate_A <- function(rho) {
    # Initialize an empty 24x24 matrix
    A <- matrix(0, nrow = 24, ncol = 24)

    # Loop over the matrix indices
    for (r in 1:24) {
        for (c in 1:24) {
            if (r >= c) {
                A[r, c] <- rho^(r - c)
            }
        }
    }
    return(A)
}


# --- Load the energy data ----
holidays <- read.csv("./data/holidays_DE_15-24.csv") |>
    mutate_at("Date", as.Date)
energy_load <- read.csv("./data/load_15-24.csv") |>
    mutate_at(c("hour_int", "weekday_int", "month_int"), as.factor) |>
    mutate(is_holiday = if_else(as.Date(date) %in% holidays$Date, 1, 0))

energy_load$date <- as.POSIXct(energy_load$date, tz = "UTC")

data <- energy_load |> filter(year(date) >= 2015)

# --------------------------------------------------------------------------
# Take one year for training
data <- energy_load |>
    filter((year(date) >= 2022) & (year(date) < 2023)) |>
    mutate(
        load_origin = load,
        load = log(load)
    )
# Take the next day for testing
data_test <- energy_load[8761:(8761 + 23), ] |>
    mutate(
        load_origin = load,
        load = log(load)
    )

# Generate the fourier terms
data_fourier_year <- ts(data$load, frequency = 24 * 365)
data$fourier_terms_year <- fourier(data_fourier_year, K = 2)
data_test$fourier_terms_year <- fourier(data_fourier_year, K = 2, h = 24)


statio <- lm(load ~ hour_int + weekday_int + weekday_int * hour_int + is_holiday + fourier_terms_year, data = data)
summary(statio)
# Assuming y is a dataframe with residuals
y <- data.frame(index = 1:length(residuals(statio)), residuals = residuals(statio))

# Some test
pacf(y$residuals)
library(tseries)
adf_test <- adf.test(y$residuals)
print(adf_test)
# Create a histogram for y$residuals
ggplot(y, aes(x = residuals)) +
    geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
    labs(
        title = "Histogram of Residuals",
        x = "Residuals",
        y = "Frequency"
    ) +
    theme_minimal()

# Create the AR-1 model
ar1 <- lm(y$residuals ~ lag(y$residuals))
summary(ar1)

rho <- coef(ar1)[2]
tau_sq <- var(ar1$residuals)

A <- generate_A(rho)

# Sample from the multivariate normal distribution
# Compute the covariance matrix sigma = tau^2 * A %*% t(A)
sigma <- tau_sq * A %*% t(A)

# Set the mean vector to be a zero vector of length 24
mean <- rep(0, 24)

# Draw samples (e.g., 1000 samples)
error_samples <- mvrnorm(n = 1, mu = mean, Sigma = sigma)

y_mean_24 <- as.numeric(tail(y, 1)[2]) * rho^(1:24)

# Create Y forecast for the next 24 hours
y_sample_24 <- y_mean_24 + error_samples

logload_mean_24 <- as.numeric(predict(statio, data_test))

logload_pred_24 <- logload_mean_24 + y_sample_24

# Hier eventuell noch anpassen wegen log-trafo -> Mean und Variance
loads_pred_24 <- exp(logload_pred_24)

# Plot the results
# Create a dataframe for plotting
df_loads <- data.frame(
    index = data_test$date,
    predicted_load = loads_pred_24,
    actual_load = data_test$load_origin
)

# Plot predicted and actual loads
ggplot(df_loads, aes(x = index)) +
    geom_line(aes(y = predicted_load, color = "Predicted Load")) +
    geom_line(aes(y = actual_load, color = "Actual Load")) +
    labs(
        title = "Predicted vs Actual Loads for the Next 24 Hours",
        x = "Index",
        y = "Load"
    ) +
    scale_color_manual(values = c("Predicted Load" = "blue", "Actual Load" = "red")) +
    theme_minimal()

# ------------------- CRPS implementation ----------------
# specify the length for the learning phase in days

length <- 364

# data als Parameter übergeben
getDIS_ar1 <- function(d, data) {
    print(d)
    # Getting the train data: starting from day i get the next 365 days
    df_train <- data[((d - 1) * 24 + 1):((length + d) * 24), ]
    # Get the next 24 hours after the training period
    df_test <- data[(((d + length) * 24) + 1):((d + length + 1) * 24), ]

    # Generate the fourier terms
    data_fourier_year <- ts(df_train$load, frequency = 24 * 365)
    df_train$fourier_terms_year <- fourier(data_fourier_year, K = 2)
    df_test$fourier_terms_year <- fourier(data_fourier_year, K = 2, h = 24)


    # --------- Training Phase ---------
    statio <- lm(load ~ hour_int + weekday_int + weekday_int * hour_int + is_holiday + fourier_terms_year, data = df_train)
    # Assuming y is a dataframe with residuals
    y <- data.frame(index = 1:length(residuals(statio)), residuals = residuals(statio))

    # Create the AR-1 model
    ar1 <- lm(y$residuals ~ lag(y$residuals))
    summary(ar1)

    rho <- coef(ar1)[2]
    tau_sq <- var(ar1$residuals)

    A <- generate_A(rho)

    # Compute the covariance matrix sigma = tau^2 * A %*% t(A)
    sigma <- tau_sq * A %*% t(A)

    # Set the mean vector to be a zero vector of length 24
    mean <- rep(0, 24)

    print("Training DONE...")
    # ------- Prediction -----
    # Define the length m of the learning phase
    m <- 90
    multivariate_forecast <- matrix(nrow = m, ncol = 24)
    # Create the mean prediction from the AR-1 model for the next 24 hours
    y_mean_24 <- as.numeric(tail(y, 1)[2]) * rho^(1:24)
    # Get the base component for the 24 hours from the seosonality model
    base_mean_24 <- as.numeric(predict(statio, df_test))

    for (i in 1:m) {
        # Sample from the multivariate normal distribution
        error_samples <- mvrnorm(n = 1, mu = mean, Sigma = sigma)

        # Add the error samples to get a load path
        y_sample_24 <- y_mean_24 + error_samples

        # Add the y sample
        pred_24 <- base_mean_24 + y_sample_24

        multivariate_forecast[i, ] <- pred_24
    }

    print("Prediction DONE...")

    # Extracting the peaks from the multivariate distribution
    peaks_dis <- apply(multivariate_forecast, MARGIN = 1, FUN = max)

    # Extracting the row with the maximum load value from df_test
    max_load_row <- df_test[which.max(df_test[, "load"]), ]

    # Extracting the peak load value
    peak <- as.numeric(max_load_row["load"])

    # Extracting the date corresponding to the peak load value
    date <- max_load_row["date"]

    return(data.frame(date = date, peak = peak, peak_dis = t(peaks_dis)))
}


# specify the length for rolling iterations in days
len_test <- 1400

peak_dis <- data.frame()
for (d in seq(1, len_test)) {
    dis <- getDIS_ar1(d, data)
    peak_dis <- rbind(peak_dis, dis)
}

write.csv(peak_dis, file = "./evaluation/dsb_ar1.csv", row.names = FALSE)

# get the crps score
crps_scores <- crps_sample(peak_dis$peak, as.matrix(peak_dis[, 3:ncol(peak_dis)]))
print("Mean CRPS")
print(mean(crps_scores))

plot(crps_scores)
