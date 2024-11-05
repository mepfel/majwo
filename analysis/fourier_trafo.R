first_diff <- log(peaks$load)
first_diff <- diff(first_diff, 1)

# Create a data frame for plotting
diff_data <- data.frame(
    index = 1:length(first_diff),
    diff_load = first_diff
)

data_h <- data |> filter((hour_int == 20))

plot(stl(ts(data_h$load, frequency = 365), "periodic"))

data_fourier <- ts(data_h$load[1:365], frequency = 365)
fourier_terms <- fourier(data_fourier, K = 2)


# Calculate the sum of the Fourier terms
sum_fourier_terms <- rowSums(fourier_terms)

# Create a data frame for plotting
fourier_data <- data.frame(
    index = 1:length(sum_fourier_terms),
    sum_fourier = sum_fourier_terms
)

# Plot the sum of Fourier terms
p_fourier <- ggplot(fourier_data, aes(x = index, y = sum_fourier)) +
    geom_line() +
    labs(
        title = "Sum of Fourier Terms",
        x = "Index",
        y = "Sum of Fourier Terms"
    )

# Display the plot
print(p_fourier)


# Create ggplot object
p <- ggplot(data_h, aes(x = date, y = load)) +
    geom_line() +
    labs(
        title = "First Difference of Peak Load",
        x = "Index",
        y = "First Difference"
    )

# Convert ggplot to ggplotly
ggplotly(p)

first_diff_withour_outlier <- first_diff[abs(first_diff) <= 5000]

# Create a data frame for plotting
diff_data <- data.frame(
    index = 1:length(first_diff_withour_outlier),
    diff_load = first_diff_withour_outlier
)

library(tseries)
# Perform ADF test for stationarity
adf_test_result <- adf.test(first_diff)
print(adf_test_result)

mean(first_diff)


# Filter the peaks for weekday_int == 3


for (i in (1:7)) {
    i <- 7
    filtered_peaks <- peaks |> filter(weekday_int == i)
    # Plot a histogram
    ggplot(filtered_peaks, aes(x = load)) +
        geom_histogram() +
        labs(title = paste("Histogram of Load for Weekday ", i), x = "Load", y = "Frequency")
}
