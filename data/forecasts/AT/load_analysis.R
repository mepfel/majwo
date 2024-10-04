library(tidyverse)
library(ggplot2)
library(plotly)

data <- read.csv("./data/forecasts/AT/loads_model_arx.csv")
data$ds <- as.POSIXct(data$ds, tz = "UTC")

data <- data |>
    filter((year(ds) >= 2016) & (year(ds) < 2017))


plot_data <- data.frame(date = data$ds, Actual = data$y, Predicted = data$yhat)

# Melt the data frame for easier plotting with ggplot
plot_data_long <- reshape2::melt(plot_data, id.vars = "date")

# Plot
fig <- ggplot(plot_data_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    labs(title = "Actual vs Predicted Load", y = "Load", x = "Date") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"))
fig
ggplotly(fig)
