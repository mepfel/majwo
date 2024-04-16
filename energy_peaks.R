library(tidyverse)
library(ggplot2)

energy <- read.csv("Data/rf_data_1823.csv")

# Approach 2
peaks <- energy %>%
  group_by(date) %>%
  slice(which.max(load))



ggplot(peaks, aes(x = load_peak)) + geom_histogram(bins = 100)

ggplot(peaks, aes(x = load_peak)) + geom_density()

# Scatterplot
ggplot(peaks, aes(x = hour_int, y = load)) + geom_point()

ggplot(peaks, aes(x = hour_int)) + geom_density_2d()

# Density of load peaks
library(MASS)
library(plotly)
den3d <- kde2d(peaks$hour_int, peaks$load)
plot_ly(x=den3d$x, y=den3d$y, z=den3d$z) %>% add_surface()


# Density of load
den3d <- kde2d(energy$hour_int, energy$load)
plot_ly(x=den3d$x, y=den3d$y, z=den3d$z) %>% add_surface()

# Density of load
den3d <- kde2d(energy$weekday_int, energy$load)
plot_ly(x=den3d$x, y=den3d$y, z=den3d$z) %>% add_surface()

# Heatmap
fig <- plot_ly(energy, x = ~weekday_int, y = ~hour_int, z = ~load)
fig <- fig %>% add_heatmap()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weekday'),
                                   yaxis = list(title = 'Hour'),
                                   zaxis = list(title = 'Load')))

fig

# Scatter Plot
fig <- plot_ly(energy, x = ~weekday_int, y = ~hour_int, z = ~load)
fig <- fig %>% add_markers(marker = list(size = 3))
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weekday'),
                                   yaxis = list(title = 'Hour'),
                                   zaxis = list(title = 'Load')))

fig


# Scatter plot of peaks
fig <- plot_ly(peaks, x = ~weekday_int, y = ~hour_int, z = ~load)
fig <- fig %>% add_markers(marker = list(size = 3))
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weekday'),
                                   yaxis = list(title = 'Hour'),
                                   zaxis = list(title = 'Load')))

fig

# Heatmap of peaks
fig <- plot_ly(peaks, x = ~weekday_int, y = ~hour_int, z = ~load)
fig <- fig %>% add_heatmap()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weekday'),
                                   yaxis = list(title = 'Hour'),
                                   zaxis = list(title = 'Load')))

fig





       