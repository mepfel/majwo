# Load libraries ---------------------------------
library(tidyverse)
library(ggthemes)
library(palmerpenguins)


# Basics of Plotting -------------------------------
pengus <- penguins

ggplot(
  data = pengus,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species))

ggplot(pengus, aes(flipper_length_mm, body_mass_g)) +
  geom_point()

ggplot(pengus, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_point(
    data = pengus |> filter(species == "Gentoo"),
    shape = "circle open", size = 3, color = "red"
  )

# Basics of Data Transformation --------------------
library(nycflights13)

flights <- nycflights13::flights

flights |>
  filter(dest == "IAH" | dest == "HOU") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

special_flights <- flights |>
  filter(dest == "IAH" | dest == "HOU") |>
  filter(year == 2013 & month == 1 & day == 4)
view(special_flights)

special_flights |>
  summarise(mean(arr_delay))

special_flights |>
  arrange(desc(dep_delay))

# Compute the average departure delay by month
flights |>
  group_by(month) |>
  summarise(mean(dep_delay, na.rm = TRUE), n = n())

flights |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1) |>
  relocate(dest)

# Plotting : Layers -------------------
mpg
cars <- mpg

# Color
ggplot(mpg, aes(displ, hwy)) +
  geom_point(color = "red", shape = 17)

ggplot(mpg, aes(hwy)) +
  geom_histogram(binwidth = 2)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~cyl)

view(diamonds)

ggplot(diamonds, aes(cut)) +
  geom_bar()

ggplot(diamonds) +
  stat_summary(
    aes(cut, depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )
