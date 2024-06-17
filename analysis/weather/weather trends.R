library(tidyverse)

# Years
first_year <- 2004
last_year <- 2024

# Fetch historical weather data from https://www.visualcrossing.com/weather/weather-data-services
weather_data <-
  tibble(year = first_year:last_year) |>
  str_glue_data("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/london/{year}-05-01/{year}-06-15?unitGroup=metric&include=days&key=TZL8G2FX5CG8NMHWGYNYE65LP&contentType=csv") |>
  map_df(read_csv, .id = "year") |>
  mutate(year = year(datetime))

weather_2024 <-
  weather_data |>
  filter(year == max(year))

weather_historical <-
  weather_data |>
  filter(year < 2024) |>
  mutate(monthday = ymd(str_glue("2024/{month(datetime)}/{day(datetime)}")))

# Plot temperatures
weather_historical |>
  ggplot(aes(x = monthday, y = feelslike, group = year)) +
  geom_line(colour = "grey85") +
  geom_line(data = weather_2024, aes(x = datetime), colour = "red") +
  theme_classic() +
  theme(plot.title.position = "plot") +
  labs(
    title = "Daily temperatures from 1st May to 15th June over the last 20 years",
    subtitle = "Red line is temperatures in 2024; grey lines show previous years",
    x = NULL,
    y = "'Feels like' temperature (Celsius)"
  )

ggsave("analysis/weather/temperature trends.png", height = 100, width = 150, units = "mm")

# Plot rainfall
weather_historical |>
  ggplot(aes(x = monthday, y = precip, group = year)) +
  geom_line(colour = "grey85") +
  geom_line(data = weather_2024, aes(x = datetime), colour = "red") +
  theme_classic() +
  theme(plot.title.position = "plot") +
  labs(
    title = "Daily rainfall from 1st May to 15th June over the last 20 years",
    subtitle = "Red line is rain in 2024; grey lines show previous years",
    x = NULL,
    y = "Precipitation (mm)"
  )

ggsave("analysis/weather/rainfall trends.png", height = 100, width = 150, units = "mm")
