# Explore how internal purchasing power of the pound has changed over time
# Analyse variation/volatility in components of the consumer price index
library(tidyverse)

# ---- Load consumer price index timeseries ----
# Source:
#Sys.setenv("VROOM_CONNECTION_SIZE" = 300000)
cpi_components_raw <- read_csv("https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv")
#Sys.unsetenv("VROOM_CONNECTION_SIZE")

# Check column names
names(cpi_components_raw) |> as_tibble() |> View()

cpi_components <-
  cpi_components_raw |>
  slice(-(1:6)) |>
  rename(Year = Title)

# ---- Internal purchasing power of the pound ----
ppp <-
  cpi_components |>
  select(
    Year,
    `Internal purchasing power of the pound (based on RPI): 1982=100`,
    `Internal purchasing power of the pound (based on RPI): 2009=100`
  ) |>
  drop_na() |>
  mutate(across(everything(), as.numeric))

ppp |>
  ggplot(aes(x = Year, y = `Internal purchasing power of the pound (based on RPI): 1982=100`)) +
  geom_line()
