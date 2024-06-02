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

# ---- Volatility in CPI components ----
cpih_components_all <-
  cpi_components |>
  select(
    Year,
    starts_with("CPIH INDEX ")
  )

names(cpih_components_all) |> as_tibble() |> View()

# Main components of CPIH
cpih_components <-
  cpih_components_all |>
  select(Year, matches("CPIH INDEX [0-9][0-9]\\.[0-9] ")) |>
  drop_na() |>

  # Keep monthly data
  filter(str_detect(Year, "[0-9]{4} [A-Z]{3}")) |>
  mutate(Year = ym(Year)) |>

  mutate(across(where(is.character), as.numeric))

# Calculate coefficient of variation across components for each month
cpih_volatility <-
  cpih_components |>
  rowwise(Year) |>
  summarise(
    mean = mean(c_across(where(is.double))),
    sd = sd(c_across(where(is.double))),
    cv = sd / mean
  ) |>
  ungroup()

cpih_volatility |>
  ggplot(aes(x = Year, y = cv)) +
  geom_line()

ggsave("analysis/inflation/CPIH volatility over time.png", width = 100, height = 100, units = "mm")

# What are the most volatile components?
cv <- list(
  mean = ~mean(.x),
  sd = ~sd(.x),
  cv = ~sd(.x) / mean
)

cpih_volatility_components <-
  cpih_components |>
  select(-Year) |>
  summarise(
    across(where(is.double), ~sd(.x) / mean(.x))
  ) |>
  pivot_longer(cols = everything(), names_to = "CPIH component", values_to = "CV")

cpih_volatility_components |>
  ggplot(aes(x = reorder(`CPIH component`, CV, sum), y = CV)) +
  geom_col() +
  coord_flip()

ggsave("analysis/inflation/CPIH volatility by component.png", width = 300, height = 175, units = "mm")

# Sub-components of CPIH
# e.g. CPIH INDEX 11.1.1.1 Restaurants, cafes and dancing establishments 2015=100
#TODO: Fix the regex. This doesn't work at the moment
cpih_subcomponents <-
  cpih_components_all |>
  select(Year, matches("CPI INDEX [0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9] "))
