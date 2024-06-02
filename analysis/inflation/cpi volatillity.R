# Explore how internal purchasing power of the pound has changed over time
# Analyse variation/volatility in components of the consumer price index
library(tidyverse)

# ---- Load consumer price index timeseries ----
# Source: https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindices
#Sys.setenv("VROOM_CONNECTION_SIZE" = 300000)
cpi_raw <- read_csv("https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv")
#Sys.unsetenv("VROOM_CONNECTION_SIZE")

cpi_raw <-
  cpi_raw |>
  slice(-(1:6)) |>
  rename(Year = Title)

# Check column names
names(cpi_raw) |> as_tibble() |> View()

# ---- Internal purchasing power of the pound ----
ppp <-
  cpi_raw |>
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
  cpi_raw |>
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

# ---- CPI components ----
# Fetch CPI index components and subcomponents names
# component_names <- sort(grep(pattern = "CPI INDEX \\d{2} ", names(cpi_raw), value = TRUE))
component_names <- sort(grep(pattern = "CPI INDEX \\d{2}\\.\\d{1,2} ", names(cpi_raw), value = TRUE))
subcomponent_names <- sort(grep(pattern = "CPI INDEX \\d{2}\\.\\d{1,2}\\.\\d{1,2} ", names(cpi_raw), value = TRUE))

cpi_components <-
  cpi_raw |>
  select(Year, all_of(component_names)) |>

  # Keep quarterly data from Q1 1988 onwards
  filter(str_detect(Year, "[0-9]{4} Q\\d{1}")) |>
  mutate(Year = yq(Year)) |>
  filter(Year >= yq("1988 Q1")) |>

  mutate(across(where(is.character), as.numeric))

# Calculate coefficient of variation across components for each month
cpi_volatility <-
  cpi_components |>
  rowwise(Year) |>
  summarise(
    mean = mean(c_across(where(is.double)), na.rm = TRUE),
    sd = sd(c_across(where(is.double)), na.rm = TRUE),
    cv = sd / mean
  ) |>
  ungroup()

cpi_volatility |>
  ggplot(aes(x = Year, y = cv)) +
  geom_line()

ggsave("analysis/inflation/CPI volatility over time.png", width = 100, height = 100, units = "mm")

# Explore CPI component volatility over different time periods
cpi_volatility_components <-
  cpi_components |>
  mutate(epoch = case_when(
    Year < yq("1990 Q3") ~ "Late 80s / early 90s",
    Year >= yq("1990 Q3") & Year <= yq("1991 Q3") ~ "Early 90s recession",
    Year > yq("1991 Q3") & Year < yq("2008 Q2") ~ "After 90s recession",
    Year >= yq("2008 Q2") & Year <= yq("2009 Q2") ~ "Great Recession",
    Year > yq("2009 Q2") & Year < yq("2020 Q1") ~ "After financial crisis",
    Year >= yq("2020 Q1") & Year <= yq("2020 Q2") ~ "Covid-19 recession",
    Year > yq("2020 Q2") & Year < yq("2023 Q3") ~ "After Covid recession",
    Year >= yq("2023 Q3") & Year <= yq("2023 Q4") ~ "2023 recession",
    Year > yq("2023 Q4") ~ "Post-2023 recession"
  )) |>
  mutate(epoch = factor(epoch, levels = c("Late 80s / early 90s", "Early 90s recession", "After 90s recession", "Great Recession", "After financial crisis", "Covid-19 recession", "After Covid recession", "2023 recession", "Post-2023 recession"))) |>
  # relocate(epoch) |>
  select(-Year) |>
  group_by(epoch) |>
  summarise(
    across(where(is.double), ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE))
  ) |>
  pivot_longer(cols = -epoch, names_to = "CPI component", values_to = "CV")

cpi_volatility_components |>
  ggplot(aes(x = reorder(`CPI component`, CV, sum), y = CV)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~epoch) +
  theme(
    plot.title.position = "plot"
  ) +
  labs(
    title = "Coefficient of variation for CPI components, by recession",
    x = NULL
  )

ggsave("analysis/inflation/CPI volatility by component.png", width = 300, height = 200, units = "mm")
