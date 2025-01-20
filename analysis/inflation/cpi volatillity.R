# Explore how internal purchasing power of the pound has changed over time
# Analyse variation/volatility in components of the consumer price index
library(tidyverse)
library(ggnewscale)
library(ggiraph)

# ---- Load consumer price index timeseries ----
# Source: https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindices
Sys.setenv("VROOM_CONNECTION_SIZE" = 300000)
cpi_raw <- read_csv("https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv")
Sys.unsetenv("VROOM_CONNECTION_SIZE")

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
# subcomponent_names <- sort(grep(pattern = "CPI INDEX \\d{2}\\.\\d{1,2}\\.\\d{1,2} ", names(cpi_raw), value = TRUE))

cpi_index_and_components <-
  cpi_raw |>
  select(Year, `CPI INDEX 00: ALL ITEMS 2015=100`, all_of(component_names)) |>

  # Keep quarterly data from Q1 1988 onwards
  filter(str_detect(Year, "[0-9]{4} Q\\d{1}")) |>
  mutate(Year = yq(Year)) |>
  filter(Year >= yq("1988 Q1")) |>

  mutate(across(where(is.character), as.numeric))

cpi_components <-
  cpi_index_and_components |>
  select(-`CPI INDEX 00: ALL ITEMS 2015=100`)

# Calculate coefficient of variation across components for each month
cpi_volatility <-
  cpi_components |>

  # Reindex to oldest index in the data
  mutate(across(starts_with("CPI"), ~ .x / first(.x))) |>

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
# Recession dates from https://en.wikipedia.org/wiki/List_of_recessions_in_the_United_Kingdom
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

# ---- Change in CPI component indices since every recession ----
cpi_components_recessions <-
  cpi_components |>

  # Don't need late 80s / early 90s
  filter(Year >= yq("1990 Q3")) |>

  mutate(epoch = case_when(
    Year >= yq("1990 Q3") & Year < yq("2008 Q2") ~ "Early 90s recession",
    Year >= yq("2008 Q2") & Year < yq("2020 Q1") ~ "Great Recession",
    Year >= yq("2020 Q1") & Year < yq("2023 Q3") ~ "Covid-19 recession",
    Year > yq("2020 Q2") & Year < yq("2023 Q3") ~ "After Covid recession",
    Year >= yq("2023 Q3") ~ "2023 recession"
  )) |>
  mutate(epoch = factor(epoch, levels = c("Early 90s recession", "Great Recession", "Covid-19 recession", "2023 recession"))) |>
  relocate(epoch)

# Calculate percentage change for each component since the start of every recession
cpi_components_recessions <-
  cpi_components_recessions |>
  group_by(epoch) |>
  mutate(across(starts_with("CPI"), ~ (.x - first(.x)) / first(.x))) |>
  ungroup()

# Plot % change in each component during/after each recession
cpi_components_recessions |>
  pivot_longer(cols = -(epoch:Year), names_to = "CPI component", values_to = "pct_change") |>

  mutate(`CPI component` = str_remove(`CPI component`, "CPI INDEX [0-9]{2}\\.[0-9]{1} : ")) |>
  mutate(`CPI component` = str_remove(`CPI component`, " 2015=100")) |>

  ggplot(aes(x = Year, y = pct_change, group = `CPI component`)) +
  geom_line(aes(colour = `CPI component`)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~epoch, scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(
    title = "Change in CPI components during and after recessions",
    x = NULL,
    y = "% change since start of recession"
  )

plotly::ggplotly()

# ---- Inflation since an arbitrary year ----
from_year <- 2000

# Calculate overall inflation rate since `from_year`
cpi_index_and_components |>
  select()

# Calculate percentage change for each component since `from_year`
cpi_components_change <-
  cpi_components |>
  mutate(Year_only = year(Year)) |>
  filter(Year_only >= from_year) |>
  select(-Year_only) |>

  mutate(across(starts_with("CPI"), ~ (.x - first(.x)) / first(.x)))

cpi_components_change <-
  cpi_components_change |>
  pivot_longer(cols = -Year, names_to = "CPI component", values_to = "pct_change") |>

  mutate(`CPI component` = str_remove(`CPI component`, "CPI INDEX [0-9]{2}\\.[0-9]{1} : ")) |>
  mutate(`CPI component` = str_remove(`CPI component`, " 2015=100")) |>
  mutate(`CPI component` = str_to_sentence(`CPI component`)) |>

  # Fetch the most recent % change value for each CPI component: this will be used to colour the lines in the plot along a red or blue gradient
  group_by(`CPI component`) |>
  mutate(latest_value = last(pct_change)) |>
  ungroup()

from_year_text <- format(min(cpi_components_change$Year), "%B %Y")
to_year_text <- format(max(cpi_components_change$Year), "%B %Y")

# Which CPI components inflated or deflated since `from_year`
# Inflated components will be coloured red; deflated will be blue
inflated_components <-
  cpi_components_change |>
  filter(Year == max(Year) & pct_change > 0) |>
  pull(`CPI component`)

deflated_components <-
  cpi_components_change |>
  filter(Year == max(Year) & pct_change <= 0) |>
  pull(`CPI component`)

plt <-
  cpi_components_change |>
  filter(`CPI component` %in% inflated_components) |>

  ggplot(aes(x = Year, y = pct_change, group = `CPI component`, data_id = `CPI component`)) +
  geom_hline(yintercept = 0) +

  geom_line_interactive(aes(
    colour = latest_value,
    tooltip = str_glue("By {format(Year, '%d %B %Y')}, the price of {tolower(`CPI component`)} increased by {scales::percent(pct_change, accuracy = 0.1)}")
  )) +
  scale_colour_gradient(low = "#ffe6e6", high = "#4d0000") +

  new_scale_colour() +

  geom_line_interactive(
    data = cpi_components_change |> filter(`CPI component` %in% deflated_components),
    aes(
      colour = latest_value,
      tooltip = str_glue("By {format(Year, '%d %B %Y')}, the price of {tolower(`CPI component`)} fell by {scales::percent(pct_change, accuracy = 0.1)}")
    )
  ) +
  scale_colour_gradient(low = "#cce0ff", high = "#001f4d") +

  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    legend.position = "none"
  ) +
  labs(
    title = str_glue("Price changes in the UK from {from_year_text} to {to_year_text}"),
    x = NULL,
    y = NULL
  )

plt

girafe(
  ggobj = plt,
  options = list(
    opts_hover_inv(css = "opacity:0.1;"),
    opts_hover(css = "stroke-width:2;"),

    # opts_selection(type = "multiple", css = "fill:#FF851B;stroke:black;"),
    opts_toolbar(saveaspng = FALSE, position = "topright", delay_mouseout = 10000),
    opts_tooltip(
      css = "background-color:black;color:white;padding:10px;border-radius:10px;box-shadow:10px 10px 10px rgba(0,0,0,0.3);font-family:Arial;font-size:12px;",
      opacity = 0.9,
      use_fill = TRUE
    ),
    opts_sizing(rescale = TRUE),
    opts_zoom(max = 2)
  )
)

# plotly::ggplotly(tooltip = "text")
