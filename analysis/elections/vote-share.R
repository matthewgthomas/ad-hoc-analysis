library(tidyverse)
library(compositr)
library(readxl)
library(plotly)

# ---- Election results ----
# Source: https://commonslibrary.parliament.uk/research-briefings/cbp-7529/
# tf <- download_file("https://researchbriefings.files.parliament.uk/documents/CBP-7529/general-elections-and-governments.xlsx", ".xlsx")

election_results <- read_excel("analysis/elections/general-elections-and-governments.xlsx", sheet = "3. GE results UK & GB", skip = 1)

election_results <-
  # Manually add projected vote share for 2024
  # Source: https://www.theguardian.com/politics/ng-interactive/2024/jul/04/uk-general-election-results-2024-live-in-full
  election_results |>
  add_row(Election = "2024", Country = "UK", Party = "CON", `Vote share` = .237, Seats = 121, `Total seats` = 650) |>
  add_row(Election = "2024", Country = "UK", Party = "LAB", `Vote share` = .338, Seats = 412, `Total seats` = 650) |>
  add_row(Election = "2024", Country = "UK", Party = "LD", `Vote share` = .122, Seats = 71, `Total seats` = 650) |>
  add_row(Election = "2024", Country = "UK", Party = "PC/SNP", `Vote share` = (.025 + .007), Seats = 13, `Total seats` = 650) |>
  add_row(Election = "2024", Country = "UK", Party = "Other", `Vote share` = .127, Seats = 27, `Total seats` = 650) |>
  add_row(Election = "2024", Country = "UK", Party = "REF", `Vote share` = .143, Seats = 4, `Total seats` = 650) |>

  # Add a row for Reform in 2019 so we can see a trend line to 2024
  add_row(Election = "2019", Country = "UK", Party = "REF", `Vote share` = 0, Seats = 0, `Total seats` = 650)

election_results <-
  election_results |>
  filter(Country == "UK") |>
  mutate(`Seat share` = Seats / `Total seats`)

# ---- Election dates ----
election_dates <- read_excel("analysis/elections/general-elections-and-governments.xlsx", sheet = "1. Governments", skip = 1)

election_dates <-
  election_dates |>

  add_row(Date = ymd("2024-07-04"), `Party forming government` = "Labour") |>

  # Manually set the years for 1974's two elections
  mutate(Election = case_when(
    Date == ymd("1974-02-28") ~ "1974F",
    Date == ymd("1974-10-10") ~ "1974O",
    .default = as.character(year(Date))
  )) |>

  # Recode parties
  mutate(
    Government = case_match(
      `Party forming government`,
      "Labour" ~ "LAB",
      "Conservative" ~ "CON",
      "Coalition" ~ "Coalition"
    )
  ) |>

  select(Election, Date, Government)

election_results <-
  election_results |>
  left_join(election_dates)

# ---- Plot trends in vote share ----
election_results |>
  filter(!is.na(`Vote share`)) |>

  ggplot(aes(x = Date, y = `Vote share`, group = Party, colour = Party)) +
  geom_line() +
  geom_vline(xintercept = election_dates$Date, linetype = 2, colour = "grey") +
  scale_color_manual(values = c("#0087dc", "#d50000", "#F6B527", "gray40", "#3B822B", "#12B6CF")) +
  theme_classic()

# ---- Vote and seat shares upon winning ----
# Ignore the coalition in 1913 and the National Government in 1931 since we can't directly compare vote share
winning_shares <-
  election_results |>
  filter(!Election %in% c("1918", "1931")) |>

  # Calculate combined vote share for CON and LD for 2010 election
  mutate(Party = if_else(Election == "2010" & Party %in% c("CON", "LD"), "Coalition", Party)) |>

  filter(Party == Government) |>

  group_by(Date, Government) |>
  summarise(
    `Vote share` = sum(`Vote share`),
    `Seat share` = sum(`Seat share`)
  ) |>
  ungroup()

# - Vote share -
winning_shares |>
  arrange(`Vote share`)

winning_shares |>
  ggplot(aes(x = Date, y = `Vote share`, group = 1, colour = Government)) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("grey40", "#0087dc", "#d50000")) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    legend.position = "top"
  ) +
  labs(
    title = "Labour's vote share in the 2024 election is the lowest of any winning party in the last century",
    subtitle = "The only vote share lower was Labour's victory in 1923",
    x = NULL,
    y = "Vote share of party/ies forming government",
    caption = "@matthewgthomas analysis of data from House of Commons Library and The Guardian\nNote: Vote share in the 2024 election is of the seats that have been declared as of 9am on 5 July (with five seats to go)."
  )

ggsave("analysis/elections/vote share of winning parties.png", width = 200, height = 175, units = "mm")

# - Seat share -
winning_shares |>
  arrange(desc(`Seat share`))

winning_shares |>
  ggplot(aes(x = Date, y = `Seat share`, group = 1, colour = Government)) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("grey40", "#0087dc", "#d50000")) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    legend.position = "top"
  ) +
  labs(
    title = "Labour's share of seats in the 2024 election is as high as in their 1997 victory",
    subtitle = "... and the highest of any winning party since 1924 election",
    x = NULL,
    y = "Seat share of party/ies forming government",
    caption = "@matthewgthomas analysis of data from House of Commons Library and The Guardian\nNote: Vote share in the 2024 election is of the seats that have been declared as of 9am on 5 July (with five seats to go)."
  )

ggsave("analysis/elections/seat share of winning parties.png", width = 200, height = 175, units = "mm")
