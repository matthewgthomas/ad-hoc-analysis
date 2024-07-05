library(tidyverse)
library(compositr)
library(readxl)
library(sf)

# ---- Make a lookup table for constituencies to nations ----
# List of constituencies ----
# Source: https://geoportal.statistics.gov.uk/datasets/ons::westminster-parliamentary-constituencies-july-2024-names-and-codes-in-the-uk-v2/about
cons <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/PCON_2024_UK_NC_v2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

cons <-
  cons |>
  st_drop_geometry() |>
  select(constituency_code = PCON24CD, constituency_name = PCON24NM) |>
  mutate(nation = case_when(
    str_detect(constituency_code, "^E") ~ "England",
    str_detect(constituency_code, "^W") ~ "Wales",
    str_detect(constituency_code, "^S") ~ "Scotland",
    str_detect(constituency_code, "^N") ~ "Northern Ireland",
  ))

# Constituency to Local Authority lookup
# Source: https://geoportal.statistics.gov.uk/datasets/ons::ward-to-westminster-parliamentary-constituency-to-lad-to-utla-july-2024-lookup-in-uk/about
lookup_cons_ltla <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD24_PCON24_LAD24_UTLA24_UK_LU/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

lookup_cons_ltla <-
  lookup_cons_ltla |>
  st_drop_geometry() |>
  distinct(constituency_code = PCON24CD, ltla24_code = LAD24CD)

# Fetch list of candidates from Democracy Club
# Source: https://candidates.democracyclub.org.uk/data/?election_date=2024-07-04&ballot_paper_id=&election_id=&party_id=&cancelled=&locked=
candidates_raw <- read_csv("https://candidates.democracyclub.org.uk/data/export_csv/?election_date=2024-07-04&ballot_paper_id=&election_id=&party_id=&cancelled=&locked=&format=csv")

candidates <-
  candidates_raw |>
  filter(str_detect(election_id, "^parl")) |>

  mutate(post_label = if_else(post_label == "Montgomeryshire and Glyndŵr", "Montgomeryshire and Glyndwr", post_label)) |>

  left_join(cons, by = c("post_label" = "constituency_name")) |>
  select(person_name, party_name, post_label, nation) |>

  mutate(first_name = str_extract(person_name, '\\w*'))

# Common names
common_names <-
  candidates |>
  count(first_name, sort = TRUE)

common_names |>
  filter(str_detect(first_name, "Matt"))

candidates |>
  filter(str_detect(first_name, "Natasha")) |>
  count(party_name, sort = TRUE)

# Common names by party
common_names_party <-
  candidates |>
  count(party_name, first_name, sort = TRUE) |>
  filter(n > 1)

common_names_party |>
  arrange(party_name, desc(n))

# Common names by nation
candidates |>
  count(nation, first_name, sort = TRUE) |>
  group_by(nation) |>
  slice_head(n = 5)

# ---- What proportion of our names won? ----
common_names_simplified <-
  candidates |>
  mutate(first_name = case_match(
    first_name,
    c("Chris", "Christopher") ~ "Christopher",
    c("Matthew", "Matt", "Mattie", "Matty") ~ "Matthew",
    .default = first_name
  )) |>
  count(first_name, sort = TRUE)

# Load list of winners from https://commonslibrary.parliament.uk/research-briefings/cbp-10009/
winners <- read_excel("analysis/elections/winning_members_0840.xlsx")

winning_names <-
  winners |>

  mutate(firstname = case_match(
    firstname,
    c("Chris", "Christopher") ~ "Christopher",
    c("Matthew", "Matt", "Mattie", "Matty") ~ "Matthew",
    .default = firstname
  )) |>

  count(firstname, sort = TRUE, name = "n_winners") |>

  left_join(common_names_simplified, by = c("firstname" = "first_name")) |>

  mutate(prop = n_winners / n)

winning_names |>
  filter(str_detect(firstname, "Sarah|Matt|Natasha|Chris$|Christopher")) |>
  arrange(desc(prop))
