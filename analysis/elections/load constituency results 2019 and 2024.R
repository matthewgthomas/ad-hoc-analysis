library(tidyverse)
library(compositr)
library(readODS)
library(sf)

# ---- List of constituencies ----
# Source: https://geoportal.statistics.gov.uk/datasets/ons::westminster-parliamentary-constituencies-july-2024-names-and-codes-in-the-uk-v2/about
cons <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/PCON_2024_UK_NC_v2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

cons <-
  cons |>
  st_drop_geometry() |>
  select(cons_code = PCON24CD, cons_name = PCON24NM)

# ---- Helper function to rename parties in Democracy Club data ----
rename_party <- function(party_name) {
  case_when(
    party_name %in% c("Labour Party", "Labour and Co-operative Party") ~ "Labour",
    party_name == "Conservative and Unionist Party" ~ "Conservative",
    party_name == "Liberal Democrats" ~ "LibDem",
    party_name == "Green Party" ~ "Green",
    party_name == "Plaid Cymru - The Party of Wales" ~ "Plaid Cymru",
    party_name == "Reform UK" ~ "Reform",
    party_name == "Scottish National Party (SNP)" ~ "SNP",
    party_name == "Speaker seeking re-election" ~ "Speaker",
    str_detect(party_name, "Independent") ~ "Independent",
    .default = "Other"
  )
}

# ---- 2024 results ----
# Download constituency results from Democracy Club
# Source: https://candidates.democracyclub.org.uk/data/?election_date=2024-07-04&ballot_paper_id=&election_id=parl.*&party_id=&cancelled=&locked=&has_gss=&has_post_id=&has_votes_cast=&has_elected=&has_tied_vote_winner=&has_rank=&has_turnout_reported=&has_spoilt_ballots=&has_total_electorate=&has_turnout_percentage=&extra_fields=gss&extra_fields=votes_cast&extra_fields=elected&extra_fields=tied_vote_winner&extra_fields=rank&extra_fields=turnout_reported&extra_fields=spoilt_ballots&extra_fields=total_electorate&extra_fields=turnout_percentage
results_2024_raw <- read_csv("https://candidates.democracyclub.org.uk/data/export_csv/?election_date=2024-07-04&ballot_paper_id=&election_id=parl.%2A&party_id=&cancelled=&locked=&has_gss=&has_post_id=&has_votes_cast=&has_elected=&has_tied_vote_winner=&has_rank=&has_turnout_reported=&has_spoilt_ballots=&has_total_electorate=&has_turnout_percentage=&extra_fields=gss&extra_fields=votes_cast&extra_fields=elected&extra_fields=tied_vote_winner&extra_fields=rank&extra_fields=turnout_reported&extra_fields=spoilt_ballots&extra_fields=total_electorate&extra_fields=turnout_percentage&format=csv")

# Keep selected variables
results_2024 <-
  results_2024_raw |>
  select(
    gss,
    cons_name = post_label,
    party_name,
    votes = votes_cast,
    spoilt_ballots,
    total_electorate,
    elected
  )

# Ensure each constituency has its ONS code
results_2024 <-
  results_2024 |>
  left_join(cons) |>
  mutate(cons_code = if_else(is.na(cons_code), gss, cons_code)) |>
  select(-gss) |>
  relocate(cons_code)

# Fetch winners
winners_2024 <-
  results_2024 |>
  filter(elected) |>
  select(cons_code, winner = party_name) |>
  mutate(winner = rename_party(winner))

# Calculate total votes / turnout per constituency
turnout_2024 <-
  results_2024 |>
  group_by(cons_code) |>
  summarise(
    turnout = sum(votes, na.rm = TRUE)
      # Only include spoilt ballets if not NA
      #... but ignore completely for the time being
      # if_else(is.na(first(spoilt_ballots)), 0, first(spoilt_ballots))
  ) |>
  ungroup()

results_2024 <-
  results_2024 |>
  select(-elected) |>
  mutate(party_name = rename_party(party_name)) |>
  group_by(cons_code, cons_name, party_name, spoilt_ballots, total_electorate) |>
  summarise(votes = sum(votes)) |>
  ungroup() |>
  pivot_wider(names_from = party_name, values_from = votes) |>
  left_join(turnout_2024) |>
  left_join(winners_2024)

# Calculate vote share
results_2024 <-
  results_2024 |>
  mutate(
    Lab_pc = Labour / turnout,
    Con_pc = Conservative / turnout,
    LD_pc = LibDem / turnout,
    Green_pc = Green / turnout,
    Ref_pc = Reform / turnout,
    Ind_pc = Independent / turnout,
    Other_pc = Other / turnout
  )

# Save 2024 results
write_csv(results_2024, "analysis/elections/data/constituency-results-2024.csv")

# ---- 2019 results in 2024's boundaries ----
# Load Rallings and Thrasher's calculation of notional results for the 2019 election
# See here for more info: https://en.wikipedia.org/wiki/2023_Periodic_Review_of_Westminster_constituencies#Notional_2019_general_election_results
# and here: https://interactive.news.sky.com/2024/doc/estimates-2019-general-election-result-new-constituencies-explainer.pdf
tf <- download_file("https://downloads.bbc.co.uk/news/nol/shared/spl/xls_spreadsheets/results_spreadsheet.ods", ".ods")

results_2019_raw <- read_ods(tf, sheet = "2__results")

results_2019 <-
  results_2019_raw |>
  mutate(Independent = rowSums(across(IND1v:IND6v), na.rm = TRUE)) |>

  select(
    cons_code = `ONS code`,
    cons_name = `Boundary Comm name`,

    Conservative = Conv,
    Green = Grnv,
    Independent,
    Labour = Labv,
    LibDem = LDv,
    Reform = Brxv,
    SNP = SNPv,

    turnout = total_votes,
    winner = Win19,

    Lab_pc = `LAB%`,
    Con_pc = `CON%`,
    LD_pc = `LD%`,
    Green_pc = `GRN%`,
    Ref_pc = `BRX%`
  ) |>
  mutate(across(ends_with("_pc"), \(x) x/100)) |>
  mutate(Ind_pc = Independent / turnout)

# Save 2019 results
write_csv(results_2019, "analysis/elections/data/constituency-results-2019.csv")
