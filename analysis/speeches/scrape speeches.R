library(tidyverse)
library(rvest)

# Scrape inaugural addresses from https://www.presidency.ucsb.edu

# Fetch list of links
links_url <- "https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/inaugural-addresses?items_per_page=70"

list_of_links <- read_html(links_url)

list_of_links <-
  list_of_links |>
  html_elements("a") |>
  html_attr("href")

inaugural_links <- grep("/documents/inaugural-address-", inaugural_links, value = TRUE)

# Fetch inaugural addresses
base_url <- "https://www.presidency.ucsb.edu"

for (speech_url in inaugural_links) {
  doc <-
    read_html(paste0(base_url, speech_url))

  # President's name
  president_name <-
    doc |>
    html_element("h3.diet-title") |>
    html_text()

  # Inauguration year
  inauguration_year <-
    doc |>
    html_element("span.date-display-single") |>
    html_text()

  inauguration_year <- str_extract(inauguration_year, "(18|19|20)[0-9]{2}")

  # Speech
  speech <-
    doc |>
    html_element(".field-docs-content") |>
    html_text()

  # Save
  write_file(speech, str_glue("analysis/speeches/data/{inauguration_year} - {president_name}.txt"))

  print(paste0("Finished ", inauguration_year, ": ", president_name))
  Sys.sleep(1)
}
