library(readr)
library(sylcount)

speech_files <- list.files("analysis/speeches/data/", full.names = TRUE)

# Initialize a list to hold each file's data
speech_list <- lapply(speech_files, function(file) {
  # Get the filename only (without path)
  file_name <- basename(file)

  # Remove ".txt" and split at " - "
  # Example: "2017 - Obama.txt" => "2017 - Obama" => c("2017", "Obama")
  base_name <- sub("\\.txt$", "", file_name)
  parts <- strsplit(base_name, " - ")[[1]]

  # Handle the case if filename format is not as expected
  if(length(parts) == 2) {
    year <- parts[1]
    person <- parts[2]
  } else {
    # If format differs, set placeholders or handle as needed
    year <- NA
    person <- NA
  }

  # Read file contents and collapse into one string
  file_text <- paste(readLines(file, warn = FALSE), collapse = "\n")

  # Return as a data frame
  data.frame(
    year = year,
    person = person,
    text = file_text,
    stringsAsFactors = FALSE
  )
})

# Combine all rows into a single data frame
speeches <- do.call(rbind, speech_list)
speeches$year <- as.numeric(speeches$year)

# Calculate readability
speeches <-
  bind_cols(
    speeches,
    readability(speeches$text)
  )

# Track readability over time
speeches |>
  select(year, re, gl, smog) |>
  pivot_longer(cols = -year, names_to = "measure", values_to = "score") |>

  ggplot(aes(x = year, y = score)) +
  geom_line() +
  facet_wrap(~measure)

speeches |>
  filter(year > 1945) |>
  mutate(name_year = str_glue("{person} ({year})")) |>

  ggplot(aes(x = reorder(name_year, re), y = re)) +
  geom_col() +
  coord_flip()
