
library(readr)
library(dplyr)

# Import survey date example
df_survey_raw <- read_csv2("data-raw/df_survey_raw.csv")

# Data preparation
df_survey <- df_survey_raw |>
  mutate(
    # Convert date to Date type
    date_submission = as.Date(date_submission, format = "%Y-%m-%d")
  )

glimpse(df_survey)

# Save it to data directory
usethis::use_data(df_survey, overwrite = TRUE)


