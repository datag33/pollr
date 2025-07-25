
library(readr)
library(dplyr)

# Import survey date example
df_survey_raw <- read_csv2("data-raw/df_survey_raw.csv")

# Data preparation
df_survey <- df_survey_raw |>
  mutate(
    # Convert date to Date type
    date_submission = as.Date(date_submission, format = "%Y-%m-%d"),

    # Convert grade to factor
    Y3_grade = fct_relevel(Y3_grade, c("Niveau Bac", "Bac + 2 / 3", "Bac + 5", "Bac + 8")),

    # Add weight in order to have 50 / 50 male / females ratios
    weight = if_else(Y1_gender == "Male", 0.5 / 174 * 257,  0.5 / 83 * 257)
)

glimpse(df_survey)

# Save it to data directory
usethis::use_data(df_survey, overwrite = TRUE)






