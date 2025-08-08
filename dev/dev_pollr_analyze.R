devtools::document()
devtools::load_all()
devtools::check()

library(gtsummary) # For checks
library(dplyr)
library(purrr)
options(error = NULL)

# ----------------------------Examples on single question---------------------


# Example 1 : single question, no weights, no cross variable

pollr_analyze_example1 <- pollr_analyze(
  survey_data = df_survey,
  question_varname = "A1_title",
  question_title = "Title",
  question_text = "What is your title?"
)

pollr_analyze_example1$data
question_results <- pollr_analyze_example1$results
pollr_analyze_example1$tab
pollr_analyze_example1$plot

tbl_summary(df_survey, include = A1_title) # Check gt summary


# Example 2 : single question, weight variable, no cross variable

pollr_analyze_example2 <- pollr_analyze(
  survey_data = df_survey,
  question_varname = "Y1_gender",
  weight_varname = "weight"
)

pollr_analyze_example2$data
pollr_analyze_example2$results



pollr_analyze_example2$plot

df_survey |> as_survey_design(weights = weight) |>
  tbl_svysummary(include = Y1_gender) # Check gt summary


# Example 3 : single question, weight, cross variable, without sorting


pollr_analyze_example3 <- pollr_analyze(
  survey_data = df_survey,
  question_varname = "Y3_grade",
  weight_varname = "weight",
  cross_varname = "Y1_gender",
  question_title = "Grade",
  question_text = "What is your grade ?",
  top = "top2"
)

pollr_analyze_example3$data
question_design <- pollr_analyze_example3$design

question_results <- pollr_analyze_example3$results
pollr_analyze_example3$tab
pollr_analyze_example3$plot

df_survey |> as_survey_design(weights = weight) |>
  tbl_svysummary(include = Y3_grade, by = Y1_gender) # Check gt summary


# Example 4 : single question, weight, cross variable, sorted results

pollr_analyze_example4 <- pollr_analyze(
  survey_data = df_survey,
  question_varname = "A1_title",
  weight_varname = "weight",
  cross_varname = "Y1_gender",
  sorted_results = TRUE, top = "top2"
)

question_results <- pollr_analyze_example4$results
pollr_analyze_example4$tab



# Example 5 : single numeric question, weight, no cross

pollr_analyze_example5 <- pollr_analyze(
  survey_data = df_survey,
  question_varname = "A3_experience",
  weight_varname = "weight",
  question_text = "What is your experience level?"
)


pollr_analyze_example5$info
pollr_analyze_example5$data
question_design <- pollr_analyze_example5$design
question_results <- pollr_analyze_example5$results
pollr_analyze_example5$tab

df_survey |> as_survey_design(weights = weight) |>
  tbl_svysummary(include = A3_experience) # Check gt summary


# Example 6 : single numeric question, weight, cross variable

pollr_analyze_example6 <- pollr_analyze(
  survey_data = df_survey,
  question_varname = "A3_experience",
  weight_varname = "weight",
  cross_varname = "Y1_gender",
  question_title = "Experience level",
  question_text = "What is your experience level?",
  sorted_results = TRUE
)


pollr_analyze_example6$info
pollr_analyze_example6$data
pollr_analyze_example6$design
question_results <- pollr_analyze_example6$results
pollr_analyze_example6$tab
pollr_analyze_example6$plot

df_survey |> as_survey_design(weights = weight) |>
  tbl_svysummary(include = A3_experience, by = Y1_gender) # Check gt summary



# Example 7 : multiple choice question, no weight, no cross

survey_data <- df_survey
question_varname <-  c("B1_using_r", "B1_using_python","B1_using_excel", "B1_using_sql")

pollr_analyze_example7 <- pollr_analyze(
  survey_data = survey_data,
  multiple_choice = TRUE,
  question_varname = question_varname)


pollr_analyze_example7$data
question_design <- pollr_analyze_example7$design
pollr_analyze_example7$results
pollr_analyze_example7$tab
pollr_analyze_example7$plot



# Example 8 : multiple choice question, weight, cross variable

survey_data <- df_survey
question_varname <-  c("B1_using_r", "B1_using_python", "B1_using_sql")

pollr_analyze_example8 <- pollr_analyze(
  survey_data = survey_data,
  multiple_choice = TRUE,
  question_varname = question_varname,
  weight_varname = "weight",
  cross_varname = "Y1_gender",
  question_title = "Sofware usage",
  question_text = "How often are you using such software...?",
  sorted_results = TRUE)

pollr_analyze_example8$plot

# Example 9 : tableau de questions simple


question_varname1 <-  c("B2_frequency_r")
question_varname2 <-  c("B2_frequency_python")
question_varname3 <-  c("B2_frequency_sql")
question_varnames <- c(question_varname1, question_varname2, question_varname3)

pollr_analyze_example9 <- pollr_analyze(survey_data = df_survey,question_varname = question_varnames, grid = TRUE, top = "top2")
pollr_analyze_example9$results
pollr_analyze_example9$data
pollr_analyze_example9$tab
pollr_analyze_example9$plot

# pollr_analyze_example9a <- pollr_analyze(survey_data = df_survey,question_varname = question_varname1)
# pollr_analyze_example9b <- pollr_analyze(survey_data = df_survey,question_varname = question_varname2)
# pollr_analyze_example9c <- pollr_analyze(survey_data = df_survey,question_varname = question_varname3)
#
# test <- question_varnames |> purrr::map(~pollr_analyze(df_survey, question_varname = .x))
# test[[1]]
#



# Example 9 : tableau de questions, weight, cross variable



pollr_analyze_example10 <- pollr_analyze(survey_data = df_survey,
                                         question_varname = c("B2_frequency_r", "B2_frequency_python", "B2_frequency_sql"),
                                         grid = TRUE,
                                         weight_varname = "weight",
                                         cross_varname = "Y1_gender",
                                        question_title = "Software usages",
                                         top = "top")
question_grid_results <- pollr_analyze_example10$results
pollr_analyze_example10$data
pollr_analyze_example10$tab





# Exemple à part autre données vinometre

library(readxl)
library(dplyr)
df_survey_vinometre <- read_excel("F:/D_Projets/Vinometre/Package_vinometre/vinometre/data_raw/data_bouchons.xlsx")

glimpse(df_survey_vinometre)
pollr_analyze(df_survey_vinometre, question_varname = c("Prefere_rouge_metal", "Prefere_rouge_liege", "Prefere_rouge_verre"), multiple_choice = TRUE, sorted_results = TRUE)
pollr_analyze(df_survey_vinometre, question_varname = "Connaissance_vin")



# TEMP

survey_data |>
 pollr_analyze(question_varname = "B2_frequency_r", sorted_results = TRUE, top = "top2")
