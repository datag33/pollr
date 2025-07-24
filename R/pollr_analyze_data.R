#' Select and prepare the required data for the question analysis
#'
#' @param survey_data A data.frame containing the survey data
#' @param question_varname A character string, name of the variable to analyze
#' @param weight_varname A character string, name of the variable for weights (optional)
#'
#' @return A tibble with question data, including response and weights variables

#' @noRd
#'
pollr_analyze_data <- function(survey_data, question_varname, weight_varname = NULL) {

  # Select relevant data from survey_data
  vars_to_select <- c(question_varname, weight_varname)

  question_data <- survey_data |>
    select(all_of(vars_to_select)) |>
    rename(response = all_of(question_varname))

  # Create a weight variable of 1 if no weight_varname
  if (is.null(weight_varname)) {
    question_data <- question_data |>
      mutate(weight = 1 )
  } else {
    question_data <- question_data |>
      rename(weight = all_of(weight_varname))
  }

  return(question_data)
}





