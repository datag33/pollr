#' Select and prepare the required data for the question analysis
#'
#' @param survey_data A data.frame containing the survey data
#' @param question_info A list with all informations about the question
#' @return A tibble with question data, including response and weights variables

#' @noRd
#'
pollr_analyze_data <- function(survey_data, question_info) {

  # Select relevant data from survey_data
  vars_to_select <- c(question_info$question_varname, question_info$weight_varname, question_info$cross_varname)

  question_data <- survey_data |>
    select(all_of(vars_to_select))

  # Specific handling for multiple choice questions
  if (question_info$multiple_choice) {
    question_data <- question_data |>
      pivot_longer(cols = question_info$question_varname, names_to = "subquestion", values_to = "response") |>
      mutate(subquestion = fct_relevel(subquestion, question_info$question_varname)) |>  # Relevel subquestion of the order specified in the question_info
      arrange(subquestion) |>
      mutate(response = as_factor(response))  # Use same order for response of subquestion
  } else
      question_data <-  question_data |> rename(response = all_of(question_info$question_varname))

  # Create a weight variable of 1 if no weight_varname
  if (is.null(question_info$weight_varname)) {
    question_data <- question_data |>
      mutate(weight = 1 )
  } else {
    question_data <- question_data |>
      rename(weight = all_of(question_info$weight_varname))
  }

  # Create a cross variable of "no group" if no cross variable
  if (is.null(question_info$cross_varname)) {
    question_data <- question_data |> mutate(cross = "no group")
  } else {
    question_data <- question_data |>
      rename(cross = all_of(question_info$cross_varname))
  }


  # Create a subquestion variable of "no subquestion" if no multiplechoice
  if (!question_info$multiple_choice) {
    question_data <- question_data |> mutate(subquestion = "no subquestion")
  }



  return(question_data)
}





