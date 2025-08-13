#' Specific steps of data preparation for multiple choices questions
#'
#' @param question_data A data.frame containing data of the multiple choice question
#' @param question_info A list with all informations about the question
#' @return A data.frame containing data of the multiple choice question

#' @noRd
#'
pollr_analyze_data_multiple <- function(question_data, question_info) {


  # Identification of "TRUE NA" for multiple choices, when all related columns are NA
  question_data <- question_data |>
    mutate(
      across(
        all_of(question_info$question_varname),
        ~ if_else(if_all(all_of(question_info$question_varname), is.na), "[No answer]", as.character(.))
      )
    )


  # Pivot subquestions
  question_data <- question_data |>
    pivot_longer(cols = question_info$question_varname, names_to = "subquestion", values_to = "response") |>
    mutate(subquestion = fct_relevel(subquestion, question_info$question_varname)) |>  # Relevel subquestion of the order specified in the question_info
    arrange(subquestion) |>
    mutate(response = as_factor(response))      # Use same order for response of subquestion


  # Keep only one "no answer" row by id (intricate... easier solution probably exist)
  question_data_no_answer <- question_data |> # Identify row first, and deduplicate...
    filter( response == "[No answer]") |>
    distinct(id, response, .keep_all = TRUE)

  question_data  <- question_data |> # Then replace multiple Noanswer rows by these
    filter(response != "[No answer]" | is.na(response)) |>
    bind_rows(question_data_no_answer)

  if ("[No answer]" %in% levels(question_data$response)) { # Put Noanswer level at the end, if it exists
    question_data <- question_data %>%
      mutate(response = fct_relevel(response, "[No answer]", after = Inf))
  }



  # Keep or delete NA values depending on the keep_na argument (multichoices questions)
  if (question_info$keep_na) {
    question_data <- question_data
  } else {
    question_data <- question_data |>
      filter(is.na(response) | response != "[No answer]")
  }


  return(question_data)
}





