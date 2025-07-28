#' List all information about the survey and the question processing
#'
#' @param survey_data A data.frame containing the survey data
#' @param question_varname A single character string, the name of the question variable
#' @param weight_varname A single character string, the name of the weight variable
#' @param cross_varname A character string, the variable name for cross-tabulation (optional)
#' @param multiple_choice A boolean indicating whether the question is multiple choice (default is FALSE)
#' @param question_title A single character string, the title of the question (optional)
#' @param question_text A single character string, the text of the question (optional)
#' @param sorted_results A boolean indicating whether to sort the results
#' @noRd

pollr_analyze_info <- function(survey_data, question_varname, multiple_choice,
                               weight_varname, cross_varname , question_title, question_text, sorted_results) {

  # Variable type, if no multiplkce choice
  if (!multiple_choice) {
    if (inherits(survey_data[[question_varname]], "numeric"))
      question_type <- "numerical"
      else if (inherits(survey_data[[question_varname]], "factor") | inherits(survey_data[[question_varname]], "character"))
      question_type <- "categorical"
      else
        stop(glue::glue("Variable `{question_varname}` must be numeric, factor or character."), call. = FALSE)
    }

  else {
    # Multiple choice is always categorical
    question_type <-  "categorical"
  }

  list(
    question_varname = question_varname,
    multiple_choice = multiple_choice,
    question_type = question_type,
    weight_varname   = weight_varname,
    cross_varname = cross_varname,
    question_title = question_title,
    question_text = question_text,
    sorted_results   = sorted_results,
    process_date     = Sys.time()
  )
}


