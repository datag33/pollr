#' Check inputs for pollr_analyse
#'
#' @param survey_data A data.frame containing the survey data
#' @param question_varname A single character string, the name of the question variable
#' @param weight_varname A single character string, the name of the weight variable (optional)
#' @param cross_varname A character string, the variable name for cross-tabulation (optional)
#' @param multiple_choice A boolean indicating whether the question is multiple choice (default is FALSE)
#' @param question_title A single character string, the title of the question (optional)
#' @param question_text A single character string, the text of the question (optional)
#' @param sorted_results A single boolean value indicating whether to sort the results (default is FALSE)
#'
#' @noRd

check_pollr_analyze_inputs <- function(survey_data, question_varname,
                                       multiple_choice,
                                       weight_varname, cross_varname,
                                       question_title, question_text,
                                       sorted_results) {

  #------------------check main arguments ------------------

    # Check if the survey_data is a dataframe
  if (!inherits(survey_data, "data.frame")) {
    stop("`survey_data` must be a data.frame", call. = FALSE)
  }

  # Check multiple choice is a single boolean value
  if (!is.logical(multiple_choice) || length(multiple_choice) != 1) {
    stop("`multiple_choice` must be a single boolean value", call. = FALSE)
  }


  #------------------check question_varname, if no multiple choice ------------------

  if (!multiple_choice) {

      # If no multiple choice, check if the question_varname is a single character string
      if (!is.character(question_varname) || length(question_varname) != 1) {
        stop("`question_varname` must be a single character string", call. = FALSE)
      }

      # If no multiple choice, check if the question_varname exists in the survey_data
      if (!question_varname %in% names(survey_data)) {
        stop(glue::glue("Variable `{question_varname}` not found in `survey_data`."), call. = FALSE)
      }

  }

  #------------------check question_varname, if multiple choices------------------

  if (multiple_choice) {


    # If multichoice, check question_varname must a vector of character strings
    if (length(question_varname) <=1 || !is.character(question_varname)) {
      stop(glue::glue("Variable `{question_varname}` must be a vector of character strings when `multiple_choice` is TRUE."), call. = FALSE)
    }

    # If multichoice, check all question_varname variables exist in the survey_data
    if ( !all(question_varname %in% names(survey_data))) {
      stop(glue::glue("Variable `{question_varname}` not found in `survey_data`."), call. = FALSE)
    }

    # If multichoice, check all question_varname variables in survey_data must be a vector of character or factor
    if( !all(sapply(question_varname, function(var) is.character(survey_data[[var]]) || is.factor(survey_data[[var]])))) {
      stop(glue::glue("Variables `{question_varname}` in `survey_data` must be vectors of character or factor when `multiple_choice` is TRUE."), call. = FALSE)
    }



  }

  #------------------check weight_varname------------------

  # Check if the weight_varname is a single character string if provided
  if (!is.null(weight_varname) && (!is.character(weight_varname) || length(weight_varname) != 1)) {
    stop("`weight_varname` must be a single character string", call. = FALSE)
  }

  # Check if the weight_varname exists in the survey_data if provided
  if (!is.null(weight_varname) && !weight_varname %in% names(survey_data)) {
    stop(glue::glue("Variable `{weight_varname}` not found in `survey_data`."), call. = FALSE)
  }

  # Check if the weight_varname is numeric if provided
  if (!is.null(weight_varname) && !is.numeric(survey_data[[weight_varname]])) {
    stop(glue::glue("Variable `{weight_varname}` must be numeric."), call. = FALSE)
  }

  # Check if the weight_varname has no missing values if provided
  if (!is.null(weight_varname) && any(is.na(survey_data[[weight_varname]]))) {
    stop(glue::glue("Variable `{weight_varname}` must not have missing values."), call. = FALSE)
  }


  #------------------check cross_varname------------------

  # Check if the cross_varname is a single character string if provided
  if (!is.null(cross_varname)) {
    if (!is.character(cross_varname) || length(cross_varname) != 1) {
      stop("`cross_varname` must be a single character string", call. = FALSE)
    }

  # Check if the cross_varname exists in the survey_data if provided
    if (!cross_varname %in% names(survey_data)) {
      stop(glue::glue("Variable `{cross_varname}` not found in `survey_data`."), call. = FALSE)
    }

  }





  #------------------check  question_text and question_title------------------


  # Check if question_title is a single string, if provided
  if (!is.null(question_title) && (!is.character(question_title) || length(question_title) != 1)) {
    stop("`question_title` must be a single character string", call. = FALSE)
  }


  # Check if question_text is a single string, if provided
  if (!is.null(question_text) && (!is.character(question_text) || length(question_text) != 1)) {
    stop("`question_text` must be a single character string", call. = FALSE)
  }


  #------------------check sorted_results------------------

  # Check if sorted_results is a single boolean
  if (!is.logical(sorted_results) || length(sorted_results) != 1) {
    stop("`sorted_results` must be a single boolean value", call. = FALSE)
  }
}
