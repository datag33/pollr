#' Check inputs for pollr_analyse
#'
#' @param survey_data A data.frame containing the survey data
#' @param question_varname A single character string, the name of the question variable
#' @param weight_varname A single character string, the name of the weight variable (optional)
#' @importFrom glue glue
#' @noRd

check_pollr_analyze_inputs <- function(survey_data, question_varname, weight_varname = NULL) {

  #------------------check main arguments ------------------

    # Check if the survey_data is a dataframe
  if (!inherits(survey_data, "data.frame")) {
    stop("`survey_data` must be a data.frame", call. = FALSE)
  }

  # Check if the question_varname is a single character string
  if (!is.character(question_varname) || length(question_varname) != 1) {
    stop("`question_varname` must be a single character string", call. = FALSE)
  }

  # Check if the question_varname exists in the survey_data
  if (!question_varname %in% names(survey_data)) {
    stop(glue::glue("Variable `{question_varname}` not found in `survey_data`."), call. = FALSE)
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

  # Check if the survey_data has no missing values in the question_varname
  if (any(is.na(survey_data[[question_varname]]))) {
    stop(glue::glue("Variable `{question_varname}` must not have missing values."), call. = FALSE)
  }


}
