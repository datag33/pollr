#' List all information about the survey and the question processing
#'
#' @param survey_data A data.frame containing the survey data
#' @param question_varname A single character string, the name of the question variable
#' @param weight_varname A single character string, the name of the weight variable
#' @param sorted_results A boolean indicating whether to sort the results
#' @noRd

pollr_analyze_info <- function(survey_data, question_varname, weight_varname, cross_varname , question_title, question_text, sorted_results) {

  if (inherits(survey_data[[question_varname]], "numeric"))
    question_type <- "numerical"
    else if (inherits(survey_data[[question_varname]], "factor") | inherits(survey_data[[question_varname]], "character"))
    question_type <- "categorical"
    else
      stop(glue::glue("Variable `{question_varname}` must be numeric, factor or character."), call. = FALSE)


  list(
    question_varname = question_varname,
    question_type = question_type,
    weight_varname   = weight_varname,
    cross_varname = cross_varname,
    question_title = question_title,
    question_text = question_text,
    sorted_results   = sorted_results,
    process_date     = Sys.time()
  )
}


