#' List all information about the survey and the question processing
#'
#' @param survey_data A data.frame containing the survey data
#' @param question_varname A single character string, the name of the question variable
#' @param weight_varname A single character string, the name of the weight variable
#' @param cross_varname A character string, the variable name for cross-tabulation (optional)
#' @param multiple_choice A boolean indicating whether the question is multiple choice (default is FALSE)
#'@param grid A boolean indicating whether the question is a grid question (default is FALSE)
#' @param question_title A single character string, the title of the question (optional)
#' @param question_text A single character string, the text of the question (optional)
#' @param sorted_results A boolean indicating whether to sort the results*
#' @param top a string indicating if we display the top / top2 /top 3 results in HTLM tables. Values can be "none"(default), "top", "top2" or "top3"
#' @param ci_level a numeric value indicating the confidence interval level (default is 0.95)
#' @noRd

pollr_analyze_info <- function(survey_data, question_varname, multiple_choice, grid,
                               weight_varname, cross_varname ,
                               question_title, question_text,
                               sorted_results, top, ci_level) {

  # Variable type, if no multiple choice and no grid
  if (!multiple_choice & !grid) {
    if (inherits(survey_data[[question_varname]], "numeric"))
      question_type <- "numerical"
      else if (inherits(survey_data[[question_varname]], "factor") | inherits(survey_data[[question_varname]], "character"))
      question_type <- "categorical"
      else
        stop(glue::glue("Variable `{question_varname}` must be numeric, factor or character."), call. = FALSE)
  }

  # Multiple choice is always categorical
  if (multiple_choice) {
    question_type <-  "categorical"
  }

  # If grid, just use the type of this first variable, as all variables are from the same type
  if (grid) {
    if (inherits(survey_data[[question_varname[1]]], "numeric"))
      question_type <- "numerical"
    else if (inherits(survey_data[[question_varname[1]]], "factor") | inherits(survey_data[[question_varname[1]]], "character"))
      question_type <- "categorical"
    else
      stop(glue::glue("Variable `{question_varname}` must be numeric, factor or character."), call. = FALSE)
  }


  # If no title specified, use the varname as title for single question
  if (question_title == "" & !multiple_choice & !grid) { question_title <- question_varname}

  list(
    question_varname = question_varname,
    multiple_choice = multiple_choice,
    grid = grid,
    question_type = question_type,
    weight_varname   = weight_varname,
    cross_varname = cross_varname,
    question_title = question_title,
    question_text = question_text,
    sorted_results   = sorted_results,
    top = top,
    ci_level = ci_level,
    process_date     = Sys.time()
  )
}


