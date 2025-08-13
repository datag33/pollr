#' Check inputs for pollr_analyze
#'
#' @param survey_data A data.frame containing the survey data
#' @param question_varname A single character string, the name of the question variable
#' @param weight_varname A single character string, the name of the weight variable (optional)
#' @param cross_varname A character string, the variable name for cross-tabulation (optional)
#' @param multiple_choice A boolean indicating whether the question is multiple choice (default is FALSE)
#' @param grid A boolean indicating whether the question is a grid question (default is FALSE)
#' @param question_title A single character string, the title of the question (optional)
#' @param question_text A single character string, the text of the question (optional)
#' @param sorted_results A single boolean value indicating whether to sort the results (default is FALSE)
#' @param keep_na A boolean indicating wheter to keep NA answers as a specific answer (default is TRUE)
#' @param top a string indicating if we display the top / top2 /top 3 results in HTLM tables. Values can be "none"(default), "top", "top2" or "top3"
#' @param ci_level a numeric value indicating the confidence interval level (default is 0.95)
#' @noRd

check_pollr_analyze_inputs <- function(survey_data, question_varname,
                                       multiple_choice, grid,
                                       weight_varname, cross_varname,
                                       question_title, question_text,
                                       sorted_results, keep_na, top, ci_level) {

  #------------------check main arguments ------------------

    # Check if the survey_data is a dataframe
  if (!inherits(survey_data, "data.frame")) {
    stop("`survey_data` must be a data.frame", call. = FALSE)
  }

  # Check multiple choice is a single boolean value
  if (!is.logical(multiple_choice) || length(multiple_choice) != 1) {
    stop("`multiple_choice` must be a single boolean value", call. = FALSE)
  }


  # Check grid is a single boolean value
  if (!is.logical(grid) || length(grid) != 1) {
    stop("`grid` must be a single boolean value", call. = FALSE)
  }



  #------------------check question_varname, if no multiple choice and no grid ------------------

  if (!multiple_choice & !grid) {

      # If no multiple choice, check if the question_varname is a single character string
      if (!is.character(question_varname) || length(question_varname) != 1) {
        stop("`question_varname` must be a single character string", call. = FALSE)
      }

      # If no multiple choice, check if the question_varname exists in the survey_data
      if (!question_varname %in% names(survey_data)) {
        stop(glue::glue("Variable `{question_varname}` not found in `survey_data`."), call. = FALSE)
      }

  }

  #------------------check question_varname, if multiple choices and no grid------------------

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

    # If multichoice, check all variables only have one value, except NA
    # Do it later


  }

  #------------------check question_varname, if grid and no multiple choice------------------

  if (grid & !multiple_choice) {


    # If grid, check question_varname must a vector of character strings
    if (length(question_varname) <=1 || !is.character(question_varname)) {
      stop(glue::glue("Variable `{question_varname}` must be a vector of character strings when `grid` is TRUE."), call. = FALSE)
    }

    # If grid, check all question_varname variables exist in the survey_data
    if ( !all(question_varname %in% names(survey_data))) {
      stop(glue::glue("Variable `{question_varname}` not found in `survey_data`."), call. = FALSE)
    }

    # If grid, check all question_varname variables in survey_data share the same class
    check_same_type(df_survey, question_varname)

  }

  #------------------check question_varname, if grid and multi choice------------------

  if (grid & multiple_choice) {
    stop("Grid and multiple_choice not supported together by pollr, at the moment.", call. = FALSE)
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

  #------------------check keep_na ------------------

  # Check if keep_na is a single boolean
  if (!is.logical(keep_na) || length(keep_na) != 1) {
    stop("`keep_na` must be a single boolean value", call. = FALSE)
  }


  #------------------check top ------------------

  # Check if top is a single string, with specific values
  if (!is.character(top) || length(top) > 1 || !top %in%  c("none", "top", "top2", "top3")) {
    stop("`top` must be a single character value, with 'none', 'top', 'top2' or 'top3' as value", call. = FALSE)
  }

  #------------------check ci_level ------------------

  # Check if ci_level is a single numerical value from 0 to 1
  if (!is.numeric(ci_level) || length(ci_level) > 1 || ci_level > 1 || ci_level < 0) {
    stop("`ci_level` must be a single numerical value between 0 and 1", call. = FALSE)
  }
}
