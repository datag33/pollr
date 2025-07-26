#' Compute results for a single question, using survey design
#'
#' @param question_design A survey design object
#' @param question_info A list with all informations about the question
#'
#' @return A tibble with question results
#' @noRd
#'
#'
pollr_analyze_results <- function(question_design, question_info) {


  if (question_info$question_type == "numerical")
    results <- pollr_analyze_results_numerical(question_design, question_info)

    else if (question_info$question_type == "categorical")
      results <- pollr_analyze_results_categorical(question_design, question_info)

    else stop(glue::glue("Variable `{question_info$question_varname}` must be numerical or categorical"), call. = FALSE)

  return(results)

}
