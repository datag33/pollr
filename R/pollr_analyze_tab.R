#' Generate a HTML table for a question
#'
#' @param question_results tibble with question results
#' @param question_info A list with all informations about the question
#'
#' @return A kableExtra HTML table object

#' @noRd
#'
pollr_analyze_tab <- function(question_results, question_info) {

  if (question_info$question_type == "numerical")
    tab <- pollr_analyze_tab_numerical(question_results, question_info)

  else if (question_info$question_type == "categorical")
    tab <- pollr_analyze_tab_categorical(question_results, question_info)

  else stop(glue::glue("Variable `{question_info$question_varname}` must be numerical or categorical"), call. = FALSE)

  return(tab)

}
