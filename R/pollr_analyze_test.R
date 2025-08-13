#' Perform a global test on survey data, if there is a cross variable
#'
#' @param question_design A survey design object
#' @param question_info A list with all informations about the question
#'
#' @return A tibble with test statistic, p value and significancy level
#' @noRd
#'
#'
pollr_analyze_test <- function(question_design, question_info) {


  if (question_info$question_type == "numerical")
    test <- pollr_analyze_test_numerical(question_design, question_info)

    else if (question_info$question_type == "categorical")
      test <- pollr_analyze_test_categorical(question_design, question_info)

    else stop(glue::glue("Variable `{question_info$question_varname}` must be numerical or categorical"), call. = FALSE)

  return(test)

}
