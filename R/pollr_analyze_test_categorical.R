#' Perform a global test on survey data for a single question with categorial data (multiple choice or not), using survey design
#'
#' @param question_design A survey design object
#' @param question_info A list with all informations about the question
#' @return A tibble with test statistic, degrees of freedom, pvalue and significancy level
#' @importFrom survey svychisq
#' @noRd
#'
#'
pollr_analyze_test_categorical <- function(question_design, question_info) {

  test <- svychisq(~response + cross, design = question_design, statistic = "F")

  question_test <- tibble(
    statistic = test$statistic,
    df = test$parameter[[1]],
    p_value = test$p.value |> round(3),
    significancy = interpret_pvalue(p_value)
  )

  return(question_test)
}
