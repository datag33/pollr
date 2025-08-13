#' Perform a global test on survey data for a single question with numerical data, using survey design
#'
#' @param question_design A survey design object
#' @param question_test A tibble with test statistic, degrees of freedom, pvalue and significancy level
#' @param question_info A list with all informations about the question
#'
#' @return A tibble with test statistic, degrees of freedom, pvalue and significancy level
#' @importFrom survey svyglm regTermTest
#' @noRd
#'
#'
pollr_analyze_test_numerical <- function(question_design, question_test, question_info) {

  # Fit a weighted linear model with svyglm
  model <- svyglm(response ~ cross, design = question_design)

  # Test global effect of 'cross' on response (ANOVA-style)
  test <- survey::regTermTest(model, "cross")

  # Output
  question_test <- tibble::tibble(
    statistic = test$Ftest[[1]],
    df = test$df,  # Numerator degrees of freedom = number of groups - 1
    p_value = test$p |> round(3),
    significancy = interpret_pvalue(test$p)
  )

  return(question_test)
}
