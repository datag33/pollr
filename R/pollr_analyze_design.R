#' Create survey design object from survey data
#'
#' @param question_data A tibble with question data, including response and weights variables
#' @return A survey design object (svydesign).
#' @importFrom srvyr as_survey_design
#' @noRd
pollr_analyze_design <- function(question_data) {

  design <- question_data |>
    as_survey_design(weights = weight)

  return(design)
}
