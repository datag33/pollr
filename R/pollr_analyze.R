#' Analyze a survey question and return summary data, table and interactive plot
#'
#' @param survey_data A dataframe with survey data
#' @param question_varname A character string, the variable name to analyze
#' @param weight_varname A character string, the variable name for weights (optional)
#'
#' @return A list with three elements:
#' \describe{
#'   \item{data}{A tibble of data question}
#'   \item{results}{A tibble of data question results}
#'   \item{table}{An HTML table of the data question results}
#'   \item{plot}{An interactive plot of the data question results}
#' }
#' @export
pollr_analyse <- function(survey_data, question_varname, weight_varname = NULL) {


  check_pollr_analyze_inputs(survey_data, question_varname, weight_varname)

  question_data <- pollr_analyze_data(survey_data, question_varname, weight_varname)
  question_design <- pollr_analyze_design(question_data)
  question_results <- pollr_analyze_results(question_design)
  question_tab <- pollr_analyze_tab(question_results)
  question_plot <- pollr_analyze_plot(question_results)

  list(
    data = question_data,
    design = question_design,
    results = question_results,
    tab = question_tab,
    plot = question_plot
  )
}







