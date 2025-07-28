#' Analyze a survey question and return summary data, table and interactive plot
#'
#' @param survey_data A dataframe with survey data
#' @param question_varname A character string, the variable name to analyze
#' @param weight_varname A character string, the variable name for weights (optional)
#' @param cross_varname A character string, the variable name for cross-tabulation (optional)
#' @param multiple_choice A boolean indicating whether the question is multiple choice (default is FALSE)
#' @param question_title A character string, the main title of the question (optional)
#' @param question_text A character string, the detailled text of the question (optional)
#' @param sorted_results A boolean indicating whether to sort the results (default is FALSE)
#'
#'
#' @return A list with six elements:
#' \describe{
#'   \item{info}{A list with all informations about the question}
#'   \item{data}{A tibble of data question}
#'   \item{design}{A survey design of data question}
#'   \item{info}{A list with all informations about the question}
#'   \item{results}{A tibble of data question results}
#'   \item{table}{An HTML table of the data question results}
#'   \item{plot}{An interactive plot of the data question results}
#' }
#' @export
#'
pollr_analyse <- function(survey_data,
                          question_varname,  multiple_choice = FALSE,
                          weight_varname = NULL, cross_varname = NULL,
                          question_title = "", question_text = "",
                          sorted_results = FALSE) {

  check_pollr_analyze_inputs(survey_data, question_varname, multiple_choice, weight_varname, cross_varname ,question_title, question_text, sorted_results)

  question_info <- pollr_analyze_info(survey_data, question_varname, multiple_choice, weight_varname, cross_varname, question_title, question_text, sorted_results)

  question_data <- pollr_analyze_data(survey_data, question_info)
  question_design <- pollr_analyze_design(question_data)

  question_results <- pollr_analyze_results(question_design, question_info)
  question_tab <- pollr_analyze_tab(question_results, question_info)
  #question_plot <- pollr_analyze_plot(question_results, question_info)

  list(
    data = question_data,
    info = question_info,
    design = question_design,
    results = question_results,
    tab = question_tab
    #plot = question_plot
  )
}







