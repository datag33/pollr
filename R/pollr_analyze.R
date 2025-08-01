#' Analyze a survey question and return summary data, table and interactive plot
#'
#' @param survey_data A dataframe with survey data
#' @param question_varname A character string, the variable name to analyze
#' @param weight_varname A character string, the variable name for weights (optional)
#' @param cross_varname A character string, the variable name for cross-tabulation (optional)
#' @param multiple_choice A boolean indicating whether the question is multiple choice (default is FALSE)
#' @param grid A boolean indicating whether the question is a grid question (default is FALSE)
#' @param question_title A character string, the main title of the question (optional)
#' @param question_text A character string, the detailled text of the question (optional)
#' @param sorted_results A boolean indicating whether to sort the results (default is FALSE)
#' @param top a string indicating if we display the top / top2 /top 3 results in HTLM tables. Values can be "none"(default), "top", "top2" or "top3"
#' @param ci_level a numeric value indicating the confidence interval level (default is 0.95)
#'
#' @return A named list with six elements:
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
pollr_analyze <- function(survey_data,
                          question_varname,  multiple_choice = FALSE, grid = FALSE,
                          weight_varname = NULL, cross_varname = NULL,
                          question_title = "", question_text = "",
                          sorted_results = FALSE, top = "none", ci_level = 0.95) {


  check_pollr_analyze_inputs(survey_data, question_varname, multiple_choice, grid, weight_varname, cross_varname ,question_title, question_text, sorted_results, top, ci_level)

  question_info <- pollr_analyze_info(survey_data, question_varname, multiple_choice, grid, weight_varname, cross_varname, question_title, question_text, sorted_results, top, ci_level)


  # Single questions (including multiple choice)
  if (!grid){
  question_data <- pollr_analyze_data(survey_data, question_info)
  question_design <- pollr_analyze_design(question_data)
  question_results <- pollr_analyze_results(question_design, question_info)
  question_tab <- pollr_analyze_tab(question_results, question_info)
  #question_plot <- pollr_analyze_plot(question_results, question_info)
  }


  # Grid questions (likert matrix)
  else {

    question_grid_all <- pollr_analyze_grid(survey_data, question_info)
    question_data <- question_grid_all$data
    question_design <- question_grid_all$design
    question_results <- question_grid_all$results
    question_tab <- question_grid_all$tab
    question_plot <- question_grid_all$plot
  }



  question_all <- list(
    data = question_data,
    info = question_info,
    design = question_design,
    results = question_results,
    tab = question_tab
    #plot = question_plot
  )


  return(question_all)
}







