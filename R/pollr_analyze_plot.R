#' Generate an interactive plot for a question
#'
#' @param question_results tibble with question results
#' @param question_sample_size A tibble with sample sizes data, total and cross targets if any
#' @param question_info A list with all informations about the question
#'
#' @return A ggiraph object of the plot
#' @importFrom ggiraph geom_col_interactive girafe
#' @noRd
#'
pollr_analyze_plot <- function(question_results, question_sample_size, question_info) {


  if (question_info$question_type == "numerical") {

    if (!is.null(question_info$cross_varname)) {
      plot <- pollr_analyze_plot_numerical(question_results, question_sample_size, question_info)
    }
  else message ("No plot created for single numerical variable without cross variable")

    }  else if (question_info$question_type == "categorical")
      plot <- pollr_analyze_plot_categorical(question_results, question_sample_size, question_info)

  else stop(glue::glue("Variable `{question_info$question_varname}` must be numerical or categorical"), call. = FALSE)

  return(plot)


}
