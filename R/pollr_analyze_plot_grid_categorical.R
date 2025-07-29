#' Generates an interactive plot for a grid question with categorical data , using survey results
#'
#' @param question_grid_results tibble with question results for all grid elements
#' @param question_info A list with all informations about the question
#'
#' @return A ggiraph object of the plot
#' @importFrom ggiraph geom_col_interactive girafe
#' @noRd
#'
#'
pollr_analyze_plot_grid_categorical <- function(question_grid_results, question_info) {

  # Prepare tooltip
  question_grid_results <- question_grid_results |>
    mutate(
      tooltip = glue("{questions_names} {response} ({cross}) : <b>{prop} % </b> \n <i>n = {n}</i>")
    )


  # Create barplot
  question_grid_plot <- ggplot(question_grid_results, aes(x = prop, y = questions_names, fill = fct_rev(response))) +
    geom_col_interactive(aes(tooltip = tooltip)) +
    theme_minimal() +
    labs(
      title = question_info$question_title,
      subtitle = question_info$question_text
    )

  # Facet by cross variable if it exists
  if (!is.null(question_info$cross_varname)) {
    question_grid_plot <- question_grid_plot +
      facet_wrap(~cross, scales = "fixed")
  }

  girafe(ggobj = question_grid_plot)

}
