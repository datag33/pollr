#' Generate an interactive plot for a question
#'
#' @param question_results tibble with question results
#'
#' @return A ggiraph object representing the barplot

#' @importFrom ggiraph geom_col_interactive girafe
#' @noRd
#'
pollr_analyze_plot <- function(question_results) {

  question_results <- question_results |>
    mutate(tooltip = paste0(response, ": ", prop, "%"))

  p <- ggplot(question_results, aes(x = fct_reorder(response, n), y = n)) +
    geom_col_interactive(aes(tooltip = tooltip), fill = "#2c7fb8") +
    coord_flip() +
    labs(x = NULL, y = "n") +
    theme_minimal()

  girafe(ggobj = p)
}
