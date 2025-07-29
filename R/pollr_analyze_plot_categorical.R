#' Generates an interactive plot for a single question with categorial data (multiple choice or not), using survey results
#'
#' @param question_results tibble with question results
#' @param question_info A list with all informations about the question
#'
#' @return A ggiraph object of the plot
#' @importFrom ggiraph geom_col_interactive girafe
#' @noRd
#'
#'
pollr_analyze_plot_categorical <- function(question_results, question_info) {

# Prepare tooltip
question_results <- question_results |>
  mutate(
    tooltip = glue("{response} ({cross}) : <b>{prop} % </b> \n <i>n = {n}</i>")
  )


# Create barplot
question_plot <- ggplot(question_results, aes(x = prop, y = fct_rev(response))) +
  geom_col_interactive(aes(tooltip = tooltip), fill = "#2c7fb8") +
  theme_minimal() +
  labs(
    title = question_info$question_title,
    subtitle = question_info$question_text,
    x = "%",
    y = ""
  )

# Facet by cross variable if it exists
if (!is.null(question_info$cross_varname)) {
  question_plot <- question_plot +
    facet_wrap(~cross, scales = "fixed")
}

girafe(ggobj = question_plot)

}
