#' Generates an interactive plot for a question single question with numerical data (multiple choice or not), using survey results
#'
#' @param question_results tibble with question results
#' @param question_info A list with all informations about the question
#'
#' @return A ggiraph object of the plot
#' @importFrom ggiraph geom_col_interactive girafe
#' @noRd
#'
#'
pollr_analyze_plot_numerical <- function(question_results, question_info) {

  # Prepare tooltip
  question_results <- question_results |>
    mutate(
      tooltip = glue("Average for {cross} : <b>{mean} </b> \n <i>n = {n}</i>")
    )


  # Create barplot of averages
  question_plot <- ggplot(question_results, aes(x = mean, y = cross)) +
    geom_col_interactive(aes(tooltip = tooltip), fill = "#2c7fb8") +
    theme_minimal() +
    labs(
      title = question_info$question_title,
      subtitle = question_info$question_text,
      x = "Mean",
      y = ""
    )


  girafe(ggobj = question_plot)

}
