#' Generates an interactive plot for a single question with categorial data (multiple choice or not), using survey results
#'
#' @param question_results tibble with question results
#' @param question_sample_size A tibble with sample sizes data, total and cross targets if any
#' @param question_info A list with all informations about the question
#'
#' @return A ggiraph object of the plot
#' @importFrom ggiraph geom_col_interactive girafe geom_errorbar_interactive
#' @noRd
#'
#'
pollr_analyze_plot_categorical <- function(question_results, question_sample_size, question_info) {

# Exclude top / top2 for plots
  question_results <- question_results |>
    filter(!response %in% c("Top", "Top2", "Top3"))

  # Total sample size value
  total_sample_size <- question_sample_size |> filter(target == "Total") |> pull(n)

  # Update cross variable with targets sample size
  # TO DO (do not lose the factor...)

  # question_results <- question_results |>
  #   left_join(question_sample_size |>
  #               filter(target != "Total") |>
  #               rename(n_target = n) |>
  #                mutate(target = factor(target, levels = levels(question_results$cross))),
  #             by = c("cross" = "target"))
  #
  # question_results <- question_results |>
  #   mutate(
  #     cross = fct_relabel(cross, ~ paste0(.x, " (n = ", n_target, ")"))
  #   )



# Prepare tooltip
question_results <- question_results |>
  mutate(
    bar_tooltip = glue("{response} ({cross}) : <b>{prop} % </b> \n <i>n = {n}</i>"),
    errorbar_tooltip = glue("{question_info$ci_level} confidence interval of proportion : {ci_low} % - {ci_high} %")
  )



# Create barplot
question_plot <- question_results |>
  ggplot(aes(
    x = prop,
    y = fct_rev(response))
    ) +
  geom_col_interactive( # Bars
    aes(tooltip = bar_tooltip),
    fill = "#2c7fb8",
    alpha = 0.8
    ) +
  geom_errorbar_interactive(  # Error bar
    aes(xmin = ci_low, xmax = ci_high, tooltip = errorbar_tooltip),
    color = "black",
    linewidth = 0.4,
    width = 0.10
  ) +
  geom_text( # Label (without decimal)
    aes(y = fct_rev(response), label = paste0(prop |> round(0), " %")),
    color = "black", fontface ="bold", size = 3, hjust = -0.4, vjust = -0.6
  ) +
  theme_pollr() + # Theme
  scale_x_continuous( # X scale
    labels = function(x) paste0(x, " %")
  ) +
  geom_vline( # Horizontal axis
    xintercept = 0, color = "grey85", linewidth = 0.3
    ) +
  labs( # Labels on titles / subtitles / caption / axis
    title = question_info$question_title,
    subtitle = glue("{question_info$question_text} (n = {total_sample_size})"),
    x = "%",
    y = "",
    caption = glue("n = {total_sample_size}")
  )

# Facet by cross variable if it exists
if (!is.null(question_info$cross_varname)) {
  question_plot <- question_plot +
    facet_wrap(~cross, scales = "fixed")
}

girafe(ggobj = question_plot)

}
