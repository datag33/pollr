#' Generate a HTML table for a question
#'
#' @param question_results tibble with question results
#'
#' @return A kableExtra HTML table object
#' @importFrom kableExtra kable kable_styling
#' @noRd
#'
pollr_analyze_tab <- function(question_results) {
  question_results |>
    mutate(
      prop = paste0("<b>", prop, "%</b>")
    ) |>
    kable(format = "html", escape = FALSE, align = "lcr", caption = "Survey results") |>
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
}
