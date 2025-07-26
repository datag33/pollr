#' Compute results for a single question with categorial data, using survey design
#'
#' @param question_design A survey design object
#' @param question_info A list with all informations about the question
#'
#' @return A tibble with question results
#' @importFrom srvyr survey_mean survey_total
#' @noRd
#'
#'
pollr_analyze_results_categorical <- function(question_design, question_info) {


  results <- question_design |>
    group_by(cross, response) |>
    summarize(
      n = survey_total(),
      prop = survey_mean(vartype = "ci", level = 0.95, proportion = TRUE),
    ) |>
    select(-n_se) |>
    rename(
      ci_low = prop_low,
      ci_high = prop_upp
    ) |>
    mutate(
      n = round(n), # Round all counts
      across(c(prop, ci_low, ci_high), ~ round(100 * ., 1)) # Round with 1 decimal proportions and CI levels
    )

  # If sorted results, then reorder the response factor depending on total counts
  if (question_info$sorted_results) {

    response_order <- results |>
      group_by(response) |>
      summarize(n = sum(n)) |>
      arrange(desc(n)) |>
      pull(response)

    results <-  results |>
      mutate(
        response = fct_relevel(response, response_order)
      )
  }

  # Sort the results using response factor
  results <-  results |>
    arrange(cross, response)


  return(results)
}
