#' Compute results for a single question with numerical data, using survey design
#'
#' @param question_design A survey design object
#' @param question_info A list with all informations about the question
#'
#' @return A tibble with question results
#' @importFrom srvyr survey_mean survey_total survey_sd
#' @noRd
#'
#'
pollr_analyze_results_numerical <- function(question_design, question_info) {


  results <- question_design |>
    group_by(cross) |>
    summarize(
      n = survey_total(),
      mean = survey_mean(response, vartype = "ci", level = 0.95),
      sd = survey_sd(response)
    ) |>
    select(-n_se) |>
    rename(
      ci_low = mean_low,
      ci_high = mean_upp
    ) |>
    mutate(
      n = round(n), # Round all counts
      across(c(mean, sd, ci_low, ci_high), ~ round(., 2)) # Round with 2 decimal means and SDs
    )

  # If sorted results, then reorder the cross factor depending on averages
  if (question_info$sorted_results) {

    cross_order <- results |>
      arrange(desc(mean )) |>
      pull(cross)

    results <-  results |>
      mutate(
        cross = fct_relevel(cross, cross_order)
      )
  }

  # Sort the results using cross factor
  results <-  results |>
    arrange(cross)

  return(results)
}


