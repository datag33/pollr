#' Compute results for a single question, using survey design
#'
#' @param question_design A survey design object
#'
#' @return A tibble with question results
#' @importFrom srvyr survey_mean survey_total
#' @noRd
#'
#'
pollr_analyze_results <- function(question_design) {


  results <- question_design |>
    group_by(response) |>
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
     n = round(n, 1),
     across(c(prop, ci_low, ci_high), ~ round(100 * ., 1))
     )

  return(results)

}
