#' Check that selected variables in a data frame have the same type
#'
#' @param data A data.frame containing the variables.
#' @param varnames A character vector of variable names to check.
#'
#' @return TRUE if all variables have the same type, otherwise throws an error.
#' @noRd

check_same_type <- function(data, varnames) {

  types <- vapply(data[varnames], typeof, character(1))

  if (!all(types == types[1])) {
    stop(
      paste0(
        "Variables do not have the same type. Types found: ",
        paste(unique(types), collapse = ", ")
      )
    )
  }
}


#' Compute Top-n lines for categorical survey results
#'
#' Create a synthetic dataframe of one line for top-n categories (e.g. top2 = first two responses)

#' @param question_design A surveyobject including a variable `response` (factor).

#' @param top a string indicating if we display the top / top2 /top 3 . Values can be "none"(default), "top", "top2" or "top3"
#' @param ci_level a numeric value indicating the confidence interval level (default is 0.95)
#' @param question_results The existing results dataframe (to which top-N will be appended).
#' @return A dataframe with a unique top-N row
#' @noRd

compute_top_n_results <- function(question_design, question_results, top, ci_level) {



  # Extract N from "top2", "top3", etc.
  top_n <- as.numeric(gsub("top", "", top))
  top_n_label <- paste0("Top", top_n)


  # Identify top N modalities from original factor order
  response_initial_order <- levels(question_results$response)
  top_n_levels <- response_initial_order[1:top_n]

  # Create indicator variable for top N
  question_design <- question_design |>
    mutate(top_n_flag = response %in% top_n_levels)

  # Compute survey stats on this flag
  top_n_results <- question_design |>
    group_by(subquestion, cross) |>
    summarize(
      n = survey_total(top_n_flag),
      prop = survey_mean(top_n_flag, vartype = "ci", level = ci_level),
      .groups = "drop"
    ) |>
    rename(ci_low = prop_low, ci_high = prop_upp) |>
    select(-n_se) |>
    mutate(
      response = top_n_label
    ) |>
    select(cross, response, n, prop, ci_low, ci_high)

  return(top_n_results)
}


