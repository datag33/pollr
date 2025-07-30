#' Compute results for a single question with categorial data (multiple choice or not), using survey design
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


  question_results <- question_design |>
    group_by(subquestion,cross, response) |>
    summarize(
      n = survey_total(), # Count
      prop = survey_mean(vartype = c("ci"), level = question_info$ci_level) # Proportion and standard error
    ) |>
    ungroup() |>
    select(-n_se) |> # se not needed for n
    rename(
      ci_low = prop_low,
      ci_high = prop_upp
    ) |>
    filter(!is.na(response)) # Remove NA


  # # Remember original order of responses (factor levels if any)
  # response_initial_order <- question_results |>
  #   pull(response) |>
  #   unique() |> as.character()

  # If sorted results, then reorder the response factor depending on total counts
  if (question_info$sorted_results) {

    response_order <- question_results|>
      group_by(response) |>
      summarize(n = sum(n)) |>
      arrange(desc(n)) |>
      pull(response) |>
      as.character()

    question_results <-  question_results |>
      mutate(
        response = fct_relevel(response, response_order)
      )
  }



  # Sort the results using response factor, in ascending order
  question_results <-  question_results |>
    arrange(cross, response)


  # If no cross variable, remove it


  # Add top lines(s) if necessary
  if (question_info$top %in% c("top","top2", "top3")) {
    top_results <- compute_top_n_results(question_design, question_results, ci_level = question_info$ci_level, top = "top1")
     }

  # Add top2 lines(s) if necessary
  if (question_info$top %in% c("top3","top2")) {
    top2_results <- compute_top_n_results(question_design, question_results, ci_level = question_info$ci_level, top = "top2")
    }

  # Add top3 lines(s) if necessary
  if (question_info$top %in% c("top3")) {
    top3_results <- compute_top_n_results(question_design, question_results, ci_level = question_info$ci_level, top = "top3")
  }


  # Append to results
  if (question_info$top =="top") {
    question_results <- question_results |> bind_rows(top_results)
    }
  if (question_info$top == "top2") {
      question_results <-question_results |> bind_rows(top_results) |> bind_rows(top2_results)
    }
  if (question_info$top =="top3") {
      question_results <-question_results |> bind_rows(top_results) |> bind_rows(top2_results) |> bind_rows(top3_results)
  }


  # Round results and put proportion to 100
  question_results <- question_results |>
    mutate(
      n = round(n), # Round all counts
      across(c(prop, ci_low, ci_high), ~ round(100 * ., 1)) # Round with 1 decimal proportions and CI levels, and put on base 100
    )




  # Remove subquestion variable
  question_results <-  question_results |>
    select(-subquestion)


  return(question_results)
}
