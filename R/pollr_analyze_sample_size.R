#' Compute the sample sizes used for the question, for total and cross targets
#'
#' @param question_data A tibble with question data, including response and weights variables
#' @param question_info A list with all informations about the question
#' @return A tibble with sample sizes data, total and cross targets if any

#' @noRd
#'
pollr_analyze_sample_size <- function(question_data, question_info) {

  # Total sample size
  question_sample_size <- question_data |>
    summarize(
      n = n_distinct(id)
    ) |>
    bind_cols(tibble(target = "Total"))


  # Adding cross samples sizes, if cross variables

  if (!is.null(question_info$cross_varname)) {
    question_sample_size_target <- question_data |>
    group_by(cross) |>
    summarize(n = n_distinct(id)) |>
    rename(target = cross)

    question_sample_size <-  question_sample_size |>
      bind_rows(question_sample_size_target)

  }

  return(question_sample_size)

}





