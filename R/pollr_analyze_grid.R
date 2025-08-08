
#' Analyze a Grid-Type Question (Likert Matrix)
#'
#' This function handles the analysis of grid-type questions (i.e., multiple sub-questions
#' sharing the same answer format). It applies `pollr_analyze()` to each sub-question,
#' aggregates the results, and prepares a unified output.
#'
#' @param survey_data A data.frame containing the survey responses.
#' @param question_info A list produced by `pollr_analyze_info()` containing metadata such as
#' question_varname (a character vector of sub-question names), weight_varname, cross_varname,
#' question_title, etc.
#'
#' @return A nanamed list structured like `pollr_analyze()`

pollr_analyze_grid <- function(survey_data, question_info) {

  # Apply pollr_analyze() individually to each sub-question (grid element)
  question_grid_all <- purrr::map(
    question_info$question_varname,
    ~ pollr_analyze(
      survey_data = survey_data,
      question_varname = .x,
      multiple_choice = question_info$multiple_choice,
      grid = FALSE,
      weight_varname = question_info$weight_varname,
      cross_varname = question_info$cross_varname,
      question_title = .x,
      question_text = question_info$question_text,
      sorted_results = question_info$sorted_results,
      top = question_info$top,
      ci_level = question_info$ci_level
    )
  )


  # Name the list elements using each question name
  questions_names <- question_grid_all |>
    map_chr(~ .x$info$question_varname)
  names(question_grid_all) <- questions_names

  # Combine data
  question_grid_data <- question_grid_all |>
    map("data") |>
    bind_rows(.id = "questions_names")

  # ReCreate design (using data)
  question_grid_design <- question_grid_data |>
    pollr_analyze_design()


  # Combine results
  question_grid_results <- question_grid_all |>
    map("results") |>
    bind_rows(.id = "questions_names")



  # Create new "combined" tab
  question_grid_tab <- pollr_analyze_tab_grid_categorical(question_grid_all, question_grid_results, question_info)


  # Create new "combined" plot
 question_grid_plot <-  pollr_analyze_plot_grid_categorical(question_grid_results, question_info)

 question_all <-  list(
    data = question_grid_data,
    info = question_info,
    design = question_grid_design,
    results = question_grid_results,
    tab = question_grid_tab,
    plot = question_grid_plot
  )

  return( question_all )

}




