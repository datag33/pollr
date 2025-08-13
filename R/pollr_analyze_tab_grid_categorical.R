#' Generates an HTML tab for a grid question with categorical data , using survey results
#'
#' @param question_grid_all a list with all grid elements
#' @param question_grid_results tibble with question results for all grid elements
#' @param question_grid_sample_size tibble with sample size for all grid elements
#' @param question_grid_test tibble with tests for all grid elements
#' @param question_info A list with all informations about the question
#'
#' @return A kableExtra HTML table object
#' @noRd
#'
#'
pollr_analyze_tab_grid_categorical <- function(question_grid_all, question_grid_results, question_grid_sample_size, question_grid_test, question_info) {



# Summary tab for all subquestions (only if no top required)

if (question_info$top != "none") {
 question_grid_tab_summary <- question_grid_results |>
   filter(toupper(response) == toupper(question_info$top)) |> # Summary on chosen top
   mutate(response = questions_names) |>
   select(-questions_names) |>
   pollr_analyze_tab(question_grid_sample_size, question_grid_test, question_info)
}

 # Individual tabs for all subquestions
 question_grid_tab <- question_grid_all |>
   map("tab")

 # Returns list with summary and then individual tabs
 question_grid_tab_all <- c(
   list(overview = question_grid_tab_summary),
   question_grid_tab
 )

 return(question_grid_tab_all)

}
