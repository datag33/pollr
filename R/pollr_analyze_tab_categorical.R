#' Generate a HTML table for a single categorical question
#'
#' @param question_results tibble with question results
#' @param question_info A list with all informations about the question
#'
#' @return A kableExtra HTML table object
#' @importFrom kableExtra kable kbl kable_minimal kable_styling column_spec row_spec add_header_above
#' @noRd
#'
pollr_analyze_tab_categorical <- function(question_results, question_info) {


  question_tab_raw <- question_results |>
    # Add ici interval in a single column instead of two low / high columns
    mutate(
      ci_interval = glue::glue("[{ci_low} - {ci_high}]")
    ) |>
    select(-ci_high, -ci_low)


  # If cross variable, put into several columns
  if (!is.null(question_info$cross_varname)) {

    nb_cross <- unique(question_tab_raw$cross) |> length()
    question_tab_raw  <- question_tab_raw  |>
      pivot_wider(names_from = cross, values_from = c(n, prop, ci_interval), names_vary = "slowest") |>
      ungroup()

  } else {
    nb_cross <-  1
    question_tab_raw  <- question_tab_raw  |> ungroup() |> select(-cross) # Remove cross variable if not used
  }




  # Define the column numbers for n, prop and ci_interval
  col_number_n <- seq(2, ncol(question_tab_raw ), by = 3)
  col_number_percent <- seq(3, ncol(question_tab_raw ), by = 3)
  col_number_ci <- seq(4, ncol(question_tab_raw ), by = 3)

  # Define the column names
  col_names <- c("Response", rep(c("n", "%", "CI"), nb_cross))


  # Add a row for total
  total_row <- question_tab_raw  |>
    filter(!response %in% c("Top1", "Top2", "Top3")) |>
    summarise(across(
      everything(),
      ~if (is.numeric(.x)) sum(.x, na.rm = TRUE) else "-")
    ) |>
    mutate(response = "Total")

  question_tab_raw  <- question_tab_raw   |>
    add_row(total_row)


  # Create the HTML table
  question_tab <- question_tab_raw |>
    kbl(
      col.names = col_names,
      format = "html",
      align = "c",
      caption = question_info$question_text
    ) |>
    kable_minimal(full_width = F, position = "left") |>
    column_spec(col_number_n , italic = TRUE, extra_css = "font-size: 90%;") |>  #  Format n column(s)
    column_spec(col_number_percent , bold = TRUE) |>    # Format percent column(s)
    column_spec(col_number_ci,color = "gray40", extra_css = "font-size: 80%;") |>  # colonne IC en plus petit
    row_spec(nrow(question_tab_raw), color = "white", background = "black")   # Formatage total

  # Formating top rows, if necessary
  if (question_info$top == "top")  row_number_top <- nrow(question_tab_raw) - 1
  if (question_info$top == "top2")  row_number_top <- c(nrow(question_tab_raw) - 2, nrow(question_tab_raw) - 1)
  if (question_info$top == "top3")  row_number_top <- c(nrow(question_tab_raw) - 3,  nrow(question_tab_raw)  - 2,   nrow(question_tab_raw)  - 1)

  if (question_info$top %in% c("top", "top2", "top3")) {
      question_tab <- question_tab |> row_spec(row_number_top, color = "white", background = "seagreen")
  }



  # Adding cross variables labels, if necessary

  if (!is.null(question_info$cross_varname)) {

    header_group <- c(1, rep(3, nb_cross))
    names(header_group) <- c(" ", unique(question_results$cross))

    question_tab <- question_tab  |>
      add_header_above(header_group)
  }


  return(question_tab)
}
