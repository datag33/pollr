#' Generate a HTML table for a question
#'
#' @param question_results tibble with question results
#' @param question_info A list with all informations about the question
#'
#' @return A kableExtra HTML table object
#' @importFrom kableExtra kable kbl kable_minimal kable_styling column_spec
#' @importFrom glue glue
#' @noRd
#'
pollr_analyze_tab <- function(question_results, question_info) {

    question_tab <- question_results |>
    # Add ici interval in a single column instead of two low / high columns
    mutate(
      ci_interval = glue::glue("[{ci_low} - {ci_high}]")
    ) |>
    select(-ci_high, -ci_low)


    # If cross variable, put into several columns
    if (!is.null(question_info$cross_varname)) {

      nb_cross <- unique(question_tab$cross) |> length()
      question_tab <- question_tab |>
        pivot_wider(names_from = cross, values_from = c(n, prop, ci_interval), names_vary = "slowest")

    } else {
      nb_cross <-  1
      question_tab <- question_tab |> ungroup() |> select(-cross) # Remove cross variable if not used
    }

    # Define the column numbers for n, prop and ci_interval
    col_number_n <- seq(2, ncol(question_tab), by = 3)
    col_number_percent <- seq(3, ncol(question_tab), by = 3)
    col_number_ci <- seq(4, ncol(question_tab), by = 3)

    # Define the column names
    col_names <- c("Response", rep(c("n", "%", "CI"), nb_cross))


    # Add a row for total
    total_row <- question_tab |>
      summarise(across(
        everything(),
        ~if (is.numeric(.x)) sum(.x, na.rm = TRUE) else "-")
        ) |>
      mutate(response = "Total")

    question_tab <- question_tab  |>
      add_row(total_row)


    # Create the HTML table
    question_tab <- question_tab |>
    kbl(
      col.names = col_names,
      format = "html",
      align = "c"
      #caption = question_info$question_text
      ) |>
    kable_minimal(full_width = F, position = "left") |>
    column_spec(col_number_n , italic = TRUE, extra_css = "font-size: 90%;") |>  #  Format n column(s)
    column_spec(col_number_percent , bold = TRUE) |>    # Format percent column(s)
    column_spec(col_number_ci,color = "gray40", extra_css = "font-size: 80%;") |>  # colonne IC en plus petit
    row_spec(nrow(question_tab), color = "white", background = "black")   # Formatage total

    # Adding cross variables labels, if necessary

    if (!is.null(question_info$cross_varname)) {

        header_group <- c(1, rep(3, nb_cross))
        names(header_group) <- c(" ", unique(question_results$cross))

        question_tab <- question_tab  |>
          add_header_above(header_group)
    }


    return(question_tab)
}
