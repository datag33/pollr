#' Generate a HTML table for a numerical question
#'
#' @param question_results tibble with question results
#' @param question_sample_size A tibble with sample sizes data, total and cross targets if any
#' @param question_test A tibble with test statistic, degrees of freedom, pvalue and significancy level
#' @param question_info A list with all informations about the question
#'
#' @return A kableExtra HTML table object
#' @importFrom kableExtra kable kbl kable_minimal kable_styling column_spec row_spec add_header_above  footnote
#' @noRd
#'
pollr_analyze_tab_numerical<- function(question_results, question_sample_size, question_test, question_info) {




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
      pivot_wider(names_from = cross, values_from = c(n, mean, median, sd, ci_interval), names_vary = "slowest")

  } else {
    nb_cross <-  1
    question_tab <- question_tab |> ungroup() |> select(-cross) # Remove cross variable if not used
  }

  # Define the column numbers for n, mean, median, sd and ci
  col_number_n <- seq(1, ncol(question_tab), by = 5)
  col_number_mean <- seq(2, ncol(question_tab), by = 5)
  col_number_median <- seq(3, ncol(question_tab), by = 5)
  col_number_sd <- seq(4, ncol(question_tab), by = 5)
  col_number_ci <- seq(5, ncol(question_tab), by = 5)

  # Define the column names
  col_names <- c(rep(c("n", "mean", "median", "sd","CI"), nb_cross))


  # Create the HTML table
  question_tab <- question_tab |>
    kbl(
      col.names = col_names,
      format = "html",
      align = "c",
      caption = question_info$question_text
    ) |>
    kable_minimal(full_width = F, position = "left") |>
    column_spec(col_number_n , italic = TRUE, extra_css = "font-size: 90%;") |>  #  Format n column(s)
    column_spec(col_number_mean,  bold = TRUE) |>    # Format mean column(s)
    column_spec(col_number_median) |>    # Format median column(s)
    column_spec(col_number_sd, italic = TRUE, extra_css = "font-size: 80%;") |>  # Format sd column(s)
    column_spec(col_number_ci,color = "gray40", extra_css = "font-size: 80%;")   # Format ci column


  # Adding cross variables labels, if necessary

  if (!is.null(question_info$cross_varname)) {

    header_group <- c(rep(5, nb_cross))
    names(header_group) <- c(unique(question_results$cross))

    question_tab <- question_tab  |>
    add_header_above(header_group)

    # Adding global test results
    question_tab <- question_tab |>
      footnote(glue("{question_test$significancy} (p = {question_test$p_value})"))


    # Adding question title, if any

    if (question_info$question_title != "") {

      question_title <- c(length(col_names))
      if (question_info$grid) { question_info$question_title <- paste0(question_info$question_title, " (", question_info$top, ")")} # Add top label on title if summary tab
      names(question_title) <- question_info$question_title
      question_tab <- question_tab  |>
        add_header_above(question_title, color = "royalblue")
    }

  }

  return(question_tab)
}
