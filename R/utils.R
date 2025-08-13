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



#' Interpret a p-value in terms of standard significance levels
#'
#' This internal function converts a numeric p-value into a human-readable
#' description of its statistical significance level (e.g., "Significant at 95%").
#' It uses common thresholds: 0.01, 0.05, and 0.10.
#'
#' @param p A numeric value between 0 and 1 representing a p-value.
#'
#' @return A character string describing the significance level.
#' @noRd


interpret_pvalue <- function(p) {
  if (is.na(p)) {
    return("p-value not available")
  } else if (p < 0.01) {
    return("Significant at 99% level")
  } else if (p < 0.05) {
    return("Significant at 95% level")
  } else if (p < 0.10) {
    return("Significant at 90% level")
  } else {
    return("Not statistically significant")
  }
}





#' pollr default theme for charts
#'
#' A clean and legible ggplot2 theme, optimized for survey results
#' and compatible with ggiraph outputs.
#'
#' @param base_size Numeric. Base font size.
#' @param base_family Character. Base font family.
#' @param accent_color Character. Hex or color name for accent elements.
#'
#' @return A ggplot2 theme object.
#' @noRd

theme_pollr <- function(base_size = 12,
                        base_family = "sans",
                        accent_color = "royalblue") {

 theme_minimal(base_size = base_size, base_family = base_family) +
    theme(

      # Plot title & subtitle
      plot.title = element_text(
        face = "bold",
        size = base_size + 6,
        color = accent_color,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = base_size,
        face = "italic",
        color = "grey40",
        hjust = 0.5
      ),

      # Axes
      axis.title = element_blank(),
      axis.text.x = element_text(color = "grey20", size = base_size - 4),
      axis.text.y = element_text(color = "grey20", size = base_size - 2),
      axis.line.x = element_line(color = "grey50", linewidth = 0.5),

      # Grid
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),

      # Caption
      plot.caption = element_text(face = "italic", size = base_size - 2),

      # Facet title
      strip.text.x = element_text(size = base_size, color="darkviolet", face="bold.italic"),

      # Legend
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = base_size - 1),
      legend.position = "bottom",

      # Margins for better HTML export
      plot.margin = margin(10, 10, 10, 10)
    )
}

