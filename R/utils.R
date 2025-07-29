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

