#' Validate that inputs are numeric
#'
#' Checks each named argument and stops with an informative error
#' if any are not numeric.
#'
#' @param ... Named numeric inputs to validate.
#'
#' @return Invisible `NULL`. Called for its side effect of
#'   stopping with an error if validation fails.
#' noRd means manual will be created for this, we don't want users to be able to use this function, its just for internal package use
#' also no export tag
#' @noRd
#'

validate_numeric_inputs <- function(...) {
  args <- list(...)
  arg_names <- names(args)

  for (i in seq_along(args)) {
    if (!is.numeric(args[[i]])) {
      stop(
        "'",
        arg_names[i],
        "' must be numeric, got ",
        class(args[[i]])[1],
        ".",
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}

standardize_effort <- function(catch, effort_hours) {
  validate_numeric_inputs(catch = catch, effort_hours = effort_hours)

  effort_days <- effort_hours * 24
  print(paste0("effort_days is ", effort_days))

  cpue(catch = catch, effort = effort_days)
}
