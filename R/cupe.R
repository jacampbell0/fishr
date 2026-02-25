#' Calculate Catch Per Unit Effort (CPUE)
#'
#' Calculates CPUE from catch and effort data, with optional gear standardization.
#'
#' @param catch Numeric vector of catch (e.g. kg)
#' @param effort Numeric vector of effort (e.g. hours)
#' @param gear_factor Numeric adjustment for gear standard (defaults is 1)
#' @param verbose Logical indicating whether to print processing messages (default is FALSE, also accepts the value of 'fishr.verbose')
#' @param method Character: one of "ratio" or "log"; will take "ratio" as default
#'
#' @returns Numeric vector of CPUE values
#' @export
#'
#' @examples
#' cpue(100, 10)
#' cpue(100, 10, gear_factor = 0.5)

cpue <- function(
  catch,
  effort,
  gear_factor = 1,
  method = c("ratio", "log"),
  verbose = getOption("fishr.verbose", default = FALSE) #verbose needs T or F and this parameter can be set at the package level
) {
  method <- match.arg(method)

  validate_numeric_inputs(catch = catch, effort = effort)

  if (verbose) {
    message("Processing ", length(catch), " records")
  }

  raw_cpue <- switch(
    #switch() function selects which expression to evaluate based on the value of method. It’s a clean alternative to a chain of if/else if statements when dispatching on a single value
    method,
    ratio = catch / effort,
    log = log(catch / effort)
  )

  raw_cpue * gear_factor
}

#only need a return stmt if you are returning a value early
