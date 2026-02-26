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

  #result <- raw_cpue * gear_factor
  #attr(result, "gear_factor") <- gear_factor
  #attr(result, "n_records") <- length(catch)
  #attr(result, "method") <- method
  #class(result) <- "cpue_result"
  #result

  new_cpue_result(
    raw_cpue * gear_factor,

    method = method,
    gear_factor = gear_factor,
    n_records = length(catch)
  )
}

#' @export
#create a class specific print function
print.cpue_result <- function(x, ...) {
  cat("CPUE Result\n")
  cat("Records:     ", attr(x, "n_records"), "\n")
  cat("Method:      ", attr(x, "method"), "\n")
  cat("Gear factor: ", attr(x, "gear_factor"), "\n")
  cat("Values:      ", round(x, 2), "\n")
  invisible(x) #you want the function to return the unaltered value of the inputs. This allows you to use x later on after the function
}

#' @noRd
new_cpue_result <- function(values, method, gear_factor, n_records) {
  structure(
    values, # The data
    method = method, # Attributes specifying metadata
    gear_factor = gear_factor,
    n_records = n_records,
    class = "cpue_result" # class is a special attribute
  )
}

#' @export
summary.cpue_result <- function(object, ...) {
  cat("Survey Result Summary\n")
  cat("---------------------\n")
  cat("Method:      ", attr(object, "method"), "\n")
  cat("Records:     ", attr(object, "n_records"), "\n")
  cat("Gear factor: ", attr(object, "gear_factor"), "\n")
  cat("Mean CPUE:   ", round(mean(object), 2), "\n")
  cat("Median CPUE: ", round(stats::median(object), 2), "\n")
  cat("SD CPUE:     ", round(stats::sd(object), 2), "\n")
  invisible(object)
}
