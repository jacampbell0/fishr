#' Calculate Catch Per Unit Effort (CPUE)
#'
#' Calculates CPUE from catch and effort data, with optional gear standardization.
#'
#' @param catch Numeric vector of catch (e.g. kg) or a data frame of "catch" and "effort" columns
#' @param ... Additional arguments passed on to methods
#'
#' @export
cpue <- function(catch, ...) {
  UseMethod("cpue")
}

#' @rdname cpue
#' @param effort Numeric vector of effort (e.g. hours)
#' @param gear_factor Numeric adjustment for gear standard (defaults is 1)
#' @param method Character: one of "ratio" or "log"; will take "ratio" as default
#' @param verbose Logical indicating whether to print processing messages (default is FALSE, also accepts the value of 'fishr.verbose')
#'
#' @returns Numeric vector of CPUE values of the class 'my_cpue'
#' @export
#'
#' @examples
#' cpue(100, 10)
#' cpue(100, 10, gear_factor = 0.5)

cpue.numeric <- function(
  catch,
  effort,
  gear_factor = 1,
  method = c("ratio", "log"),
  verbose = getOption("fishr.verbose", default = FALSE), #verbose needs T or F and this parameter can be set at the package level
  ...
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

#' @rdname cpue
#' @export
cpue.data.frame <- function(
  catch,
  gear_factor = 1,
  method = c("ratio", "log"),
  verbose = getOption("fishr.verbose", FALSE),
  ...
) {
  if (!"catch" %in% names(catch)) {
    stop("Column 'catch' not found in data frame.", call. = FALSE)
  }
  if (!"effort" %in% names(catch)) {
    stop("Column 'effort' not found in data frame.", call. = FALSE)
  }

  # We can then call the numeric method by extracting the relevant columns and passing them to cpue() again.
  # This way we reuse the existing logic and maintain a single source of truth for the CPUE calculation.
  cpue(
    catch = catch[["catch"]], #or catch$catch
    effort = catch[["effort"]], # or catch$effort
    gear_factor = gear_factor,
    method = method,
    verbose = verbose,
    ...
  )
}

#' @rdname cpue
#' @export
cpue.default <- function(catch, ...) {
  stop("Unsupported input type for cpue(): ", class(catch), call. = FALSE)
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
