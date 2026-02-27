#' Calculate Catch Per Unit Effort (CPUE)
#'
#' Calculates CPUE from catch and effort data, with optional gear
#' standardization. Supports ratio and log-transformed methods.
#'
<<<<<<< HEAD
#' @param catch Input data: a numeric vector of catch values, or a data frame
#'   containing catch and effort columns.
#' @param ... Additional arguments passed to methods.
#'
#' @export

=======
#' @param catch Numeric vector of catch (e.g. kg) or a data frame of "catch" and "effort" columns
#' @param ... Additional arguments passed on to methods
#'
#' @export
>>>>>>> 63b5d8ce50c6743dfadbb9ea34d774b9d28dbda6
cpue <- function(catch, ...) {
  UseMethod("cpue")
}

<<<<<<< HEAD

#' @rdname cpue
#' @param effort Numeric vector of effort (e.g. hours)
#' @param gear_type Character. Gear type used for sampling. Must be one of the
#'   types in the internal `gear_types` table. Defaults to `"nordic_gillnet"`,
#'   the standard reference gear (factor = 1.0).
=======
#' @rdname cpue
#' @param effort Numeric vector of effort (e.g. hours)
#' @param gear_factor Numeric adjustment for gear standard (defaults is 1)
>>>>>>> 63b5d8ce50c6743dfadbb9ea34d774b9d28dbda6
#' @param method Character: one of "ratio" or "log"; will take "ratio" as default
#' @param verbose Logical indicating whether to print processing messages (default is FALSE, also accepts the value of 'fishr.verbose')
#'
#' @returns Numeric vector of CPUE values of the class 'my_cpue'
#' @export
#'
#' @examples
#' cpue(100, 10)
#' cpue(100, 10, gear_type = "nordic_gillnet")

cpue.numeric <- function(
  catch,
  effort,
  gear_type = "nordic_gillnet",
  method = c("ratio", "log"),
<<<<<<< HEAD
  verbose = getOption("fishr.verbose", default = FALSE)
=======
  verbose = getOption("fishr.verbose", default = FALSE), #verbose needs T or F and this parameter can be set at the package level
  ...
>>>>>>> 63b5d8ce50c6743dfadbb9ea34d774b9d28dbda6
) {
  if (!gear_type %in% gear_types$gear_type) {
    stop(
      "'gear_type' must be one of: ",
      paste(gear_types$gear_type, collapse = ", "),
      call. = FALSE
    )
  }

  gear_factor <- gear_types$gear_factor[gear_types$gear_type == gear_type]

  if (is.null(gear_factor)) {
    stop(
      "gear_factor is deprecated, use gear_type"
    )
  }

  method <- match.arg(method)

  validate_numeric_inputs(catch = catch, effort = effort)

  if (verbose) {
    message("Processing ", length(catch), " records")
  }

  raw_cpue <- switch(
    method,
    ratio = catch / effort,
    log = log(catch / effort)
  )

<<<<<<< HEAD
  #original result
  #raw_cpue * gear_factor

  #lesson 8 - OOP: Adding metadata with attributes
=======
>>>>>>> 63b5d8ce50c6743dfadbb9ea34d774b9d28dbda6
  #result <- raw_cpue * gear_factor
  #attr(result, "gear_factor") <- gear_factor
  #attr(result, "n_records") <- length(catch)
  #attr(result, "method") <- method
  #class(result) <- "cpue_result"
  #result

<<<<<<< HEAD
  #lesson 8 - OOP: Create a constructor function
  new_cpue_result(
    values = raw_cpue * gear_factor,
    method = method,
    gear_type = gear_type,
=======
  new_cpue_result(
    raw_cpue * gear_factor,

    method = method,
    gear_factor = gear_factor,
>>>>>>> 63b5d8ce50c6743dfadbb9ea34d774b9d28dbda6
    n_records = length(catch)
  )
}

#' @rdname cpue
#' @export
cpue.data.frame <- function(
  catch,
<<<<<<< HEAD
  gear_type = "nordic_gillnet",
=======
  gear_factor = 1,
>>>>>>> 63b5d8ce50c6743dfadbb9ea34d774b9d28dbda6
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
<<<<<<< HEAD
    catch = catch[["catch"]],
    effort = catch[["effort"]],
    gear_type = gear_type,
=======
    catch = catch[["catch"]], #or catch$catch
    effort = catch[["effort"]], # or catch$effort
    gear_factor = gear_factor,
>>>>>>> 63b5d8ce50c6743dfadbb9ea34d774b9d28dbda6
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

<<<<<<< HEAD
#' @export
print.cpue_result <- function(x, ...) {
  #lesson 8 - OOP: Writing a print method
  #cat("CPUE Results for", length(x), "records\n")
  #cat("Values:", round(x, 2), "\n")
  #lesson 8 - OOP: Create a constructor function
  cat("CPUE Result\n")
  cat("Records:     ", attr(x, "n_records"), "\n")
  cat("Method:      ", attr(x, "method"), "\n")
  cat("Gear type:   ", attr(x, "gear_type"), "\n")
  cat("Values:      ", round(x, 2), "\n")
  invisible(x)
}

#' @noRd
new_cpue_result <- function(values, method, gear_type, n_records) {
  structure(
    values, # The data
    method = method, # Attributes specifying metadata
    gear_type = gear_type,
=======

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
>>>>>>> 63b5d8ce50c6743dfadbb9ea34d774b9d28dbda6
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
<<<<<<< HEAD
  cat("Gear type:   ", attr(object, "gear_type"), "\n")
=======
  cat("Gear factor: ", attr(object, "gear_factor"), "\n")
>>>>>>> 63b5d8ce50c6743dfadbb9ea34d774b9d28dbda6
  cat("Mean CPUE:   ", round(mean(object), 2), "\n")
  cat("Median CPUE: ", round(stats::median(object), 2), "\n")
  cat("SD CPUE:     ", round(stats::sd(object), 2), "\n")
  invisible(object)
}
