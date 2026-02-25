#' Calculate Biomass Index
#'
#' Calculates biomass index from CPUE and area swept. Can optionally
#' compute CPUE from catch and effort data.
#'
#' @param cpue Numeric vector of CPUE values.
#' @param area_swept Numeric vector of area swept e.g. km²
#' @inheritParams cpue
#' @inheritDotParams cpue -catch -effort
#'
#' @details
#' Two modes of use:
#'
#' - Provide 'cpue' directly
#' - Provide 'catch' and 'effort' separately
#'
#' @seealso See [cpue()] for more details.
#'
#' @return A numeric vector of biomass index values
#' @export
#'
#' @examples
#' # From pre-computed CPUE
#' biomass_index(cpue = 10, area_swept = 5)
#'
#' # Compute CPUE on the fly
#' biomass_index(area_swept = 5, catch = 100, effort = 10)
#'
#' # Pass method through to cpue()
#' biomass_index(
#'   area_swept = 5,
#'   catch = c(100, 200),
#'   effort = c(10, 20),
#'   method = "log"
#' )
biomass_index <- function(
  cpue = NULL, #we want to make sure cpue isn't calculated here
  area_swept,
  verbose = getOption("fishr.verbose", default = FALSE),
  catch = NULL, #by passing catch and effort we can point biomass to cpue
  effort = NULL,
  ...
) {
  rlang::check_dots_used()

  if (is.null(cpue) && (!is.null(catch) && !is.null(effort))) {
    cpue <- cpue(catch, effort, ...)
  }

  if (is.null(cpue)) {
    stop("Must provide either 'cpue' or both 'catch' and 'effort'.")
  }
  if (verbose) {
    message("Calculating biomass for ", length(cpue), " records")
  }
  cpue * area_swept
}
