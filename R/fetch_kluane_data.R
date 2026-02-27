#' Fetch Kluane Lake Trout survey data
#'
#' Retrieves gill net survey records from the Government of Canada open data
#' portal via the CKAN datastore API. Returns individual fish records from
#' standardised gill net surveys at lakes in Kluane National Park, Yukon.
#'
#' @param limit Integer. Maximum number of records to fetch (default 2000).
#'
#' @return A data frame with columns `lake`, `year`, `set_number`, `species`,
#'   `fork_length_mm`, and `weight_g`.
#' @export
#'
#' @examples
#' \dontrun{
#' kluane <- fetch_kluane_data()
#' head(kluane)
#' }
#' # \dontrun{} prevents this example from running during R CMD check
#' # since it requires a network connection.
fetch_kluane_data <- function(limit = 2000) {
  req <- httr2::request(
    "https://open.canada.ca/data/en/api/3/action/datastore_search"
  )
  req <- httr2::req_url_query(
    req,
    resource_id = "af1e5730-34bd-4314-831c-1d940d99e1a7",
    limit = limit
  )
  resp <- httr2::req_perform(req)

  # simplifyVector = TRUE converts the JSON array of objects to a data frame
  # and coerces JSON null values to NA automatically.
  records <- httr2::resp_body_json(resp, simplifyVector = TRUE)$result$records

  # The first record is a bilingual EN/FR column header row, not data.
  records <- records[-1, ]

  data.frame(
    lake = records$Lake,
    year = as.integer(records$Year),
    set_number = as.integer(records[["Set Number"]]),
    species = records$Species,
    fork_length_mm = as.numeric(records[["Fork Length (millimetres)"]]),
    weight_g = as.numeric(records[["Weight (grams)"]]),
    stringsAsFactors = FALSE
  )
}
