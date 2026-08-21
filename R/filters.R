#' Create a spatial filter for use in a scene search
#'
#' @param ll_lon Lower left longitude
#' @param ll_lat Lower left latitude
#' @param ur_lon Upper right longitude
#' @param ur_lat Upper right latitude
#' @return A named list with spatial filter parameters
#' @export
#' @examples
#' filter_spatial(ll_lon = -120, ll_lat = 40, ur_lon = -119, ur_lat = 41)
filter_spatial <- function(ll_lon, ll_lat, ur_lon, ur_lat) {
  list(
    filterType = "mbr",
    lowerLeft = list(latitude = ll_lat, longitude = ll_lon),
    upperRight = list(latitude = ur_lat, longitude = ur_lon)
  )
}


#' Create a temporal filter for use in a scene search
#'
#' @param start Start date in "YYYY-MM-DD" format
#' @param end End date in "YYYY-MM-DD" format
#' @return A named list with start and end dates
#' @export
#' @examples
#' filter_temporal("2020-07-01", "2020-07-31")
filter_temporal <- function(start, end) {
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", start) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", end)) {
    stop("Start and end dates must be in 'YYYY-MM-DD' format")
  }
  list(start = start, end = end)
}


#' Create a cloud cover filter for use in a scene search
#'
#' @param min Minimum cloud cover percentage (0-100)
#' @param max Maximum cloud cover percentage (0-100)
#' @return A named list with min and max cloud cover percentages
#' @export
#' @examples
#' filter_cloud(min = 0, max = 30)
filter_cloud <- function(min = 0, max = 100) {
  if (min < 0) {
    stop("Minimum cloud cover cannot be negative")
  }
  if (max > 100) {
    stop("Maximum cloud cover cannot exceed 100")
  }
  if (min > max) {
    stop("Minimum cannot be greater than maximum")
  }
  list(min = min, max = max)
}


#' Create a metadata filter matching a single value
#'
#' Metadata filters test dataset-specific fields. Get the available
#' `filter_id` values, and what they mean, from `M2MDataset$filters()`.
#'
#' @param filter_id The field's filter id, from the `id` column of
#'   `M2MDataset$filters()`.
#' @param value The value to match.
#' @param operand One of "=" or "like".
#' @return A named list describing the filter.
#' @export
#' @examples
#' filter_metadata_value("5e83d14fb9436d88", "045")
filter_metadata_value <- function(filter_id, value, operand = c("=", "like")) {
  operand <- match.arg(operand)
  list(
    filterType = "value",
    filterId = filter_id,
    value = value,
    operand = operand
  )
}


#' Create a metadata filter matching a range of values
#'
#' @param filter_id The field's filter id, from the `id` column of
#'   `M2MDataset$filters()`.
#' @param first The lower bound.
#' @param second The upper bound.
#' @return A named list describing the filter.
#' @export
#' @examples
#' filter_metadata_between("5e83d14fb9436d88", 40, 45)
filter_metadata_between <- function(filter_id, first, second) {
  list(
    filterType = "between",
    filterId = filter_id,
    firstValue = first,
    secondValue = second
  )
}


#' Combine metadata filters with a logical operator
#'
#' @param ... Metadata filters to combine, as built by
#'   [filter_metadata_value()] or [filter_metadata_between()].
#' @return A named list describing the combined filter.
#' @export
#' @examples
#' filter_metadata_and(
#'   filter_metadata_value("5e83d14fb9436d88", "045"),
#'   filter_metadata_value("5e83d14ff1eda1b8", "027")
#' )
filter_metadata_and <- function(...) {
  list(filterType = "and", childFilters = list(...))
}


#' @rdname filter_metadata_and
#' @export
filter_metadata_or <- function(...) {
  list(filterType = "or", childFilters = list(...))
}
