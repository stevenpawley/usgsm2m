daterangeFilter <- function(start, end) {
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", start) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", end)) {
    stop("Start and end dates must be in 'YYYY-MM-DD' format")
  }
  list(start = start, end = end)
}

#' Create a cloud cover filter for use in scene search
#'
#' @param min Minimum cloud cover percentage (0-100)
#' @param max Maximum cloud cover percentage (0-100)
#' @return A named list with min and max cloud cover percentages
#' @export
filter_cloud <- function(min = 0, max = 100) {
  list(min = min, max = max)
}


filter_scene <- function(
    acquisitionFilter = NULL,
    cloudCoverFilter = NULL,
    ingestFilter = NULL,
    metadataFilter = NULL,
    seasonalFilter = NULL,
    spatialFilter = NULL
    ) {
  # Check that at least one filter is provided
  if (is.null(acquisitionFilter) && is.null(cloudCoverFilter) &&
      is.null(ingestFilter) && is.null(metadataFilter) &&
      is.null(seasonalFilter) && is.null(spatialFilter)) {
    stop("At least one filter must be provided")
  }

  # Create the sceneFilter list
  sceneFilter <- list(
    acquisitionFilter = acquisitionFilter,
    cloudCoverFilter = cloudCoverFilter,
    ingestFilter = ingestFilter,
    metadataFilter = metadataFilter,
    seasonalFilter = seasonalFilter,
    spatialFilter = spatialFilter
  )

  # drop NULL elements
  sceneFilter <- sceneFilter[!sapply(sceneFilter, is.null)]
}


#' Create a spatial filter for use in scene search
#'
#' @param ll_lon Lower left longitude
#' @param ll_lat Lower left latitude
#' @param ur_lon Upper right longitude
#' @param ur_lat Upper right latitude
#' @return A named list with spatial filter parameters
#' @export
filter_spatial <- function(ll_lon, ll_lat, ur_lon, ur_lat) {
  list(
    filterType = "mbr",
    lowerLeft = list(latitude = ll_lat, longitude = ll_lon),
    upperRight = list(latitude = ur_lat, longitude = ur_lon)
  )
}


#' Create a temporal filter for use in scene search
#'
#' @param start Start date in "YYYY-MM-DD" format
#' @param end End date in "YYYY-MM-DD" format
#' @return A named list with start and end dates
#' @export
filter_temporal <- function(start, end) {
  list(start = start, end = end)
}






#' Filter scene products and particularly the secondaryDownloads
#'
#' @param scene_products A tibble of scene products as returned by
#'   `ers_scene_products()`
#'
#' @returns A tibble with the secondaryDownloads unnested
#' @export
filter.scene_products <- function(scene_products) {
  # todo: create a function to filter scene products and particularly the
  # secondaryDownloads,
  unnested <- scene_products |>
    tidyr::unnest("secondaryDownloads", sep = "_")
}
