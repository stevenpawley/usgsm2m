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


#' Create a cloud cover filter for use in scene search
#'
#' @param min Minimum cloud cover percentage (0-100)
#' @param max Maximum cloud cover percentage (0-100)
#' @return A named list with min and max cloud cover percentages
#' @export
filter_cloud <- function(min = 0, max = 100) {
  list(min = min, max = max)
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
