#' Used to find datasets in the catalog by searching using a dataset name search
#' pattern. Wildcards are automatically inserted at the start and end of the
#' dataset_name string.
#'
#' The dataset-search method is the fundamental endpoint in the USGS M2M API that
#' allows you to discover and search for available datasets within the USGS/EROS
#' data inventory.
#'
#' @param session An object of class "ers_session" returned by the `ers_login`
#'   function.
#' @param dataset_name Search pattern to use for the dataset name.
#' @param spatial_filter Optional spatialFilter dict.
#' @param temporal_filter Optional spatialFilter dict.
#'
#' @return A tibble containing the dataset name, description, and other metadata
#' @export
ers_dataset_search <- function(
    session,
    dataset_name,
    spatial_filter = NULL,
    temporal_filter = NULL) {
  datasearch_payload <- list(datasetName = dataset_name)

  if (!is.null(spatial_filter)) {
    datasearch_payload$spatialFilter <- spatial_filter
  }

  if (!is.null(temporal_filter)) {
    datasearch_payload$temporalFilter <- temporal_filter
  }

  datasearch_result <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("dataset-search") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = datasearch_payload) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  datasets <- m2m_response_data(datasearch_result, "Dataset search failed")

  if (is.null(datasets)) {
    return(NULL)
  }

  df <- datasets %>%
    jsonify::to_json() %>%
    jsonify::from_json() %>%
    dplyr::as_tibble()

  unnest_cols <- if ("spatialBounds" %in% names(df)) {
    rlang::expr(c(dplyr::where(is.list), -"spatialBounds"))
  } else {
    rlang::expr(dplyr::where(is.list))
  }
  df <- df |>
    tidyr::unnest(!!unnest_cols)

  # coerce spatialBounds to nested tibble
  if ("spatialBounds" %in% names(df)) {
    if (inherits(df$spatialBounds, "data.frame")) {
      df$spatialBounds <- apply(df$spatialBounds, 1, function(x) dplyr::as_tibble(t(x)))
    } else {
      df$spatialBounds <- lapply(df$spatialBounds, function(x) dplyr::as_tibble(x))
    }
  }

  # coerce temporalCoverage to nested tibble
  format_temporal <- function(x) {
    times <- x %>%
      stringr::str_remove_all("\\[|]|\"") %>%
      stringr::str_split(",")

    dplyr::tibble(start = times[[1]][1], end = times[[1]][2])
  }
  df$temporalCoverage <- lapply(df$temporalCoverage, format_temporal)

  # coerce catalogs
  df$catalogs <- apply(df$catalogs, 1, function(x) x)

  return(df)
}
