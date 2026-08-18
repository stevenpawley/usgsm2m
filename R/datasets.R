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

  coerce_dataset_df(df)
}


# Coerce a raw dataset-record data.frame (as produced by jsonify::from_json())
# into its final tibble shape: unnest list columns, and reshape
# spatialBounds/temporalCoverage/catalogs into nested tibbles/lists.
#
# Shared by ers_dataset_search() (multi-row) and ers_dataset() (single-row),
# which both return the same per-dataset record shape from the M2M API.
coerce_dataset_df <- function(df) {
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
  if ("temporalCoverage" %in% names(df)) {
    format_temporal <- function(x) {
      times <- x %>%
        stringr::str_remove_all("\\[|]|\"") %>%
        stringr::str_split(",")

      dplyr::tibble(start = times[[1]][1], end = times[[1]][2])
    }
    df$temporalCoverage <- lapply(df$temporalCoverage, format_temporal)
  }

  # coerce catalogs
  if ("catalogs" %in% names(df)) {
    df$catalogs <- apply(df$catalogs, 1, function(x) x)
  }

  return(df)
}


#' Look up a single dataset by ID or name
#'
#' Use this to resolve a dataset's ID/alias and full metadata, or as a
#' prerequisite lookup before calling `ers_dataset_filters()` or
#' `ers_scene_search()`.
#'
#' @param session An object of class "ers_session" returned by the `ers_login`
#'   function.
#' @param dataset_id The dataset identifier. Use this or `dataset_name`, not
#'   both.
#' @param dataset_name The system-friendly dataset name (alias), for example
#'   'landsat_ot_c2_l2'. Use this or `dataset_id`, not both.
#'
#' @return A single-row tibble containing the dataset name, description, and
#'   other metadata.
#' @export
ers_dataset <- function(session, dataset_id = NULL, dataset_name = NULL) {
  if (is.null(dataset_id) && is.null(dataset_name)) {
    stop("dataset_id or dataset_name must be supplied")
  }

  if (!is.null(dataset_id) && !is.null(dataset_name)) {
    stop("Supply only one of dataset_id or dataset_name, not both")
  }

  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("dataset") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(
      data = list(datasetId = dataset_id, datasetName = dataset_name)
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  dataset <- m2m_response_data(resp, "Dataset not found")

  if (is.null(dataset)) {
    return(NULL)
  }

  # Wrap in a list so jsonify serializes it as a one-element JSON array:
  # jsonify fills NA for null scalar fields (e.g. acquisitionEnd on an
  # active dataset) when parsing an array, but leaves them as R NULL -
  # which as_tibble() rejects - when parsing a lone JSON object.
  df <- list(dataset) %>%
    jsonify::to_json() %>%
    jsonify::from_json() %>%
    dplyr::as_tibble()

  coerce_dataset_df(df)
}


#' Retrieve the metadata filter fields available for a dataset
#'
#' These filter fields are used to build a `metadataFilter` for
#' `filter_scene()` / `ers_scene_search()`: each row's `id` is the
#' `filterId` that a metadata filter refers to.
#'
#' @param session An object of class "ers_session" returned by the `ers_login`
#'   function.
#' @param dataset_name The system-friendly dataset name (alias), for example
#'   'landsat_ot_c2_l2'.
#'
#' @return A tibble with one row per filter field, including `id`
#'   (the `filterId`), `legacyFieldId`, `fieldLabel`, `searchSql`, and
#'   `fieldConfig` (a list-column, since its shape varies by filter type).
#' @export
ers_dataset_filters <- function(session, dataset_name) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("dataset-filters") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(datasetName = dataset_name)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  filter_fields <- m2m_response_data(resp, "No filters found")

  if (is.null(filter_fields)) {
    return(NULL)
  }

  filter_fields <- lapply(filter_fields, function(field) {
    df <- field %>%
      jsonify::to_json() %>%
      jsonify::from_json()

    meta <- df[names(df) != "fieldConfig"]
    meta <- meta[!sapply(meta, is.null)]
    meta <- dplyr::as_tibble(meta)
    meta$fieldConfig <- list(df$fieldConfig)
    meta
  })

  purrr::list_rbind(filter_fields)
}
