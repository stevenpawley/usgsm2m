# Search the dataset catalog by name pattern (dataset-search endpoint).
# Wildcards are automatically inserted around dataset_name by the API.
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

  if (length(datasets) == 0) {
    return(tibble::tibble())
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


# Look up a single dataset by id or alias (dataset endpoint).
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

  # An unknown dataset comes back as HTTP 200 with a null payload and no
  # errorCode, so m2m_check_response() sees nothing wrong - turn that into a
  # real error rather than letting the coercion below fail obscurely.
  if (length(dataset) == 0) {
    rlang::abort(
      paste0(
        "Dataset not found: ",
        if (is.null(dataset_name)) dataset_id else dataset_name
      ),
      class = c("m2m_not_found", "m2m_error")
    )
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


# Retrieve the metadata filter fields for a dataset (dataset-filters endpoint).
# Each row's `id` is the filterId used when building a metadataFilter.
ers_dataset_filters <- function(session, dataset_name) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("dataset-filters") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(datasetName = dataset_name)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  filter_fields <- m2m_response_data(resp, "No filters found")

  if (length(filter_fields) == 0) {
    return(tibble::tibble())
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
