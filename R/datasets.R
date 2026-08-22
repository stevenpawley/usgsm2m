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
  # jsonify leaves a null field as a zero-length list element and a
  # multi-valued field (e.g. catalogs) as a longer one. Unwrap only the
  # length-1 cases into plain columns and fill nulls with NA: unnesting a
  # multi-valued field would repeat its dataset across several rows, and
  # unnesting a null one would drop that dataset's row entirely.
  list_cols <- names(df)[vapply(
    df,
    function(col) is.list(col) && !is.data.frame(col),
    logical(1)
  )]

  for (nm in setdiff(list_cols, "spatialBounds")) {
    col <- df[[nm]]
    lens <- lengths(col)

    if (all(lens <= 1)) {
      col[lens == 0] <- NA
      df[[nm]] <- unlist(col, use.names = FALSE)
    }
  }

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

  # catalogs is genuinely multi-valued for some datasets, so it stays a
  # list-column; normalize the data.frame form jsonify sometimes returns.
  if ("catalogs" %in% names(df) && is.data.frame(df$catalogs)) {
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

  # A neutral context: this covers authorisation failures too, where asserting
  # "not found" would contradict the API's own explanation.
  dataset <- m2m_response_data(resp, "Dataset lookup failed")

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

  coerce_filter_fields(m2m_response_data(resp, "No filters found"))
}


# Turn the dataset-filters payload into one row per filter field.
#
# Nested members (fieldConfig, and valueList for Select fields) become
# list-columns: letting as_tibble() see them as plain vectors would recycle
# each field into as many rows as it has options.
coerce_filter_fields <- function(filter_fields) {
  if (length(filter_fields) == 0) {
    return(tibble::tibble())
  }

  filter_fields <- lapply(filter_fields, function(field) {
    field <- field[!vapply(field, is.null, logical(1))]

    cells <- lapply(field, function(x) {
      if (length(x) == 1 && !is.list(x)) x else list(x)
    })

    tibble::as_tibble(cells)
  })

  purrr::list_rbind(filter_fields)
}
