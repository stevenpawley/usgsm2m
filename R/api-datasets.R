# Internal M2M dataset endpoints. Functions in this file construct payloads,
# perform requests through m2m_request(), and delegate response shaping to
# coerce-datasets.R.

# Search the dataset catalog by name pattern (dataset-search endpoint).
# Wildcards are automatically inserted around dataset_name by the API.
api_dataset_search <- function(
    session,
    dataset_name,
    spatial_filter = NULL,
    temporal_filter = NULL) {
  payload <- list(datasetName = dataset_name)

  if (!is.null(spatial_filter)) {
    payload$spatialFilter <- spatial_filter
  }

  if (!is.null(temporal_filter)) {
    payload$temporalFilter <- temporal_filter
  }

  resp <- m2m_request(session, "dataset-search", payload)

  datasets <- m2m_response_data(resp, "Dataset search failed")

  coerce_dataset_records(datasets)
}


# Look up a single dataset by id or alias (dataset endpoint).
api_dataset <- function(session, dataset_id = NULL, dataset_name = NULL) {
  if (is.null(dataset_id) && is.null(dataset_name)) {
    stop("dataset_id or dataset_name must be supplied")
  }

  if (!is.null(dataset_id) && !is.null(dataset_name)) {
    stop("Supply only one of dataset_id or dataset_name, not both")
  }

  resp <- m2m_request(
    session,
    "dataset",
    list(datasetId = dataset_id, datasetName = dataset_name)
  )

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

  coerce_dataset_records(list(dataset))
}


# Retrieve the metadata filter fields for a dataset (dataset-filters endpoint).
# Each row's `id` is the filterId used when building a metadataFilter.
api_dataset_filters <- function(session, dataset_name) {
  resp <- m2m_request(
    session,
    "dataset-filters",
    list(datasetName = dataset_name)
  )

  coerce_filter_fields(m2m_response_data(resp, "No filters found"))
}
