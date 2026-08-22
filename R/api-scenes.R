# Internal M2M scene and scene-list endpoints. Response normalization lives in
# coerce-scenes.R; shared pagination stays here because it controls requests.

# Create a server-side scene list (scene-list-add endpoint).
api_scene_list_add <- function(session, dataset_name, scenes, list_id) {
  # Preserve a JSON array for a one-scene request.
  payload <- list(
    listId = list_id,
    idField = "entityId",
    entityIds = as.list(scenes$entityId),
    datasetName = dataset_name
  )

  resp <- m2m_request(session, "scene-list-add", payload)

  m2m_response_data(resp, "Scene list not created")
}


# Retrieve the scenes in a previously created scene list (scene-list-get).
api_scene_list_get <- function(session, list_id) {
  resp <- m2m_request(session, "scene-list-get", list(listId = list_id))

  entity_ids <- m2m_response_data(resp, "Scene list not found")

  coerce_records(entity_ids)
}


# Delete a scene list from the ERS service (scene-list-remove endpoint).
api_scene_list_remove <- function(session, list_id) {
  resp <- m2m_request(session, "scene-list-remove", list(listId = list_id))

  m2m_check_response(resp, "Scene list not found")

  invisible(NULL)
}


# Summarize a scene list's contents (scene-list-summary endpoint). Returns a
# per-dataset tibble, with the overall spatial bounds attached as the
# "spatialBounds" attribute.
api_scene_list_summary <- function(session, list_id) {
  resp <- m2m_request(session, "scene-list-summary", list(listId = list_id))

  coerce_scene_list_summary(m2m_response_data(resp, "Scene list not found"))
}


# Discover downloadable products for a scene list (download-options endpoint).
# `secondaryDownloads` (the individual bands) is kept as a list-column.
api_scene_products <- function(session, dataset_name, list_id, band_group = TRUE) {
  payload <- list(
    listId = list_id,
    datasetName = dataset_name
  )

  if (band_group) {
    payload$includeSecondaryFileGroups <- TRUE
  }

  resp <- m2m_request(session, "download-options", payload)

  coerce_products(m2m_response_data(resp, "No products found"), band_group)
}
# Fetch metadata for every scene in a scene list (scene-metadata-list).
api_scene_metadata_list <- function(session, list_id) {
  resp <- m2m_request(session, "scene-metadata-list", list(listId = list_id))

  records <- m2m_response_data(resp, "Scene metadata not found")

  coerce_scene_metadata_list(records)
}


# Fetch the pages for either scene-search endpoint.
m2m_paginate_scene_search <- function(
    session,
    endpoint,
    data,
    on_fail_message,
    max_results = NULL,
    call = rlang::caller_env()) {
  page_size <- 10000L
  starting_number <- 1L
  all_results <- list()
  total_hits <- Inf
  last_page <- NULL

  while (length(all_results) < total_hits) {
    if (!is.null(max_results) && length(all_results) >= max_results) {
      break
    }

    requested <- if (is.null(max_results)) {
      page_size
    } else {
      min(page_size, max_results - length(all_results))
    }

    resp <- m2m_request(
      session,
      endpoint,
      c(
        data,
        list(maxResults = requested, startingNumber = starting_number)
      )
    )
    page <- m2m_response_data(resp, on_fail_message, call = call)

    last_page <- page
    total_hits <- page$totalHits
    all_results <- c(all_results, page$results)

    if (length(page$results) == 0 || is.null(page$nextRecord)) {
      break
    }
    starting_number <- page$nextRecord
  }

  if (!is.null(max_results) && length(all_results) > max_results) {
    all_results <- all_results[seq_len(max_results)]
  }

  list(results = all_results, total_hits = total_hits, last_page = last_page)
}


api_scene_search <- function(
    session,
    dataset_name,
    spatial_filter = NULL,
    temporal_filter = NULL,
    cloud_filter = NULL,
    metadata_filter = NULL,
    max_results = NULL) {
  scene_filter <- list(
    spatialFilter = spatial_filter,
    acquisitionFilter = temporal_filter,
    cloudCoverFilter = cloud_filter,
    metadataFilter = metadata_filter
  )
  scene_filter <- scene_filter[!vapply(scene_filter, is.null, logical(1))]

  pages <- m2m_paginate_scene_search(
    session,
    "scene-search",
    list(
      datasetName = dataset_name,
      sceneFilter = scene_filter
    ),
    "Scene search failed",
    max_results
  )

  list(
    results = coerce_scene_results(pages$results),
    total_hits = pages$total_hits
  )
}


# Find the scenes related to a given scene (scene-search-secondary endpoint).
#
# Only datasets that define a secondary relationship support this; the rest
# answer with a DATASET_ERROR. The related scenes belong to a *different*
# dataset, whose alias the response reports and which callers need in order to
# act on the results.
api_scene_search_secondary <- function(
    session,
    entity_id,
    dataset_name,
    max_results = NULL) {
  pages <- m2m_paginate_scene_search(
    session,
    "scene-search-secondary",
    list(entityId = entity_id, datasetName = dataset_name),
    "Secondary scene search failed",
    max_results
  )

  page <- pages$last_page %||% list()

  list(
    results = coerce_scene_results(pages$results),
    total_hits = if (is.finite(pages$total_hits)) pages$total_hits else 0L,
    secondary_dataset_alias = page$secondaryDatasetAlias %||% NA_character_,
    secondary_dataset_id = page$secondaryDatasetId %||% NA_character_
  )
}
