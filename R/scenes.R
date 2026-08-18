# Create a server-side scene list (scene-list-add endpoint).
#
# The M2M API requires a scene list before download-options / order-products
# can be called against a set of scenes. Returns the number of scenes the
# API accepted into the list.
ers_scene_list_add <- function(session, dataset_name, scenes, list_id) {
  # as.list() keeps entityIds a JSON array even for a single scene: httr2
  # serializes with auto_unbox = TRUE, which would otherwise turn a length-1
  # vector into a bare string and make the API return HTTP 500.
  scn_list_add_payload <- list(
    listId = list_id,
    idField = "entityId",
    entityIds = as.list(scenes$entityId),
    datasetName = dataset_name
  )

  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-list-add") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = scn_list_add_payload) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  m2m_response_data(resp, "Scene list not created")
}


# Retrieve the scenes in a previously created scene list (scene-list-get).
ers_scene_list_get <- function(session, list_id) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-list-get") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(listId = list_id)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  entity_ids <- m2m_response_data(resp, "Scene list not found")

  m2m_records_to_tibble(entity_ids)
}


# Delete a scene list from the ERS service (scene-list-remove endpoint).
ers_scene_list_remove <- function(session, list_id) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-list-remove") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(listId = list_id)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  m2m_check_response(resp, "Scene list not found")

  invisible(NULL)
}


# Summarize a scene list's contents (scene-list-summary endpoint). Returns a
# per-dataset tibble, with the overall spatial bounds attached as the
# "spatialBounds" attribute.
ers_scene_list_summary <- function(session, list_id) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-list-summary") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(listId = list_id)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  summary <- m2m_response_data(resp, "Scene list not found")

  spatial_bounds <- dplyr::as_tibble(summary$summary$spatialBounds) |>
    tidyr::unnest("coordinates")

  dataset_summary <- lapply(
    summary$datasets,
    function(x) {
      dplyr::tibble(
        datasetName = x$datasetName,
        sceneCount = x$sceneCount,
        listTimeout = x$listTimeout,
        invalidScenes = ifelse(length(x$invalidScenes) == 0, NA_character_, list(dplyr::as_tibble(x$invalidScenes))),
        datasetAvailable = x$datasetAvailable,
        spatialBounds = list(dplyr::as_tibble(x$spatialBounds)),
        temporalExtent = list(dplyr::as_tibble(x$temporalExtent))
      )
    }
  )
  dataset_summary <- purrr::list_rbind(dataset_summary)
  attr(dataset_summary, "spatialBounds") <- spatial_bounds

  return(dataset_summary)
}


# Discover downloadable products for a scene list (download-options endpoint).
# `secondaryDownloads` (the individual bands) is kept as a list-column.
ers_scene_products <- function(session, dataset_name, list_id, band_group = TRUE) {
  download_opt_payload <- list(
    listId = list_id,
    datasetName = dataset_name
  )

  if (band_group) {
    download_opt_payload$includeSecondaryFileGroups <- TRUE
  }

  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-options") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = download_opt_payload) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  product_list <- m2m_response_data(resp, "No products found")

  if (length(product_list) == 0) {
    return(tibble::tibble())
  }

  products <- lapply(product_list, function(product) {
    df <- product %>%
      jsonify::to_json() %>%
      jsonify::from_json()

    meta <- df[names(df) != "secondaryDownloads"]
    meta <- meta[!sapply(meta, is.null)]
    meta <- dplyr::as_tibble(meta)

    if (band_group) {
      meta$secondaryDownloads <- list(df$secondaryDownloads)
    }
    meta
  })

  purrr::list_rbind(products)
}


# Fetch metadata for a single scene (scene-metadata endpoint).
ers_scene_metadata <- function(session, entity_id) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-metadata") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(entityId = entity_id)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  metadata <- m2m_response_data(resp, "Scene metadata not found")

  metadata %>%
    jsonify::to_json() %>%
    jsonify::from_json() %>%
    dplyr::as_tibble()
}


# Fetch metadata for every scene in a scene list (scene-metadata-list).
ers_scene_metadata_list <- function(session, list_id) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-metadata-list") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(listId = list_id)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  metadata_list <- m2m_response_data(resp, "Scene metadata not found")

  if (length(metadata_list) == 0) {
    return(tibble::tibble())
  }

  metadata_list <- lapply(metadata_list, function(metadata) {
    metadata %>%
      jsonify::to_json() %>%
      jsonify::from_json() %>%
      dplyr::as_tibble()
  })

  purrr::list_rbind(metadata_list)
}


# Search a dataset for scenes (scene-search endpoint), paging through all
# results since the API returns at most a few thousand scenes per request.
#
# Returns a list of the results tibble plus the API's reported totalHits, so
# callers can tell a truncated result set from a complete one.
ers_scene_search <- function(
    session,
    dataset_name,
    spatial_filter = NULL,
    temporal_filter = NULL,
    cloud_filter = NULL,
    metadata_filter = NULL,
    max_results = NULL) {
  page_size <- 10000L
  starting_number <- 1L
  all_results <- list()
  total_hits <- Inf

  scene_filter <- list(
    spatialFilter = spatial_filter,
    acquisitionFilter = temporal_filter,
    cloudCoverFilter = cloud_filter,
    metadataFilter = metadata_filter
  )
  scene_filter <- scene_filter[!vapply(scene_filter, is.null, logical(1))]

  while (length(all_results) < total_hits) {
    if (!is.null(max_results) && length(all_results) >= max_results) {
      break
    }

    requested <- if (is.null(max_results)) {
      page_size
    } else {
      min(page_size, max_results - length(all_results))
    }

    search_payload <- list(
      datasetName = dataset_name,
      sceneFilter = scene_filter,
      maxResults = requested,
      startingNumber = starting_number
    )

    resp <- session$service %>%
      httr2::request() %>%
      httr2::req_url_path_append("scene-search") %>%
      httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
      httr2::req_body_json(data = search_payload) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()

    page <- m2m_response_data(resp, "Scene search failed")

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

  if (length(all_results) == 0) {
    return(list(results = tibble::tibble(), total_hits = 0L))
  }

  r <- list(results = all_results) %>%
    jsonify::to_json() %>%
    jsonify::from_json()

  df <- dplyr::as_tibble(r$results)

  # flatten list-columns
  df_flat <- df |>
    tidyr::unnest(dplyr::where(is.list), names_sep = "_") |>
    dplyr::rename_with(~ gsub("^browse_", "", .x))

  # re-nest metadata
  df_flat$metadata <- lapply(
    seq_len(nrow(df_flat)),
    function(i) {
      dplyr::tibble(
        id = as.character(df_flat[i, ]$metadata_id),
        fieldName = as.character(df_flat[i, ]$metadata_fieldName),
        value = as.character(df_flat[i, ]$metadata_value),
        dictionaryLink = as.character(df_flat[i, ]$metadata_dictionaryLink),
      ) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, "")))
    }
  )

  df_flat <- df_flat |>
    dplyr::select(-dplyr::starts_with("metadata_"))

  list(results = df_flat, total_hits = total_hits)
}
