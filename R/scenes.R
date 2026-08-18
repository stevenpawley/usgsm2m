#' Send a request to build a scene list within the ERS service for use in other
#' M2M endpoints such as `download-options`, `order-products`, or another
#' `scene-search`.
#'
#' The scene list is assigned to a temporary identifier based on the dataset
#' name, and is then sent to the ERS system using the `scene-list-add` endpoint.
#' This function returns a tibble of scene entity IDs and metadata that can be
#' used in subsequent requests. The dataset_name and scene list identifier
#' are stored as attributes of the returned tibble for use in subsequent
#' requests.
#'
#' Typically this function is used after the `scene-search` function to build a
#' scene list from the search results.
#'
#' @param session An object of class "ers_session" returned by the `ers_login`
#' @param dataset_name datasetAlias name, for example 'landsat_ot_c2_l2'.
#' @param scenes Scene list of interest. The scene list is assigned to a
#'   temporary identifier based on the dataset name, and is then sent to the ERS
#'   system using the `scene-list-add` endpoint.
#' @param list_id The identifier of the scene list to create. This can be any
#'  string, but should be unique within the user's account. It is recommended to
#'  use a descriptive name that reflects the purpose of the scene list.
#'
#' @return tibble
#' @export
ers_scene_list_add <- function(session, dataset_name, scenes, list_id) {
  # prepare the scene list request using the scene entityIds
  scn_list_add_payload <- list(
    listId = list_id,
    idField = "entityId",
    entityIds = scenes$entityId,
    datasetName = dataset_name
  )

  # get how many scenes are available
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-list-add") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = scn_list_add_payload) %>%
    httr2::req_perform()

  if (resp$status_code == 200) {
    count <- httr2::resp_body_json(resp)$data
    message(glue::glue("{count} scenes are available"))
  } else {
    message("Scene list not created")
  }

  return(invisible(NULL))
}


#' Retrieve the list of scenes associated with a previously created scene list.
#'
#' This function sends a request to the `scene-list-get` endpoint to retrieve
#' the scenes associated with a specified scene list identifier. The function
#' returns a tibble of scene entity IDs and metadata that can be used in
#' subsequent requests.
#'
#' @param session An object of class "ers_session" returned by the `ers_session`
#' @param list_id The identifier of the scene list to retrieve.
#'
#' @returns A tibble of scene entity IDs and metadata associated with the
#'  specified scene list.
#' @export
ers_scene_list_get <- function(session, list_id) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-list-get") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(listId = list_id)) %>%
    httr2::req_perform()

  if (resp$status_code == 200) {
    entity_ids <- httr2::resp_body_json(resp)$data

    entity_ids <- entity_ids %>%
      jsonify::to_json() %>%
      jsonify::from_json()

    entity_ids <- dplyr::as_tibble(entity_ids)
  } else {
    message("Scene list not found")
    return(NULL)
  }
  return(entity_ids)
}


#' Remove a previously created scene list from the ERS service.
#'
#' @param session An object of class "ers_session" returned by the `ers_session`
#' @param list_id The identifier of the scene list to remove.
#'
#' @returns NULL. A message is printed indicating whether the scene list was
#'  successfully removed or not found.
#' @export
ers_scene_list_remove <- function(session, list_id) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-list-remove") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(listId = list_id)) %>%
    httr2::req_perform()

  if (resp$status_code == 200) {
    message(glue::glue("Scene list {list_id} removed"))
  } else {
    message("Scene list not found")
  }
  return(invisible(NULL))
}


#' Summarize the contents of a scene list.
#'
#' This function sends a request to the `scene-list-summary` endpoint to retrieve
#' a summary of the contents of a specified scene list identifier. The function
#' returns a tibble summarizing the spatial bounds and temporal extent of the
#' scenes in the list, as well as a summary of the datasets included in the list
#' and their availability.
#'
#' @param session An object of class "ers_session" returned by the `ers_session`
#' @param list_id The identifier of the scene list to summarize.
#'
#' @returns A tibble summarizing the contents of the specified scene list.
#' @export
ers_scene_list_summary <- function(session, list_id) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-list-summary") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(listId = list_id)) %>%
    httr2::req_perform()

  if (resp$status_code == 200) {
    summary <- httr2::resp_body_json(resp)$data

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

  } else {
    message("Scene list not found")
    return(NULL)
  }

  return(dataset_summary)
}


#' Identify product IDs that are "available" for each scene. The download
#' options request is used to discover downloadable products.
#'
#' This uses a send request to the `download-options` endpoint that builds
#' the list of products associated with each scene. This is typically followed
#' by use of the `download-request` and `download-retrieve` endpoints to prepare
#' and download the products, respectively.
#'
#' @param session An object of class "ers_session" returned by the `ers_login`
#' @param dataset_name datasetAlias name, for example 'landsat_ot_c2_l2'.
#' @param list_id The identifier of the scene list to retrieve.
#' @param entities A tibble of scene entity IDs and metadata returned by the
#'  `scene_list_add` function.
#' @param band_group A flag indicating whether to include secondary file groups.
#'   If True (the default), secondary file groups will be included in the
#'   payload.
#'
#' @return A list containing all available products in each scene. Each dict
#'   represents a product and includes various properties, such as the
#'   'entityId' of the scene that the product belongs to, and
#'   'secondaryDownloads' which is a list of dicts of associated bands or band
#'   related metadata.
#' @export
ers_scene_products <- function(session, dataset_name, list_id, entities, band_group = TRUE) {
  # Prepare the payload for the download options request
  download_opt_payload <- list(
    listId = list_id,
    datasetName = dataset_name
  )

  # If band_group is specified, include the secondary file groups in the payload
  if (band_group) {
    download_opt_payload$includeSecondaryFileGroups <- TRUE
  }

  # Send request to the download options endpoint and retrieve list of
  # available products
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-options") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = download_opt_payload) %>%
    httr2::req_perform()

  if (resp$status_code == 200) {
    product_list <- httr2::resp_body_json(resp)$data

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

    products <- purrr::list_rbind(products)
    class(products) <- c("scene_products", class(products))
    return(products)
  } else {
    message("No products found")
    return(NULL)
  }
}


ers_scene_metadata <- function(session, entity_id) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-metadata") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(entityId = entity_id)) %>%
    httr2::req_perform()

  if (resp$status_code == 200) {
    metadata <- httr2::resp_body_json(resp)$data

    metadata <- metadata %>%
      jsonify::to_json() %>%
      jsonify::from_json()

    metadata <- dplyr::as_tibble(metadata)
  } else {
    message("Scene metadata not found")
    return(NULL)
  }
  return(metadata)
}

ers_scene_metadata_list <- function(session, list_id) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-metadata-list") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(listId = list_id)) %>%
    httr2::req_perform()

  if (resp$status_code == 200) {
    metadata_list <- httr2::resp_body_json(resp)$data
    metadata_list <- lapply(metadata_list, function(metadata) {
      df <- metadata %>%
        jsonify::to_json() %>%
        jsonify::from_json()

      dplyr::as_tibble(df)
    })
    metadata_list <- purrr::list_rbind(metadata_list)
  } else {
    message("Scene metadata not found")
    return(NULL)
  }
  return(metadata_list)
}

#' Return a scene list based on spatial, temporal and cloud-based filters
#'
#' @param session An object of class "ers_session" returned by the `ers_login`
#' @param dataset_name datasetAlias name, for example 'landsat_ot_c2_l2'.
#' @param spatial_filter A named list that specifies a bounding box, for example:
#'   list(
#'     filterType = "mbr",
#'     lowerLeft = list(latitude = 59, longitude = -120),
#'     upperRight = list(latitude = 60, longitude = -119)
#'   )
#'   See https://m2m.cr.usgs.gov/api/docs/datatypes/#spatialFilter for more
#'  information on construction of the spatial filter.
#' @param temporal_filter A named list of start and end times, for example:
#'  list(start = "2020-06-20", end = "2020-09-22")
#' See https://m2m.cr.usgs.gov/api/docs/datatypes/#temporalFilter for more
#' information on the temporal filter.
#' @param cloud_filter  A named list of cloud cover ranges, for example:
#'  list(min = 0, max = 30)
#'
#' @return tibble of scene entity ids and metadata
#' @export
ers_scene_search <- function(
    session,
    dataset_name,
    spatial_filter,
    temporal_filter,
    cloud_filter) {
  search_payload <- list(
    datasetName = dataset_name,
    sceneFilter = list(
      spatialFilter = spatial_filter,
      acquisitionFilter = temporal_filter,
      cloudCoverFilter = cloud_filter
    )
  )

  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("scene-search") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = search_payload) %>%
    httr2::req_perform()

  if (resp$status_code == 200) {
    scenes <- httr2::resp_body_json(resp)$data

    r <- scenes %>%
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

    # todo
    # browseRotationEnabled
    # spatialBounds_coordinates
    # spatialCoverage_coordinates

  } else {
    message("Scene search failed")
    return(NULL)
  }

  return(df_flat)
}
