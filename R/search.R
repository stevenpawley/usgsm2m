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
    httr2::req_perform()

  if (datasearch_result$status_code == 200) {
    datasets <- httr2::resp_body_json(datasearch_result)$data

    df <- datasets %>%
      jsonify::to_json() %>%
      jsonify::from_json() %>%
      dplyr::as_tibble()

    df <- df |>
      tidyr::unnest(c(dplyr::where(is.list), -"spatialBounds"))

    # coerce spatialBounds to nested tibble
    if (inherits(df$spatialBounds, "data.frame")) {
      df$spatialBounds <- apply(df$spatialBounds, 1, function(x) dplyr::as_tibble(t(x)))
    } else {
      df$spatialBounds <- lapply(df$spatialBounds, function(x) dplyr::as_tibble(x))
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

  } else {
    message("Dataset not found")
    return(NULL)
  }
  return(df)
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

  }

  return(df_flat)
}
