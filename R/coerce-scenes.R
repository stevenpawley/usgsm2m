# Normalize scene, scene-list, and product payloads returned by the M2M API.

coerce_scene_list_summary <- function(summary) {
  bounds <- summary$summary$spatialBounds
  spatial_bounds <- if (is.null(bounds) || length(bounds) == 0) {
    tibble::tibble()
  } else {
    dplyr::as_tibble(bounds) |> tidyr::unnest("coordinates")
  }

  if (length(summary$datasets) == 0) {
    empty <- dplyr::tibble(
      datasetName = character(),
      sceneCount = integer(),
      invalidSceneCount = integer(),
      invalidScenes = list(),
      listTimeout = character(),
      datasetAvailable = logical(),
      spatialBounds = list(),
      temporalExtent = list()
    )
    attr(empty, "spatialBounds") <- spatial_bounds
    return(empty)
  }

  dataset_summary <- lapply(
    summary$datasets,
    function(x) {
      dplyr::tibble(
        datasetName = x$datasetName %||% NA_character_,
        sceneCount = x$sceneCount %||% NA_integer_,
        invalidSceneCount = x$invalidSceneCount %||% NA_integer_,
        invalidScenes = list(coerce_records(x$invalidScenes)),
        listTimeout = x$listTimeout %||% NA_character_,
        datasetAvailable = x$datasetAvailable %||% NA,
        spatialBounds = list(dplyr::as_tibble(x$spatialBounds)),
        temporalExtent = list(dplyr::as_tibble(x$temporalExtent))
      )
    }
  )
  dataset_summary <- purrr::list_rbind(dataset_summary)
  attr(dataset_summary, "spatialBounds") <- spatial_bounds

  dataset_summary
}


# Turn a download-options payload into one row per product, keeping its
# individual files in a secondaryDownloads list-column when requested.
coerce_products <- function(product_list, band_group = TRUE) {
  if (length(product_list) == 0) {
    return(tibble::tibble())
  }

  products <- lapply(product_list, function(product) {
    secondary <- product$secondaryDownloads

    df <- product[names(product) != "secondaryDownloads"] %>%
      jsonify::to_json() %>%
      jsonify::from_json()

    meta <- df[!vapply(df, is.null, logical(1))]
    meta <- dplyr::as_tibble(meta)

    if (band_group) {
      meta$secondaryDownloads <- list(coerce_records(secondary))
    }
    meta
  })

  purrr::list_rbind(products)
}


coerce_scene_metadata_list <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble())
  }

  records <- lapply(records, function(record) {
    record %>%
      jsonify::to_json() %>%
      jsonify::from_json() %>%
      dplyr::as_tibble()
  })

  purrr::list_rbind(records)
}


# Flatten scene-search results into a tibble and restore each scene's metadata
# as a nested tibble despite jsonify's matrix/list representation differences.
coerce_scene_results <- function(all_results) {
  if (length(all_results) == 0) {
    return(tibble::tibble())
  }

  result <- list(results = all_results) %>%
    jsonify::to_json() %>%
    jsonify::from_json()

  flat <- dplyr::as_tibble(result$results) |>
    tidyr::unnest(dplyr::where(is.list), names_sep = "_") |>
    dplyr::rename_with(~ gsub("^browse_", "", .x))

  metadata_values <- function(column, i) {
    if (is.matrix(column)) {
      as.character(column[i, ])
    } else {
      as.character(column[[i]])
    }
  }

  flat$metadata <- lapply(
    seq_len(nrow(flat)),
    function(i) {
      dplyr::tibble(
        id = metadata_values(flat$metadata_id, i),
        fieldName = metadata_values(flat$metadata_fieldName, i),
        value = metadata_values(flat$metadata_value, i),
        dictionaryLink = metadata_values(flat$metadata_dictionaryLink, i)
      ) |>
        dplyr::mutate(
          dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, ""))
        )
    }
  )

  flat |>
    dplyr::select(-dplyr::starts_with("metadata_"))
}
