#' Request download URLs for scene ID - product ID pairs
#'
#' This method is used to insert the requested downloads into the download queue
#' and returns the available download URLs.
#'
#' This method generates a list of c(entityId = NULL, productId = NULL)
#' which is sent to the `download-request` endpoint to build download
#' payload. Typically this is followed by the `download-retrieve` endpoint
#' to actually download the products.
#'
#' @param session An object of class "ers_session" returned by the `ers_login`
#' @param products A tibble of scene entity IDs and metadata returned by the
#'   `get_scene_products` function.
#' @param band_names A character vector of band names to download. If NULL, all
#'   bands will be downloaded.
#' @param label A label to assign to the download request. This is used to
#'  identify the download request in the download queue.
#'
#' @return A list of download URLs for each scene ID - product ID pair
#' @export
ers_download_request <- function(session, products, band_names = NULL, label) {
  if (is.null(products)) {
    stop("products cannot be NULL")
  }

  if (is.null(label)) {
    stop("label is required")
  }

  # subset the products list based on the supplied band_names
  downloads <- products %>%
    dplyr::select("secondaryDownloads") %>%
    tidyr::unnest(dplyr::all_of("secondaryDownloads"))

  if (!is.null(band_names)) {
    downloads <- dplyr::filter(
      products,
      bulkAvailable == TRUE,
      stringr::str_detect(displayId, stringr::str_c(band_names, collapse = "|"))
    )
  }

  downloads <- downloads %>%
    dplyr::select(c("entityId", "id"))

  # Send the download request using the provided payload and store the results
  download_req_payload <- downloads %>%
    setNames(c("entityId", "productId")) %>%
    purrr::transpose()

  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-request") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(
      data = list(downloads = download_req_payload, label = label)
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  download_queue <- m2m_response_data(resp, "Download request failed")

  if (is.null(download_queue)) {
    return(NULL)
  }

  available_downloads <- lapply(
    download_queue$availableDownloads,
    function(x) {
      dplyr::as_tibble(t(x)) |>
        tidyr::unnest(dplyr::everything())
    }
  )
  available_downloads <- purrr::list_rbind(available_downloads)

  # todo: handle checksum_values

  if (
    length(download_queue$newRecords) == 0 &&
      length(download_queue$duplicateProducts) == 0
  ) {
    message("No records returned")
  }

  return(available_downloads)
}


#' Check the download queue for all available and previously requested but not
#' completed downloads.
#'
#' @param session An object of class "ers_session" returned by the `ers_session`
#'
#' @returns A tibble with the current download queue
#' @export
ers_download_search <- function(session) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-search") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  res <- m2m_response_data(resp, "No records found")

  if (is.null(res)) {
    return(NULL)
  }

  df <- res %>%
    jsonify::to_json() %>%
    jsonify::from_json() %>%
    dplyr::as_tibble()

  return(df)
}


#' Poll the download queue for download URLs, by label
#'
#' Wraps the `download-retrieve` endpoint. Call this after
#' `ers_download_request()` to pick up items that were not immediately
#' available (they showed up in `newRecords`/`duplicateProducts` rather than
#' `availableDownloads` because the distribution system needed time to
#' prepare them): this method returns their download URLs once ready, and
#' still-pending items separately so you know to poll again later.
#'
#' Note this is distinct from `ers_download_retrieve()`, which downloads
#' files to disk given URLs you already have - this function only queries
#' the M2M API for the current state of the queue.
#'
#' @param session An object of class "ers_session" returned by the `ers_login`
#'   function.
#' @param label The label used when the downloads were originally requested.
#'   If NULL, downloads across all labels are returned.
#' @param download_application Optional name of the application performing
#'   the download, to scope results to.
#'
#' @return A list with three elements: `available` (a tibble of downloads
#'   that are ready, including their download URLs), `requested` (a tibble
#'   of downloads still being prepared), and `queue_size` (the total number
#'   of items left in the queue).
#' @export
ers_download_queue <- function(session, label = NULL, download_application = NULL) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-retrieve") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(
      data = list(label = label, downloadApplication = download_application)
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  queue <- m2m_response_data(resp, "Download retrieve failed")

  if (is.null(queue)) {
    return(NULL)
  }

  to_tibble <- function(records) {
    if (length(records) == 0) {
      return(tibble::tibble())
    }
    records %>%
      jsonify::to_json() %>%
      jsonify::from_json() %>%
      dplyr::as_tibble()
  }

  list(
    available = to_tibble(queue$available),
    requested = to_tibble(queue$requested),
    queue_size = queue$queueSize
  )
}


#' Download all files in the download queue. Returns all available and
#' previously requests but not completed downloads.
#'
#' @param session An object of class "ers_session" returned by the `ers_session`
#' @param download_request_results The results returned by the
#'   `ers_download_request` function.
#' @param label A label to assign to the download request. This is used to
#'  identify the download request in the download queue.
#' @param out_dir The output directory to save the downloaded files.
#'
#' @return A tibble with the download status for each file
#' @export
ers_download_retrieve <- function(
  session,
  download_request_results,
  label,
  out_dir
) {

  results <- purrr::map2(
    download_request_results$entityId,
    download_request_results$url,
    function(entityId, url) {
      filename <- file.path(out_dir, entityId)
      filename <- gsub("_TIF", ".TIF", filename)

      resp <- session$service %>%
        httr2::request() %>%
        httr2::req_url(url) %>%
        httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
        httr2::req_retry(max_tries = 5L, backoff = function(x) 30) %>%
        httr2::req_error(is_error = function(resp) FALSE) %>%
        httr2::req_perform(path = filename)

      if (resp$status_code == 200) {
        tibble::tibble(
          entityId = entityId,
          url = url,
          path = filename,
          status = "downloaded"
        )
      } else {
        tibble::tibble(
          entityId = entityId,
          url = url,
          path = NA,
          status = "failed"
        )
      }
    }
  )

  results <- purrr::list_rbind(results)
  return(results)
}

#' Retrieve the text of one or more End User License Agreements (EULAs)
#'
#' Some datasets (e.g. Sentinel data via EarthExplorer) require the user to
#' accept a EULA before `ers_download_request()` will succeed for them. This
#' method retrieves the EULA text so it can be reviewed or displayed to the
#' user; acceptance itself is completed once through the EarthExplorer
#' website (https://ers.cr.usgs.gov/) - there is no endpoint in the M2M API
#' to accept a EULA programmatically.
#'
#' @param session An object of class "ers_session" returned by the `ers_login`
#'   function.
#' @param eula_code A single EULA code to retrieve. Use this or `eula_codes`,
#'   not both.
#' @param eula_codes A character vector of EULA codes to retrieve. Use this
#'   or `eula_code`, not both.
#'
#' @return A tibble with one row per EULA, containing `eulaCode` and
#'   `agreementContent`.
#' @export
ers_download_eula <- function(session, eula_code = NULL, eula_codes = NULL) {
  if (is.null(eula_code) && is.null(eula_codes)) {
    stop("eula_code or eula_codes must be supplied")
  }

  if (!is.null(eula_code) && !is.null(eula_codes)) {
    stop("Supply only one of eula_code or eula_codes, not both")
  }

  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-eula") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(
      data = list(eulaCode = eula_code, eulaCodes = eula_codes)
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  eulas <- m2m_response_data(resp, "EULA lookup failed")

  if (is.null(eulas)) {
    return(NULL)
  }

  # A single eula_code returns one Eula object; eula_codes returns an array
  # of them - normalize both into a one-element list before building the
  # tibble, same as ers_dataset() does for its single-object response.
  if (!is.null(eula_code)) {
    eulas <- list(eulas)
  }

  eulas %>%
    jsonify::to_json() %>%
    jsonify::from_json() %>%
    dplyr::as_tibble()
}


#' Remove items from an existing download order
#'
#' @param session An authenticated session object.
#' @param label The label of the download order to remove.
#'
#' @return None. Prints a message indicating success or failure.
#' @export
ers_download_remove_order <- function(session, label) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-order-remove") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(label = label)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  if (m2m_request_ok(resp, "Failed to remove order")) {
    message("Order removed successfully")
  }

  return(invisible(NULL))
}


#' Remove specific items from an existing download order
#'
#' @param session An authenticated session object.
#' @param downloadId A vector of download IDs to remove from the order.
#' @return None. Prints a message indicating success or failure.
#' @export
ers_download_remove_items <- function(session, downloadId) {
  for (id in downloadId) {
    resp <- session$service %>%
      httr2::request() %>%
      httr2::req_url_path_append("download-remove") %>%
      httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
      httr2::req_body_json(data = list(downloadId = id)) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()

    m2m_request_ok(resp, paste0("Failed to remove download item ", id))
  }

  return(invisible(NULL))
}
