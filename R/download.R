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
    httr2::req_perform()

  if (resp$status_code == 200) {
    download_queue <- httr2::resp_body_json(resp)$data
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
  } else {
    message("Download request failed")
    return(NULL)
  }

  return(available_downloads)
}

ers_download_queue_search <- function(session) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-search") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_perform()

  if (resp$status_code == 200) {
    res <- httr2::resp_body_json(resp)$data

    df <- res %>%
      jsonify::to_json() %>%
      jsonify::from_json() %>%
      dplyr::as_tibble()
  } else {
    message("No records found")
    return(NULL)
  }

  return(df)
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
    download_queue$entityId,
    download_queue$url,
    function(entityId, url) {
      filename <- file.path(out_dir, entityId)
      filename <- gsub("_TIF", ".TIF", filename)

      resp <- session$service %>%
        httr2::request() %>%
        httr2::req_url(url) %>%
        httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
        httr2::req_retry(max_tries = 5L, backoff = function(x) 30) %>%
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
