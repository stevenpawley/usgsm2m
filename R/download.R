# Queue downloads for a set of entityId/productId pairs (download-request).
#
# Returns the immediately-available downloads (with URLs) plus counts of the
# records the API accepted but has not prepared yet - those are collected
# later via the download-retrieve endpoint.
ers_download_request <- function(session, downloads, label) {
  if (is.null(downloads)) {
    stop("downloads cannot be NULL")
  }

  if (is.null(label)) {
    stop("label is required")
  }

  download_req_payload <- downloads %>%
    dplyr::select(c("entityId", "id")) %>%
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

  available_downloads <- lapply(
    download_queue$availableDownloads,
    function(x) {
      dplyr::as_tibble(t(x)) |>
        tidyr::unnest(dplyr::everything())
    }
  )
  available_downloads <- purrr::list_rbind(available_downloads)

  list(
    available = available_downloads,
    n_new = length(download_queue$newRecords),
    n_duplicate = length(download_queue$duplicateProducts)
  )
}


# Search the download queue regardless of status (download-search endpoint).
ers_download_search <- function(session) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-search") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  res <- m2m_response_data(resp, "No records found")

  m2m_records_to_tibble(res)
}


# Poll the queue for prepared download URLs by label (download-retrieve).
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

  list(
    available = m2m_records_to_tibble(queue$available),
    requested = m2m_records_to_tibble(queue$requested),
    queue_size = queue$queueSize
  )
}


# Download files to disk from URLs already obtained from the queue.
ers_download_files <- function(session, downloads, out_dir) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  results <- purrr::map2(
    downloads$entityId,
    downloads$url,
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
          path = NA_character_,
          status = "failed"
        )
      }
    }
  )

  purrr::list_rbind(results)
}


# Retrieve EULA text by code (download-eula endpoint).
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
      # as.list() keeps eulaCodes a JSON array for a single code; see the
      # note in ers_scene_list_add() on auto_unbox.
      data = list(
        eulaCode = eula_code,
        eulaCodes = if (is.null(eula_codes)) NULL else as.list(eula_codes)
      )
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  eulas <- m2m_response_data(resp, "EULA lookup failed")

  # A single eula_code returns one Eula object; eula_codes returns an array
  # of them - normalize both into a one-element list before building the
  # tibble, same as ers_dataset() does for its single-object response.
  if (!is.null(eula_code)) {
    eulas <- list(eulas)
  }

  m2m_records_to_tibble(eulas)
}


# Remove an entire download order by label (download-order-remove endpoint).
ers_download_remove_order <- function(session, label) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-order-remove") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(label = label)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  m2m_check_response(resp, "Failed to remove order")

  invisible(NULL)
}


# Remove individual items from a download order (download-remove endpoint).
ers_download_remove_items <- function(session, downloadId) {
  for (id in downloadId) {
    resp <- session$service %>%
      httr2::request() %>%
      httr2::req_url_path_append("download-remove") %>%
      httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
      httr2::req_body_json(data = list(downloadId = id)) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()

    m2m_check_response(resp, paste0("Failed to remove download item ", id))
  }

  invisible(NULL)
}
