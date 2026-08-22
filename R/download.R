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
#
# Returns one row per file with the downloadId carried through where the queue
# supplied one, and the size actually written, so proxied downloads can be
# reported back to the API afterwards.
ers_download_files <- function(session, downloads, out_dir) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  service_host <- m2m_url_host(session$service)

  download_ids <- if ("downloadId" %in% names(downloads)) {
    downloads$downloadId
  } else {
    rep(NA_integer_, nrow(downloads))
  }

  results <- purrr::pmap(
    list(downloads$entityId, downloads$url, download_ids),
    function(entityId, url, downloadId) {
      filename <- file.path(out_dir, entityId)
      filename <- gsub("_TIF", ".TIF", filename)

      req <- httr2::request(url) %>%
        httr2::req_retry(max_tries = 5L, backoff = function(x) 30) %>%
        httr2::req_error(is_error = function(resp) FALSE)

      # Only the M2M API itself gets the session token. Proxied downloads are
      # served by other USGS hosts on signed URLs, where the signature is the
      # credential - sending the token there would hand it to a host that
      # neither needs nor asked for it.
      if (identical(m2m_url_host(url), service_host)) {
        req <- httr2::req_headers(req, `X-Auth-Token` = session$api_key)
      }

      resp <- httr2::req_perform(req, path = filename)
      status <- httr2::resp_status(resp)

      if (status == 200) {
        return(tibble::tibble(
          entityId = entityId,
          downloadId = downloadId,
          url = url,
          path = filename,
          size = file.size(filename),
          status = "downloaded"
        ))
      }

      # A signed URL that has expired comes back as 403, which is otherwise an
      # opaque failure.
      note <- if (status %in% c(401L, 403L)) "expired" else "failed"

      tibble::tibble(
        entityId = entityId,
        downloadId = downloadId,
        url = url,
        path = NA_character_,
        size = NA_real_,
        status = note
      )
    }
  )

  purrr::list_rbind(results)
}


# Report proxied downloads as complete (download-complete-proxied endpoint).
#
# The M2M API does not serve proxied downloads itself, so it cannot observe
# the transfer; without this they stay in the queue indefinitely.
ers_download_complete_proxied <- function(session, downloads) {
  if (nrow(downloads) == 0) {
    return(invisible(NULL))
  }

  payload <- downloads %>%
    dplyr::select(c("downloadId", "size")) %>%
    setNames(c("downloadId", "downloadedSize")) %>%
    purrr::transpose()

  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-complete-proxied") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(proxiedDownloads = payload)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  m2m_check_response(resp, "Failed to report completed proxied downloads")

  invisible(NULL)
}


# List the distinct labels in the user's download queue (download-labels).
# Each row summarizes one order.
ers_download_labels <- function(session, download_application = NULL) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-labels") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(downloadApplication = download_application)) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  labels <- m2m_response_data(resp, "Could not list download labels")

  m2m_records_to_tibble(labels)
}


# Summarize an order by dataset (download-summary endpoint).
#
# downloadApplication is required by the API - omitting it returns
# INPUT_PARAMETER_REQUIRED - and must match the application the downloads were
# requested under for the counts to be populated.
ers_download_summary <- function(
    session,
    label,
    download_application = "M2M",
    send_email = FALSE) {
  if (is.null(label)) {
    stop("label is required")
  }

  if (is.null(download_application)) {
    stop("download_application is required by the download-summary endpoint")
  }

  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-summary") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(
      data = list(
        downloadApplication = download_application,
        label = label,
        sendEmail = send_email
      )
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  summary <- m2m_response_data(resp, "Download summary failed")

  list(
    label = summary$label %||% label,
    download_count = summary$downloadCount %||% 0L,
    scene_count = summary$sceneCount %||% 0L,
    total_estimated_size = summary$totalEstimatedSize %||% 0L,
    collections = m2m_records_to_tibble(summary$collections)
  )
}


# Move an order's scenes into the queue for processing (download-order-load).
#
# This mutates server-side state rather than reading it: it is what makes a
# staged order start being prepared for download.
ers_download_order_load <- function(session, label, download_application = NULL) {
  if (is.null(label)) {
    stop("label is required")
  }

  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-order-load") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(
      data = list(label = label, downloadApplication = download_application)
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  loaded <- m2m_response_data(resp, "Could not load download order")

  # The endpoint answers with a null payload when the label matches nothing.
  m2m_records_to_tibble(loaded)
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
