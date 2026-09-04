# Internal M2M download endpoints and file transfer operations. API response
# and filename normalization lives in coerce-downloads.R.

# Queue downloads for a set of entityId/productId pairs (download-request).
api_download_request <- function(session, downloads, label) {
  if (is.null(downloads)) {
    stop("downloads cannot be NULL")
  }

  if (is.null(label)) {
    stop("label is required")
  }

  payload <- downloads %>%
    dplyr::select(c("entityId", "id")) %>%
    setNames(c("entityId", "productId")) %>%
    purrr::transpose()

  resp <- m2m_request(
    session,
    "download-request",
    list(downloads = payload, label = label)
  )

  queue <- m2m_response_data(resp, "Download request failed")

  coerce_download_request(queue)
}


# Search the download queue regardless of status (download-search endpoint).
api_download_search <- function(session) {
  resp <- m2m_request(session, "download-search")

  records <- m2m_response_data(resp, "No records found")

  coerce_records(records)
}


# Poll the queue for prepared download URLs by label (download-retrieve).
api_download_queue <- function(session, label = NULL, download_application = NULL) {
  resp <- m2m_request(
    session,
    "download-retrieve",
    list(label = label, downloadApplication = download_application)
  )

  queue <- m2m_response_data(resp, "Download retrieve failed")

  list(
    available = coerce_records(queue$available),
    requested = coerce_records(queue$requested),
    queue_size = queue$queueSize
  )
}


# Download files to disk from URLs already obtained from the queue.
#
# Returns one row per file with the downloadId carried through where the queue
# supplied one, and the size actually written, so proxied downloads can be
# reported back to the API afterwards.
api_download_files <- function(session, downloads, out_dir) {
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
      # The real name is only known once the server has answered, so stream to
      # a scratch file in the destination directory and rename it afterwards.
      # Writing it in out_dir keeps the rename on one filesystem, and a failed
      # transfer leaves nothing behind rather than a file named after the scene
      # holding an error body.
      scratch <- tempfile("m2m-", tmpdir = out_dir)

      req <- httr2::request(url) %>%
        httr2::req_retry(max_tries = 5L, backoff = function(x) 30) %>%
        httr2::req_error(is_error = function(resp) FALSE)

      # Send the token only to the M2M host.
      if (identical(m2m_url_host(url), service_host)) {
        req <- httr2::req_headers(req, `X-Auth-Token` = session$api_key)
      }

      resp <- httr2::req_perform(req, path = scratch)
      status <- httr2::resp_status(resp)

      if (status == 200) {
        filename <- file.path(
          out_dir,
          coerce_download_name(resp, url = url, entityId = entityId)
        )

        if (file.exists(scratch)) {
          file.rename(scratch, filename)
        }

        return(tibble::tibble(
          entityId = entityId,
          downloadId = downloadId,
          url = url,
          path = filename,
          size = file.size(filename),
          status = "downloaded"
        ))
      }

      unlink(scratch)

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
api_download_complete_proxied <- function(session, downloads) {
  if (nrow(downloads) == 0) {
    return(invisible(NULL))
  }

  payload <- downloads %>%
    dplyr::select(c("downloadId", "size")) %>%
    setNames(c("downloadId", "downloadedSize")) %>%
    purrr::transpose()

  resp <- m2m_request(
    session,
    "download-complete-proxied",
    list(proxiedDownloads = payload)
  )

  m2m_check_response(resp, "Failed to report completed proxied downloads")

  invisible(NULL)
}


# List the distinct labels in the user's download queue (download-labels).
# Each row summarizes one order.
api_download_labels <- function(session, download_application = NULL) {
  resp <- m2m_request(
    session,
    "download-labels",
    list(downloadApplication = download_application)
  )

  labels <- m2m_response_data(resp, "Could not list download labels")

  coerce_records(labels)
}


# Summarize an order by dataset (download-summary endpoint).
api_download_summary <- function(session, label, download_application = "M2M",
                                 send_email = FALSE) {
  if (is.null(label)) {
    stop("label is required")
  }

  # The endpoint answers INPUT_PARAMETER_REQUIRED without it, so refuse here
  # rather than making a request that cannot succeed.
  if (is.null(download_application)) {
    stop("download_application is required by the download-summary endpoint")
  }

  resp <- m2m_request(
    session,
    "download-summary",
    list(
      downloadApplication = download_application,
      label = label,
      sendEmail = send_email
    )
  )

  summary <- m2m_response_data(resp, "Download summary failed")

  list(
    label = summary$label %||% label,
    download_count = summary$downloadCount %||% 0L,
    scene_count = summary$sceneCount %||% 0L,
    total_estimated_size = summary$totalEstimatedSize %||% 0L,
    collections = coerce_records(summary$collections)
  )
}


# Move an order's scenes into the queue for processing (download-order-load).
#
# This mutates server-side state rather than reading it: it is what makes a
# staged order start being prepared for download.
api_download_order_load <- function(session, label, download_application = NULL) {
  if (is.null(label)) {
    stop("label is required")
  }

  resp <- m2m_request(
    session,
    "download-order-load",
    list(label = label, downloadApplication = download_application)
  )

  loaded <- m2m_response_data(resp, "Could not load download order")

  coerce_records(loaded)
}


# Retrieve EULA text by code (download-eula endpoint).
api_download_eula <- function(session, eula_code = NULL, eula_codes = NULL) {
  if (is.null(eula_code) && is.null(eula_codes)) {
    stop("eula_code or eula_codes must be supplied")
  }

  if (!is.null(eula_code) && !is.null(eula_codes)) {
    stop("Supply only one of eula_code or eula_codes, not both")
  }

  resp <- m2m_request(
    session,
    "download-eula",
    # as.list() keeps eulaCodes a JSON array for a single code; see the
    # note in api_scene_list_add() on auto_unbox.
    list(
      eulaCode = eula_code,
      eulaCodes = if (is.null(eula_codes)) NULL else as.list(eula_codes)
    )
  )

  eulas <- m2m_response_data(resp, "EULA lookup failed")

  # A single eula_code returns one Eula object; eula_codes returns an array
  # of them - normalize both into a one-element list before building the
  # tibble, same as api_dataset() does for its single-object response.
  if (!is.null(eula_code)) {
    eulas <- list(eulas)
  }

  coerce_records(eulas)
}


# Remove an entire download order by label (download-order-remove endpoint).
api_download_remove_order <- function(session, label) {
  resp <- m2m_request(session, "download-order-remove", list(label = label))

  m2m_check_response(resp, "Failed to remove order")

  invisible(NULL)
}


# Remove individual items from the download queue (download-remove endpoint).
api_download_remove_items <- function(session, download_id, quiet = FALSE) {
  download_id <- unique(download_id[!is.na(download_id)])

  if (length(download_id) == 0) {
    return(invisible(0L))
  }

  if (!quiet && length(download_id) > 25) {
    message("Removing ", length(download_id), " items")
  }

  for (id in download_id) {
    resp <- m2m_request(session, "download-remove", list(downloadId = id))

    m2m_check_response(resp, paste0("Failed to remove download item ", id))
  }

  invisible(length(download_id))
}
