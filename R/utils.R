# Check an M2M API response and raise an error if it failed.
#
# By default httr2::req_perform() throws on any HTTP 4xx/5xx status, so
# request pipelines must call httr2::req_error(is_error = function(resp) FALSE)
# before req_perform() for this function's status-code check to ever run.
#
# The M2M API also signals failures with HTTP 200 and an `errorCode` in the
# body (e.g. an expired session), so a 200 status alone doesn't mean success.
#
# Errors carry class "m2m_error", plus "m2m_http_error" for a bad status or
# "m2m_api_error" for an errorCode in the body, so callers can catch them
# selectively with tryCatch(). An authorisation failure additionally carries
# "m2m_no_access". Failures abort rather than returning NULL because these run
# inside method chains, where a NULL would surface later as a confusing
# "attempt to apply non-function".
m2m_check_response <- function(resp, on_fail_message, call = rlang::caller_env()) {
  if (httr2::resp_status(resp) != 200) {
    rlang::abort(
      paste0(
        on_fail_message,
        " (HTTP ", httr2::resp_status(resp), " ", httr2::resp_status_desc(resp), ")"
      ),
      class = c("m2m_http_error", "m2m_error"),
      status = httr2::resp_status(resp),
      call = call
    )
  }

  body <- httr2::resp_body_json(resp)

  if (!is.null(body$errorCode)) {
    detail <- if (!is.null(body$errorMessage)) body$errorMessage else body$errorCode

    # DATASET_AUTH means the dataset exists but this account cannot reach it.
    # Reported with the caller's context it reads as though the name were
    # wrong ("Dataset not found: Dataset status is unavailable to this user"),
    # which sends people looking for a typo, so say what is actually wrong and
    # point at the call that lists the datasets they can use.
    if (identical(body$errorCode, "DATASET_AUTH")) {
      rlang::abort(
        paste0(
          "No access to this dataset: ", detail,
          ". $find_datasets() lists the datasets this account can use."
        ),
        class = c("m2m_no_access", "m2m_api_error", "m2m_error"),
        error_code = body$errorCode,
        call = call
      )
    }

    rlang::abort(
      paste0(on_fail_message, ": ", detail),
      class = c("m2m_api_error", "m2m_error"),
      error_code = body$errorCode,
      call = call
    )
  }

  invisible(body)
}


# Extract the `data` payload from an M2M API response.
# See m2m_check_response() for the failure conditions this raises on.
m2m_response_data <- function(resp, on_fail_message, call = rlang::caller_env()) {
  m2m_check_response(resp, on_fail_message, call = call)$data
}


# Build the session list that the internal ers_* functions expect from an
# M2MSession R6 object's fields.
m2m_legacy_session <- function(api_key, service) {
  structure(list(api_key = api_key, service = service), class = "ers_session")
}


# Generate a scene list identifier scoped to this package. The M2M API
# requires a server-side scene list before download-options can be called;
# the R6 layer creates these transparently, so the id only needs to be
# unique within the user's account rather than meaningful.
m2m_new_list_id <- function() {
  paste0(
    "USGSm2m_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  )
}


# The host part of a URL, for deciding whether a request is going to the M2M
# API itself or to one of the hosts it proxies downloads to.
m2m_url_host <- function(url) {
  tolower(sub("^[a-z]+://([^/?#]+).*$", "\\1", url))
}


# Warn that a selector matched nothing, showing what was there to match.
#
# Product and band names vary between datasets - the bundle is "Landsat
# Collection 2 Level-2 Product Bundle" for one and "Standard Format" for
# another - so a pattern carried over from another dataset silently selects
# nothing, and the failure only surfaces later as "No products selected".
m2m_warn_no_match <- function(patterns, field, values) {
  available <- unique(values[!is.na(values)])
  shown <- available[seq_len(min(6L, length(available)))]

  warning(
    "Nothing matched ", paste0("\"", patterns, "\"", collapse = " or "),
    ".\n  Available ", field, ": ",
    paste(shown, collapse = ", "),
    if (length(available) > length(shown)) {
      paste0(", ... (", length(available), " in total)")
    },
    call. = FALSE
  )
}


# Convert a list of API records into a tibble, returning an empty tibble
# for an empty/absent record set.
m2m_records_to_tibble <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble())
  }

  records %>%
    jsonify::to_json() %>%
    jsonify::from_json() %>%
    dplyr::as_tibble()
}


# Format a "Next: $method()" hint for the print methods, which exist to make
# the pipeline order discoverable from the console.
m2m_print_next <- function(...) {
  steps <- c(...)
  cat("  Next:    ", paste0(steps, collapse = "  |  "), "\n", sep = "")
}
