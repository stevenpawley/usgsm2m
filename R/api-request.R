# Shared transport primitives for the internal api_* endpoint functions.

# Check an M2M API response and raise a classed error if it failed. The API
# reports failures both through HTTP status codes and errorCode in a 200 body.
m2m_check_response <- function(resp, on_fail_message, call = rlang::caller_env()) {
  if (httr2::resp_status(resp) != 200) {
    rlang::abort(
      paste0(
        on_fail_message,
        " (HTTP ", httr2::resp_status(resp), " ",
        httr2::resp_status_desc(resp), ")"
      ),
      class = c("m2m_http_error", "m2m_error"),
      status = httr2::resp_status(resp),
      call = call
    )
  }

  body <- httr2::resp_body_json(resp)

  if (!is.null(body$errorCode)) {
    detail <- body$errorMessage %||% body$errorCode

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


m2m_response_data <- function(resp, on_fail_message, call = rlang::caller_env()) {
  m2m_check_response(resp, on_fail_message, call = call)$data
}


# Perform an authenticated request. Endpoint functions construct the payload
# and interpret the response; this function owns only shared HTTP mechanics.
m2m_request <- function(session, endpoint, data = NULL) {
  req <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append(endpoint) %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_error(is_error = function(resp) FALSE)

  if (!is.null(data)) {
    req <- httr2::req_body_json(req, data = data)
  }

  httr2::req_perform(req)
}
