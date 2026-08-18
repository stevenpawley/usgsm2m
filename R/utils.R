# Check whether an M2M API response succeeded, messaging on failure.
#
# By default httr2::req_perform() throws on any HTTP 4xx/5xx status, so
# request pipelines must call httr2::req_error(is_error = function(resp) FALSE)
# before req_perform() for this function's status-code check to ever run.
#
# The M2M API also signals failures with HTTP 200 and an `errorCode` in the
# body (e.g. an expired session), so a 200 status alone doesn't mean success.
m2m_request_ok <- function(resp, on_fail_message) {
  if (httr2::resp_status(resp) != 200) {
    message(paste0(
      on_fail_message,
      " (HTTP ", httr2::resp_status(resp), " ", httr2::resp_status_desc(resp), ")"
    ))
    return(FALSE)
  }

  body <- httr2::resp_body_json(resp)

  if (!is.null(body$errorCode)) {
    detail <- if (!is.null(body$errorMessage)) body$errorMessage else body$errorCode
    message(paste0(on_fail_message, ": ", detail))
    return(FALSE)
  }

  TRUE
}

# Extract the `data` payload from an M2M API response, or NULL on failure.
# See m2m_request_ok() for the failure conditions this checks.
m2m_response_data <- function(resp, on_fail_message) {
  if (!m2m_request_ok(resp, on_fail_message)) {
    return(NULL)
  }

  httr2::resp_body_json(resp)$data
}
