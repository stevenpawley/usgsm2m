# Helpers for the offline request/response tests.
#
# These use httr2's own mocking rather than recorded fixtures. Nothing is
# written to disk, which matters here: the login-token response body *is* the
# user's API key, and every other request carries it in an X-Auth-Token
# header, so recorded cassettes would have to be redacted or they would leak
# credentials into the repository.

# A session pointing at the real URL but with a placeholder key. No request
# made with it ever leaves the process.
mock_session <- function() {
  m2m_legacy_session("test_api_key", "https://m2m.cr.usgs.gov/api/api/json/stable/")
}

# Build an M2M-shaped JSON response.
mock_response <- function(data = NULL, status = 200L, error_code = NULL,
                          error_message = NULL) {
  # NULL entries are dropped rather than serialized: jsonify renders an R NULL
  # as {}, which the response check would then read as a non-null errorCode
  # and treat every mocked success as a failure.
  body <- list(data = data, errorCode = error_code, errorMessage = error_message)
  body <- body[!vapply(body, is.null, logical(1))]

  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(as.character(jsonify::to_json(body, unbox = TRUE)))
  )
}

# Run `code` with every request answered by `responses`, in order, and return
# both the result and the requests that were made.
#
# `responses` may be a single response (reused for every request) or a list
# consumed one per request.
with_captured_requests <- function(responses, code) {
  requests <- list()
  i <- 0L

  mock <- function(req) {
    i <<- i + 1L
    requests[[i]] <<- req
    if (inherits(responses, "httr2_response")) responses else responses[[i]]
  }

  result <- httr2::with_mocked_responses(mock, code)

  list(result = result, requests = requests, n = i)
}

# The parsed JSON body a request would send.
request_body <- function(req) {
  req$body$data
}

# One page of a scene-search response. A final page carries no nextRecord;
# the field is omitted rather than set to NULL for the same reason as in
# mock_response() - jsonify would render it as {}, which is not NULL once
# parsed, and the search would page forever.
mock_search_page <- function(scenes, total_hits, next_record = NULL) {
  page <- list(
    results = scenes,
    recordsReturned = length(scenes),
    totalHits = total_hits,
    startingNumber = 1L,
    nextRecord = next_record
  )
  page[!vapply(page, is.null, logical(1))]
}
