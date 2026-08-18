# Authenticate against the M2M login-token endpoint and return an API key.
#
# Internal: the public entry point is m2m_session(), which wraps this in an
# M2MSession object.
m2m_login <- function(username, token, service_url) {
  if (is.null(username) || !is.character(username) || nchar(username) == 0) {
    stop("Username cannot be NULL and must be a character string")
  }

  if (is.null(token) || !is.character(token) || nchar(token) == 0) {
    stop("Token cannot be NULL and must be a character string")
  }

  resp <- tryCatch(
    {
      httr2::request(service_url) %>%
        httr2::req_url_path_append("login-token") %>%
        httr2::req_body_json(list(username = username, token = token)) %>%
        httr2::req_error(is_error = function(resp) FALSE) %>%
        httr2::req_perform()
    },
    error = function(e) {
      stop("Login was unsuccessful, check: https://ers.cr.usgs.gov/registers")
    }
  )

  api_key <- m2m_response_data(resp, "Login was unsuccessful")

  if (is.null(api_key)) {
    stop("Login was unsuccessful: no API key returned")
  }

  api_key
}


# Invalidate a session's API key via the logout endpoint.
m2m_logout <- function(session) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("logout") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  m2m_check_response(resp, "Logout failed")

  invisible(NULL)
}
