#' Authenticate with the Earth Explorer M2M interface using a username and
#' password to generate a temporary API key
#'
#' @param username ERS username. This is the same username that is used to log
#'   into Earth Explorer.
#' @param token Earth explorer M2M Application token. To generate a token, go to
#'   https://ers.cr.usgs.gov/. The token is different from the password that is
#'   used to log into Earth Explorer, and is more secure because the token is a
#'   64-bit encrypted string.
#'
#' @return class "ers_session" object containing the API key and service URL
#' @export
#' @examples
#' \dontrun{
#' # Login using environment variables
#' ers_session()
#'
#' # Login using function arguments
#' ers_session(username = "your_username", token = "your_token")
#' }
ers_session <- function(
  username = Sys.getenv("M2M_USERNAME"),
  token = Sys.getenv("M2M_TOKEN")
) {
  # Validate input parameters
  if (is.null(username) || !is.character(username) || nchar(username) == 0) {
    stop("Username cannot be NULL and must be a character string")
  }

  if (is.null(token) || !is.character(token) || nchar(token) == 0) {
    stop("Token cannot be NULL and must be a character string")
  }

  service_url <- "https://m2m.cr.usgs.gov/api/api/json/stable/"

  resp <- tryCatch(
    {
      httr2::request(service_url) %>%
        httr2::req_url_path_append("login-token") %>%
        httr2::req_body_json(list(username = username, token = token)) %>%
        httr2::req_perform()
    },
    error = function(e) {
      stop("Login was unsuccessful, check: https://ers.cr.usgs.gov/registers")
    }
  )

  if (resp$status_code == 200) {
    body <- httr2::resp_body_json(resp)
    api_key <- body$data

    if (is.null(api_key)) {
      detail <- if (!is.null(body$errorMessage)) {
        body$errorMessage
      } else if (!is.null(body$errorCode)) {
        body$errorCode
      } else {
        "unknown error"
      }
      stop(paste0("Login was unsuccessful: ", detail))
    }

    message("Login was successful")
  }

  session <- list(api_key = api_key, service = service_url)
  class(session) <- "ers_session"
  return(session)
}


#' Log out of the Earth Explorer M2M interface, invalidating the session's
#' API key
#'
#' M2M sessions also expire automatically after a period of inactivity, but
#' calling this when finished releases the session immediately rather than
#' waiting for that timeout.
#'
#' @param session An object of class "ers_session" returned by `ers_session`
#'
#' @return None. A message indicates whether logout succeeded.
#' @export
ers_logout <- function(session) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("logout") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  if (m2m_request_ok(resp, "Logout failed")) {
    message("Logout was successful")
  }

  return(invisible(NULL))
}
