#' Connect to the USGS/EROS M2M API
#'
#' Authenticates against the M2M API and returns a session object, which is
#' the entry point for everything else in this package. From a session you
#' reach a dataset, from a dataset a scene search, and so on - each step
#' returns an object whose methods are the valid next steps, so the download
#' pipeline is discoverable by tab-completing `$` in the console.
#'
#' The usual pipeline is:
#'
#' ```
#' sess <- m2m_session()
#' queue <- sess$
#'   dataset("landsat_ot_c2_l2")$
#'   search(spatial = filter_spatial(-120, 40, -119, 41),
#'          temporal = filter_temporal("2020-07-01", "2020-07-31"))$
#'   products()$
#'   select_bands(c("B4", "B5"))$
#'   request(label = "my_order")
#'
#' queue$retrieve("data/")
#' ```
#'
#' @param username ERS username. This is the same username that is used to log
#'   into Earth Explorer. Defaults to the `M2M_USERNAME` environment variable.
#' @param token Earth Explorer M2M application token. To generate a token, go
#'   to https://ers.cr.usgs.gov/. The token is different from the password
#'   used to log into Earth Explorer. Defaults to the `M2M_TOKEN`
#'   environment variable.
#'
#' @return An [M2MSession] object.
#' @export
#' @examples
#' \dontrun{
#' # Login using environment variables
#' sess <- m2m_session()
#'
#' # Login using arguments
#' sess <- m2m_session(username = "your_username", token = "your_token")
#' }
m2m_session <- function(
  username = Sys.getenv("M2M_USERNAME"),
  token = Sys.getenv("M2M_TOKEN")
) {
  M2MSession$new(username = username, token = token)
}


#' M2M API session
#'
#' Represents an authenticated connection to the USGS/EROS M2M API. Create
#' one with [m2m_session()] rather than calling `M2MSession$new()` directly.
#'
#' @export
M2MSession <- R6::R6Class(
  "M2MSession",
  public = list(
    #' @description Authenticate and open a session. Both arguments are
    #'   required; [m2m_session()] is the documented way in and is what
    #'   supplies the `M2M_USERNAME` / `M2M_TOKEN` defaults.
    #' @param username ERS username.
    #' @param token Earth Explorer M2M application token.
    initialize = function(username, token) {
      if (missing(username) || is.null(username) || !is.character(username) ||
        nchar(username) == 0) {
        stop("Username cannot be NULL and must be a character string")
      }

      if (missing(token) || is.null(token) || !is.character(token) ||
        nchar(token) == 0) {
        stop("Token cannot be NULL and must be a character string")
      }

      private$service_url <- "https://m2m.cr.usgs.gov/api/api/json/stable/"
      private$api_key <- private$login(username, token)
      private$username_ <- username
      invisible(self)
    },

    #' @description Look up a dataset by alias or id. This is the usual
    #'   starting point for a search.
    #' @param name The system-friendly dataset alias, e.g.
    #'   `"landsat_ot_c2_l2"`. Use this or `id`, not both.
    #' @param id The dataset identifier. Use this or `name`, not both.
    #' @return An [M2MDataset] object.
    dataset = function(name = NULL, id = NULL) {
      info <- ers_dataset(
        private$session(),
        dataset_id = id,
        dataset_name = name
      )
      M2MDataset$new(session = self, info = info)
    },

    #' @description Search the catalog for datasets matching a name pattern.
    #'   Use this when you don't yet know a dataset's alias; wildcards are
    #'   applied automatically around `pattern`.
    #' @param pattern Search pattern for the dataset name.
    #' @param spatial Optional spatial filter, see [filter_spatial()].
    #' @param temporal Optional temporal filter, see [filter_temporal()].
    #' @return A tibble of matching datasets. Pass a `datasetAlias` value from
    #'   the result to `$dataset()` to continue.
    find_datasets = function(pattern, spatial = NULL, temporal = NULL) {
      ers_dataset_search(
        private$session(),
        dataset_name = pattern,
        spatial_filter = spatial,
        temporal_filter = temporal
      )
    },

    #' @description Reconnect to an existing download order by its label,
    #'   for example to resume collecting a large order in a later R session.
    #' @param label The label the downloads were requested under.
    #' @return An [M2MDownloadQueue] object.
    download_queue = function(label) {
      M2MDownloadQueue$new(session = self, label = label)
    },

    #' @description List every download in the queue, regardless of status
    #'   or label.
    #' @return A tibble of queued downloads.
    downloads = function() {
      ers_download_search(private$session())
    },

    #' @description Remove individual downloads from the queue, across any
    #'   label. Use this to clear out completed or unwanted entries without
    #'   dropping a whole order the way `M2MDownloadQueue$cancel()` does.
    #'
    #'   Ids come from the `downloadId` column of `$downloads()`:
    #'
    #'   ```
    #'   done <- dplyr::filter(sess$downloads(), statusText == "Complete")
    #'   sess$remove_items(done$downloadId)
    #'   ```
    #'
    #'   The API removes one item per request and answers the same way for an
    #'   id that does not exist, so a successful call is not evidence that
    #'   anything was removed - check `$downloads()` afterwards.
    #' @param download_id A vector of download ids.
    #' @param quiet Suppress the message shown before a large batch.
    #' @return The number of ids submitted, invisibly.
    remove_items = function(download_id, quiet = FALSE) {
      ers_download_remove_items(private$session(), download_id, quiet = quiet)
    },

    #' @description List the distinct order labels in the download queue, one
    #'   row each. Use this to find an order to reconnect to with
    #'   `$download_queue()` when you no longer remember its label.
    #' @param download_application Optional application name to scope the
    #'   listing to.
    #' @return A tibble with `label`, `downloadCount`, `totalComplete`,
    #'   `downloadSize` and `dateEntered` (epoch milliseconds).
    download_labels = function(download_application = NULL) {
      ers_download_labels(private$session(), download_application)
    },

    #' @description Retrieve the text of one or more End User License
    #'   Agreements. Some datasets require a EULA to be accepted (once,
    #'   through the EarthExplorer website) before downloads will succeed.
    #' @param code A single EULA code. Use this or `codes`, not both.
    #' @param codes A character vector of EULA codes. Use this or `code`.
    #' @return A tibble with `eulaCode` and `agreementContent`.
    eula = function(code = NULL, codes = NULL) {
      ers_download_eula(private$session(), eula_code = code, eula_codes = codes)
    },

    #' @description Attach to a scene list that already exists server-side.
    #' @param list_id The scene list identifier.
    #' @param dataset_name The dataset alias the list belongs to. Optional -
    #'   it is looked up from the list's summary when needed. Supply it to
    #'   save that lookup, or to pick one when the list spans several
    #'   datasets.
    #' @return An [M2MSceneList] object.
    scene_list = function(list_id, dataset_name = NULL) {
      M2MSceneList$new(
        session = self,
        list_id = list_id,
        dataset_name = dataset_name %||% NA_character_
      )
    },

    #' @description End the session, invalidating its API key. M2M sessions
    #'   also expire on their own after a period of inactivity.
    #' @return The session, invisibly.
    logout = function() {
      # $session() refuses if this session was already logged out, so a second
      # call errors rather than quietly re-sending a dead key.
      session <- private$session()

      resp <- session$service %>%
        httr2::request() %>%
        httr2::req_url_path_append("logout") %>%
        httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
        httr2::req_error(is_error = function(resp) FALSE) %>%
        httr2::req_perform()

      m2m_check_response(resp, "Logout failed")

      private$api_key <- NULL
      invisible(self)
    },

    #' @description Print a summary of the session.
    #' @param ... Ignored.
    print = function(...) {
      cat("<M2MSession>\n")
      cat("  User:    ", private$username_, "\n", sep = "")
      cat("  Status:  ", if (is.null(private$api_key)) "logged out" else "connected", "\n", sep = "")
      if (!is.null(private$api_key)) {
        m2m_print_next("$dataset(name)", "$find_datasets(pattern)")
      }
      invisible(self)
    }
  ),
  private = list(
    api_key = NULL,
    service_url = NULL,
    username_ = NULL,

    # Exchange credentials for an API key via the login-token endpoint.
    #
    # This is the one endpoint that does not take a session, since it is what
    # produces one - so it lives here rather than alongside the ers_* transport
    # functions, whose shared signature it could not follow anyway.
    login = function(username, token) {
      resp <- tryCatch(
        {
          httr2::request(private$service_url) %>%
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
    },

    # The internal endpoint functions take a plain list session; build one
    # on demand rather than storing a second copy of the credentials.
    session = function() {
      if (is.null(private$api_key)) {
        stop("This session has been logged out; create a new one with m2m_session()")
      }
      m2m_legacy_session(private$api_key, private$service_url)
    }
  )
)


# Internal accessor so sibling R6 classes can borrow a session's credentials
# without exposing the API key on the public interface.
m2m_session_handle <- function(session) {
  session$.__enclos_env__$private$session()
}
