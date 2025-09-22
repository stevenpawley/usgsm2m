#' Remove items from an existing download order
#'
#' @param session An authenticated session object.
#' @param label The label of the download order to remove.
#'
#' @return None. Prints a message indicating success or failure.
#' @export
ers_download_remove_order <- function(session, label) {
  resp <- session$service %>%
    httr2::request() %>%
    httr2::req_url_path_append("download-order-remove") %>%
    httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
    httr2::req_body_json(data = list(label = "edmonton-data-2020")) %>%
    httr2::req_perform()

  if (resp$status_code == 200) {
    message("Order removed successfully")
  } else {
    message("Failed to remove order")
  }
}


#' Remove specific items from an existing download order
#'
#' @param session An authenticated session object.
#' @param downloadId A vector of download IDs to remove from the order.
#' @return None. Prints a message indicating success or failure.
#' @export
ers_download_remove_items <- function(session, downloadId) {
  for (id in downloadId) {
    session$service %>%
      httr2::request() %>%
      httr2::req_url_path_append("download-remove") %>%
      httr2::req_headers(`X-Auth-Token` = session$api_key) %>%
      httr2::req_body_json(data = list(downloadId = id)) %>%
      httr2::req_perform()
  }
}
