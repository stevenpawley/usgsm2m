mock_session <- function() {
  structure(
    list(
      api_key = "mock_api_key",
      service = "https://m2m.cr.usgs.gov/api/api/json/stable/"
    ),
    class = "ers_session"
  )
}
