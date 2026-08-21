# Integration tests need real M2M credentials; skip cleanly without them.
skip_if_no_m2m <- function() {
  skip_on_cran()
  skip_if_offline("m2m.cr.usgs.gov")
  if (Sys.getenv("M2M_USERNAME") == "" || Sys.getenv("M2M_TOKEN") == "") {
    skip("M2M_USERNAME / M2M_TOKEN not set")
  }
}

# A session shared across integration tests, created once on first use so the
# suite doesn't log in repeatedly.
test_session <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- m2m_session()
    }
    cached
  }
})
