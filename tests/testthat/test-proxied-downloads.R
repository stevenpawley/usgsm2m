# Offline tests for the proxied-download path.
#
# The M2M API serves some downloads itself and hands others off to another
# USGS host on a signed URL, listing the latter under `requested` with a
# "Proxied" status. Those carry a working URL and are immediately fetchable,
# so readiness is a matter of having a URL rather than which bucket the API
# put a row in.

queue_rows <- function(..., status = "Proxied") {
  urls <- c(...)
  dplyr::tibble(
    downloadId = seq_along(urls) + 1000L,
    entityId = paste0("E", seq_along(urls)),
    statusText = status,
    url = urls
  )
}

# A queue object built without touching the network.
fake_queue <- function(available = dplyr::tibble(), requested = dplyr::tibble(),
                       queue_size = 0L) {
  q <- M2MDownloadQueue$new(
    session = NULL, label = "test", available = available, n_preparing = queue_size
  )
  q$requested <- requested
  q
}

test_that("rows with a URL count as ready wherever the API listed them", {
  q <- fake_queue(requested = queue_rows("https://landsatlook.usgs.gov/a?requestSignature=x"))

  expect_equal(nrow(q$ready()), 1)
  expect_equal(nrow(q$pending()), 0)
  expect_true(q$is_ready())
})

test_that("rows without a URL are pending, not ready", {
  q <- fake_queue(requested = queue_rows(NA_character_, status = "Staging"))

  expect_equal(nrow(q$ready()), 0)
  expect_equal(nrow(q$pending()), 1)
  expect_false(q$is_ready())
})

test_that("a mixed order reports both", {
  q <- fake_queue(
    available = queue_rows("https://m2m.cr.usgs.gov/f1", status = "Available"),
    requested = queue_rows(NA_character_, status = "Staging")
  )

  expect_equal(nrow(q$ready()), 1)
  expect_equal(nrow(q$pending()), 1)
  expect_false(q$is_ready())
})

test_that("the auth token goes only to the M2M host", {
  session <- mock_session()
  dir <- withr::local_tempdir()

  downloads <- dplyr::tibble(
    entityId = c("m2m_file", "proxied_file"),
    downloadId = c(1L, 2L),
    url = c(
      "https://m2m.cr.usgs.gov/api/api/json/stable/download/1",
      "https://landsatlook.usgs.gov/data/x.tif?requestSignature=abc"
    )
  )

  captured <- with_captured_requests(
    list(
      httr2::response(status_code = 200L, body = charToRaw("aa")),
      httr2::response(status_code = 200L, body = charToRaw("bb"))
    ),
    ers_download_files(session, downloads = downloads, out_dir = dir)
  )

  m2m_headers <- names(captured$requests[[1]]$headers)
  proxied_headers <- names(captured$requests[[2]]$headers)

  expect_true("X-Auth-Token" %in% m2m_headers)
  # a signed URL is self-authenticating; the token must not be sent there
  expect_false("X-Auth-Token" %in% proxied_headers)
})

test_that("an expired signed URL is reported as expired, not just failed", {
  session <- mock_session()
  dir <- withr::local_tempdir()

  downloads <- dplyr::tibble(
    entityId = "gone",
    downloadId = 1L,
    url = "https://landsatlook.usgs.gov/data/x.tif?requestSignature=stale"
  )

  captured <- with_captured_requests(
    list(httr2::response(status_code = 403L, body = charToRaw(""))),
    ers_download_files(session, downloads = downloads, out_dir = dir)
  )

  expect_equal(captured$result$status, "expired")
  expect_true(is.na(captured$result$path))
})

test_that("proxied completions are reported with id and downloaded size", {
  downloads <- dplyr::tibble(
    downloadId = c(11L, 12L),
    size = c(100, 250)
  )

  captured <- with_captured_requests(
    mock_response(data = list()),
    ers_download_complete_proxied(mock_session(), downloads)
  )

  body <- request_body(captured$requests[[1]])
  expect_length(body$proxiedDownloads, 2)
  expect_named(body$proxiedDownloads[[1]], c("downloadId", "downloadedSize"))
  expect_equal(body$proxiedDownloads[[1]]$downloadId, 11L)
  expect_equal(body$proxiedDownloads[[2]]$downloadedSize, 250)
})

test_that("reporting nothing makes no request", {
  captured <- with_captured_requests(
    mock_response(data = list()),
    ers_download_complete_proxied(
      mock_session(),
      dplyr::tibble(downloadId = integer(), size = numeric())
    )
  )

  expect_equal(captured$n, 0)
})
