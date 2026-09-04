# Offline tests for not re-downloading files that are already in out_dir.

# Compare file contents without assuming a trailing newline: a downloaded body
# is written verbatim.
file_text <- function(path) readChar(path, file.size(path), useBytes = TRUE)

test_that("a file named by the URL is skipped without a request", {
  session <- mock_session()
  dir <- withr::local_tempdir()
  writeLines("already here", file.path(dir, "LE07_SR_B1.TIF"))

  downloads <- dplyr::tibble(
    entityId = "LE70420242009182_SR_B1_TIF",
    downloadId = 1L,
    url = "https://landsatlook.usgs.gov/data/LE07_SR_B1.TIF?requestSignature=abc"
  )

  captured <- with_captured_requests(
    list(),
    api_download_files(session, downloads = downloads, out_dir = dir)
  )

  expect_equal(captured$n, 0L)
  expect_equal(captured$result$status, "skipped")
  expect_equal(basename(captured$result$path), "LE07_SR_B1.TIF")
  expect_equal(file_text(file.path(dir, "LE07_SR_B1.TIF")), "already here\n")
})

test_that("a file named by the response headers is skipped once they arrive", {
  session <- mock_session()
  dir <- withr::local_tempdir()
  writeLines("already here", file.path(dir, "LE07_L2SP_042024_02_T1.tar"))

  downloads <- dplyr::tibble(
    entityId = "LE70420242009182",
    downloadId = 1L,
    url = "https://m2m.cr.usgs.gov/api/api/json/stable/download/1"
  )

  captured <- with_captured_requests(
    list(httr2::response(
      200L,
      headers = list(`Content-Disposition` = 'attachment; filename="LE07_L2SP_042024_02_T1.tar"'),
      body = charToRaw("fresh")
    )),
    api_download_files(session, downloads = downloads, out_dir = dir)
  )

  expect_equal(captured$result$status, "skipped")
  # the body is never written, so the copy on disk is untouched
  expect_equal(file_text(file.path(dir, "LE07_L2SP_042024_02_T1.tar")), "already here\n")
  expect_equal(list.files(dir), "LE07_L2SP_042024_02_T1.tar")
})

test_that("overwrite = TRUE downloads over an existing file", {
  session <- mock_session()
  dir <- withr::local_tempdir()
  writeLines("stale", file.path(dir, "LE07_L2SP_042024_02_T1.tar"))

  downloads <- dplyr::tibble(
    entityId = "LE70420242009182",
    downloadId = 1L,
    url = "https://m2m.cr.usgs.gov/api/api/json/stable/download/1"
  )

  captured <- with_captured_requests(
    list(httr2::response(
      200L,
      headers = list(`Content-Disposition` = 'attachment; filename="LE07_L2SP_042024_02_T1.tar"'),
      body = charToRaw("fresh")
    )),
    api_download_files(session, downloads = downloads, out_dir = dir, overwrite = TRUE)
  )

  expect_equal(captured$result$status, "downloaded")
  expect_equal(file_text(file.path(dir, "LE07_L2SP_042024_02_T1.tar")), "fresh")
  expect_equal(list.files(dir), "LE07_L2SP_042024_02_T1.tar")
})

test_that("a missing file is still downloaded when others are skipped", {
  session <- mock_session()
  dir <- withr::local_tempdir()
  writeLines("already here", file.path(dir, "one.tar"))

  downloads <- dplyr::tibble(
    entityId = c("E1", "E2"),
    downloadId = c(1L, 2L),
    url = c(
      "https://dds.cr.usgs.gov/bundle/one.tar",
      "https://dds.cr.usgs.gov/bundle/two.tar"
    )
  )

  captured <- with_captured_requests(
    list(httr2::response(
      200L,
      url = "https://dds.cr.usgs.gov/bundle/two.tar",
      body = charToRaw("two")
    )),
    api_download_files(session, downloads = downloads, out_dir = dir)
  )

  # only the missing file is requested
  expect_equal(captured$n, 1L)
  expect_equal(captured$result$status, c("skipped", "downloaded"))
  expect_setequal(list.files(dir), c("one.tar", "two.tar"))
})
