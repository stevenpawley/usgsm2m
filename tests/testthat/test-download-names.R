# Offline tests for how downloaded files are named.
#
# The queue identifies files by entityId, which for a scene-level product is
# the scene id with no extension, so the name has to come from the server.

test_that("a Content-Disposition filename is used", {
  resp <- httr2::response(
    200L,
    url = "https://m2m.cr.usgs.gov/api/api/json/stable/download/1",
    headers = list(
      `Content-Disposition` = 'attachment; filename="LE07_L2SP_042024_20090701_02_T1.tar"'
    )
  )

  expect_equal(
    coerce_download_name(resp, url = "https://m2m.cr.usgs.gov/download/1", entityId = "LE70420242009182"),
    "LE07_L2SP_042024_20090701_02_T1.tar"
  )
})

test_that("an RFC 5987 filename* is decoded and preferred", {
  resp <- httr2::response(
    200L,
    headers = list(
      `Content-Disposition` = "attachment; filename=fallback.bin; filename*=UTF-8''LE07%20bundle.tar.gz"
    )
  )

  expect_equal(coerce_download_name(resp, url = "https://x/y", entityId = "E1"), "LE07 bundle.tar.gz")
})

test_that("a path in the header cannot escape the output directory", {
  resp <- httr2::response(
    200L,
    headers = list(`Content-Disposition` = 'attachment; filename="../../etc/passwd"')
  )

  expect_equal(coerce_download_name(resp, url = "https://x/y", entityId = "E1"), "passwd")
})

test_that("the served URL supplies the name when there is no header", {
  resp <- httr2::response(
    200L,
    url = "https://landsatlook.usgs.gov/data/LE07_SR_B1.TIF?requestSignature=abc"
  )

  expect_equal(coerce_download_name(resp, url = "https://x/y", entityId = "E1"), "LE07_SR_B1.TIF")
})

test_that("a URL with no filename in it is not used", {
  expect_null(coerce_url_name("https://m2m.cr.usgs.gov/api/api/json/stable/download/12345"))
  expect_null(coerce_url_name("https://landsatlook.usgs.gov/gen-bundle?id=LE07"))
  # the host on its own must not be read as a filename
  expect_null(coerce_url_name("https://landsatlook.usgs.gov"))
})

test_that("the entityId is the last resort, with a band suffix turned into an extension", {
  resp <- httr2::response(200L, url = "https://m2m.cr.usgs.gov/download/1")

  expect_equal(
    coerce_download_name(resp, url = "https://m2m.cr.usgs.gov/download/1", entityId = "LE70420242009182_SR_B1_TIF"),
    "LE70420242009182_SR_B1.TIF"
  )
  expect_equal(
    coerce_download_name(resp, url = "https://m2m.cr.usgs.gov/download/1", entityId = "LE70420242009182"),
    "LE70420242009182"
  )
})

test_that("a downloaded bundle is written under its server-supplied name", {
  session <- mock_session()
  dir <- withr::local_tempdir()

  downloads <- dplyr::tibble(
    entityId = "LE70420242009182",
    downloadId = 1L,
    url = "https://m2m.cr.usgs.gov/api/api/json/stable/download/1"
  )

  captured <- with_captured_requests(
    list(httr2::response(
      200L,
      headers = list(`Content-Disposition` = 'attachment; filename="LE07_L2SP_042024_02_T1.tar"'),
      body = charToRaw("data")
    )),
    api_download_files(session, downloads = downloads, out_dir = dir)
  )

  expect_equal(basename(captured$result$path), "LE07_L2SP_042024_02_T1.tar")
  expect_equal(dirname(captured$result$path), dir)
})

test_that("a failed download leaves no partial file behind", {
  session <- mock_session()
  dir <- withr::local_tempdir()

  downloads <- dplyr::tibble(
    entityId = "gone",
    downloadId = 1L,
    url = "https://landsatlook.usgs.gov/data/x.tif?requestSignature=stale"
  )

  captured <- with_captured_requests(
    list(httr2::response(403L, body = charToRaw("expired"))),
    api_download_files(session, downloads = downloads, out_dir = dir)
  )

  expect_equal(captured$result$status, "expired")
  expect_equal(list.files(dir), character(0))
})
