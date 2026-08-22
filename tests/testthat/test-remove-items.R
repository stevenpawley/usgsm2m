# Offline tests for selective queue removal.
#
# The download-remove endpoint takes one downloadId per call - an array under
# the same key is rejected and there is no plural form - so these check that
# one request is made per id and that the payload is a scalar.

test_that("one request is made per download id", {
  captured <- with_captured_requests(
    list(mock_response(), mock_response(), mock_response()),
    ers_download_remove_items(mock_session(), c(1L, 2L, 3L))
  )

  expect_equal(captured$n, 3)
  expect_equal(captured$result, 3L)

  ids <- vapply(captured$requests, function(r) request_body(r)$downloadId, integer(1))
  expect_equal(ids, c(1L, 2L, 3L))
})

test_that("downloadId is sent as a scalar, not an array", {
  captured <- with_captured_requests(
    list(mock_response()),
    ers_download_remove_items(mock_session(), 42L)
  )

  # an array under this key is rejected with INPUT_PARAMETER_INVALID
  body <- request_body(captured$requests[[1]])
  expect_length(body$downloadId, 1)
  expect_false(is.list(body$downloadId))
})

test_that("duplicate and missing ids are dropped before sending", {
  captured <- with_captured_requests(
    list(mock_response(), mock_response()),
    ers_download_remove_items(mock_session(), c(7L, 7L, NA_integer_, 8L))
  )

  expect_equal(captured$n, 2)
  ids <- vapply(captured$requests, function(r) request_body(r)$downloadId, integer(1))
  expect_equal(ids, c(7L, 8L))
})

test_that("removing nothing makes no request", {
  for (empty in list(integer(), NA_integer_, NULL)) {
    captured <- with_captured_requests(
      mock_response(),
      ers_download_remove_items(mock_session(), empty)
    )
    expect_equal(captured$n, 0)
    expect_equal(captured$result, 0L)
  }
})

test_that("a large batch says how many items it is removing", {
  responses <- rep(list(mock_response()), 30)

  expect_message(
    with_captured_requests(
      responses,
      ers_download_remove_items(mock_session(), seq_len(30))
    ),
    "Removing 30 items"
  )
})

test_that("a large batch can be silenced", {
  responses <- rep(list(mock_response()), 30)

  expect_no_message(
    with_captured_requests(
      responses,
      ers_download_remove_items(mock_session(), seq_len(30), quiet = TRUE)
    )
  )
})

test_that("a failure on one id is raised rather than swallowed", {
  expect_error(
    with_captured_requests(
      list(mock_response(), mock_response(error_code = "INPUT_PARAMETER_INVALID")),
      ers_download_remove_items(mock_session(), c(1L, 2L))
    ),
    class = "m2m_api_error"
  )
})

test_that("the queue exposes removal scoped to its own order", {
  q <- M2MDownloadQueue$new(
    session = NULL, label = "test", available = dplyr::tibble(), n_preparing = 0L
  )

  expect_true(is.function(q$remove_items))
  # distinct from cancel(), which drops the whole order
  expect_true(is.function(q$cancel))
})
