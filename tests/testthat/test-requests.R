# Offline tests for what the package sends and how it handles what comes back.
#
# Tier 1 (test-coercion-unit.R) tests the coercion functions on parsed
# payloads. These cover the parts either side of that: the request bodies
# constructed for the API, pagination, and error handling. Both run with no
# credentials and no network. See helper-mock-http.R.

# --- request construction ----------------------------------------------------

test_that("entityIds is sent as an array even for a single scene", {
  # httr2 serializes with auto_unbox = TRUE, so a length-1 character vector
  # became a bare string rather than a JSON array and the API answered
  # HTTP 500 for every single-scene list.
  captured <- with_captured_requests(
    mock_response(data = 1L),
    api_scene_list_add(
      mock_session(),
      dataset_name = "landsat_ot_c2_l2",
      scenes = data.frame(entityId = "ONLY_ONE"),
      list_id = "test"
    )
  )

  body <- request_body(captured$requests[[1]])

  # a list serializes to [...]; a bare character vector of length 1 does not
  expect_type(body$entityIds, "list")
  expect_length(body$entityIds, 1)
  expect_equal(body$entityIds[[1]], "ONLY_ONE")
  expect_equal(body$datasetName, "landsat_ot_c2_l2")
  expect_equal(body$idField, "entityId")
})

test_that("several entityIds are still sent as an array", {
  captured <- with_captured_requests(
    mock_response(data = 3L),
    api_scene_list_add(
      mock_session(),
      dataset_name = "landsat_ot_c2_l2",
      scenes = data.frame(entityId = c("A", "B", "C")),
      list_id = "test"
    )
  )

  expect_type(request_body(captured$requests[[1]])$entityIds, "list")
  expect_length(request_body(captured$requests[[1]])$entityIds, 3)
})

test_that("a single eulaCode is sent as an array", {
  # same auto-unbox trap as entityIds
  captured <- with_captured_requests(
    mock_response(data = list(list(eulaCode = "X", agreementContent = "text"))),
    api_download_eula(mock_session(), eula_codes = "SINGLE")
  )

  body <- request_body(captured$requests[[1]])
  expect_type(body$eulaCodes, "list")
  expect_length(body$eulaCodes, 1)
})

test_that("every authenticated request carries the auth header", {
  captured <- with_captured_requests(
    mock_response(data = list()),
    api_download_search(mock_session())
  )

  headers <- captured$requests[[1]]$headers
  expect_true("X-Auth-Token" %in% names(headers))
  expect_equal(as.character(headers[["X-Auth-Token"]]), "test_api_key")
})

test_that("download requests send entityId and productId pairs", {
  downloads <- dplyr::tibble(
    entityId = c("E1", "E2"),
    id = c("P1", "P2")
  )

  captured <- with_captured_requests(
    mock_response(data = list(
      availableDownloads = list(), newRecords = list(), duplicateProducts = list()
    )),
    api_download_request(mock_session(), downloads = downloads, label = "lbl")
  )

  body <- request_body(captured$requests[[1]])
  expect_equal(body$label, "lbl")
  expect_length(body$downloads, 2)
  expect_named(body$downloads[[1]], c("entityId", "productId"))
  expect_equal(body$downloads[[1]]$entityId, "E1")
  expect_equal(body$downloads[[1]]$productId, "P1")
})

test_that("scene search sends only the filters it was given", {
  captured <- with_captured_requests(
    mock_response(data = mock_search_page(list(), 0L)),
    api_scene_search(
      mock_session(),
      dataset_name = "landsat_ot_c2_l2",
      temporal_filter = filter_temporal("2020-07-01", "2020-07-31")
    )
  )

  scene_filter <- request_body(captured$requests[[1]])$sceneFilter

  # NULL filters are dropped rather than sent as nulls
  expect_named(scene_filter, "acquisitionFilter")
  expect_equal(scene_filter$acquisitionFilter$start, "2020-07-01")
})

# --- pagination --------------------------------------------------------------

test_that("scene search follows nextRecord across pages", {
  page1 <- mock_response(data = mock_search_page(
    list(fake_scene(1), fake_scene(2)), total_hits = 3L, next_record = 3L
  ))
  page2 <- mock_response(data = mock_search_page(
    list(fake_scene(3)), total_hits = 3L, next_record = NULL
  ))

  captured <- with_captured_requests(
    list(page1, page2),
    api_scene_search(mock_session(), dataset_name = "landsat_ot_c2_l2")
  )

  expect_equal(captured$n, 2)
  expect_equal(nrow(captured$result$results), 3)
  expect_equal(captured$result$total_hits, 3)
  expect_equal(captured$result$results$entityId, c("E1", "E2", "E3"))

  # the second request must resume where the first left off
  expect_equal(request_body(captured$requests[[2]])$startingNumber, 3L)
})

test_that("scene search stops when a page reports no nextRecord", {
  captured <- with_captured_requests(
    list(mock_response(data = mock_search_page(
      list(fake_scene(1)), total_hits = 99L, next_record = NULL
    ))),
    api_scene_search(mock_session(), dataset_name = "landsat_ot_c2_l2")
  )

  # totalHits says more exist, but without a nextRecord there is nowhere to go
  expect_equal(captured$n, 1)
  expect_equal(nrow(captured$result$results), 1)
})

test_that("max_results caps the scenes returned", {
  captured <- with_captured_requests(
    list(mock_response(data = mock_search_page(
      list(fake_scene(1), fake_scene(2), fake_scene(3)),
      total_hits = 10L, next_record = 4L
    ))),
    api_scene_search(
      mock_session(),
      dataset_name = "landsat_ot_c2_l2",
      max_results = 2
    )
  )

  expect_equal(nrow(captured$result$results), 2)
  expect_equal(request_body(captured$requests[[1]])$maxResults, 2)
})

test_that("an empty search returns an empty tibble, not an error", {
  captured <- with_captured_requests(
    mock_response(data = mock_search_page(list(), 0L)),
    api_scene_search(mock_session(), dataset_name = "landsat_ot_c2_l2")
  )

  expect_equal(nrow(captured$result$results), 0)
  expect_equal(captured$result$total_hits, 0)
})

# --- error handling ----------------------------------------------------------

test_that("an HTTP error raises a classed m2m_http_error", {
  expect_error(
    with_captured_requests(
      mock_response(status = 500L),
      api_download_search(mock_session())
    ),
    class = "m2m_http_error"
  )
})

test_that("an errorCode in a 200 body raises a classed m2m_api_error", {
  # the API signals most failures with HTTP 200 and an errorCode, so a 200
  # alone does not mean success
  expect_error(
    with_captured_requests(
      mock_response(error_code = "AUTH_INVALID", error_message = "bad token"),
      api_download_search(mock_session())
    ),
    class = "m2m_api_error"
  )
})

test_that("the error message carries the API's own explanation", {
  err <- tryCatch(
    with_captured_requests(
      mock_response(error_code = "RATE_LIMIT", error_message = "too many requests"),
      api_download_search(mock_session())
    ),
    m2m_error = function(e) e
  )

  expect_s3_class(err, "m2m_api_error")
  expect_match(conditionMessage(err), "too many requests")
})

test_that("both error classes inherit from m2m_error", {
  for (resp in list(mock_response(status = 503L), mock_response(error_code = "X"))) {
    err <- tryCatch(
      with_captured_requests(resp, api_download_search(mock_session())),
      error = function(e) e
    )
    expect_s3_class(err, "m2m_error")
  }
})

test_that("an authorisation failure is distinguished from a missing dataset", {
  # DATASET_AUTH means the dataset exists but the account cannot reach it.
  # Reported with the caller's context it used to read "Dataset not found:
  # Dataset status is unavailable to this user", which sends people hunting
  # for a typo.
  err <- tryCatch(
    with_captured_requests(
      mock_response(
        error_code = "DATASET_AUTH",
        error_message = "Dataset status is unavailable to this user"
      ),
      api_dataset(mock_session(), dataset_name = "modis_mod09a1_v61")
    ),
    m2m_error = function(e) e
  )

  expect_s3_class(err, "m2m_no_access")
  expect_no_match(conditionMessage(err), "not found")
  expect_match(conditionMessage(err), "No access")
  expect_match(conditionMessage(err), "find_datasets")
})

test_that("m2m_no_access still inherits the general error classes", {
  # so existing tryCatch(m2m_api_error = ) handlers keep working
  err <- tryCatch(
    with_captured_requests(
      mock_response(error_code = "DATASET_AUTH", error_message = "nope"),
      api_dataset(mock_session(), dataset_name = "x")
    ),
    error = function(e) e
  )

  expect_s3_class(err, "m2m_no_access")
  expect_s3_class(err, "m2m_api_error")
  expect_s3_class(err, "m2m_error")
})

test_that("other error codes are not treated as access failures", {
  err <- tryCatch(
    with_captured_requests(
      mock_response(error_code = "RATE_LIMIT", error_message = "slow down"),
      api_dataset(mock_session(), dataset_name = "x")
    ),
    error = function(e) e
  )

  expect_s3_class(err, "m2m_api_error")
  expect_false(inherits(err, "m2m_no_access"))
})

test_that("an unknown dataset is reported as not found", {
  # the API answers HTTP 200 with a null payload and no errorCode
  expect_error(
    with_captured_requests(
      mock_response(data = NULL),
      api_dataset(mock_session(), dataset_name = "nope")
    ),
    class = "m2m_not_found"
  )
})

# --- authentication ----------------------------------------------------------

test_that("logging in sends the credentials and keeps the key it gets back", {
  captured <- with_captured_requests(
    mock_response(data = "an_api_key"),
    m2m_session(username = "user", token = "token")
  )

  body <- request_body(captured$requests[[1]])
  expect_equal(body$username, "user")
  expect_equal(body$token, "token")

  expect_s3_class(captured$result, "M2MSession")
  # the key is private, and exposed to sibling classes as an ERS session
  expect_equal(captured$result$ers_session()$api_key, "an_api_key")
})

test_that("a rejected login raises rather than returning a broken session", {
  expect_error(
    with_captured_requests(
      mock_response(error_code = "AUTH_INVALID", error_message = "bad token"),
      m2m_session(username = "user", token = "token")
    ),
    class = "m2m_api_error"
  )
})

test_that("a logged-out session refuses to make further requests", {
  session <- with_captured_requests(
    list(mock_response(data = "an_api_key"), mock_response(data = list())),
    {
      sess <- m2m_session(username = "user", token = "token")
      sess$logout()
      sess
    }
  )$result

  expect_error(session$downloads(), "logged out")
  expect_output(print(session), "logged out")
})
