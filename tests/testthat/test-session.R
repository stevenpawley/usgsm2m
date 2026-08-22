test_that("m2m_session validates credentials before calling the API", {
  expect_error(m2m_session(username = "", token = "x"), "Username cannot be NULL")
  expect_error(m2m_session(username = "user", token = ""), "Token cannot be NULL")
})

test_that("the environment defaults live only on m2m_session()", {
  # $new() is not the documented entry point and no longer reads the
  # environment, so the defaults are defined in exactly one place
  expect_error(M2MSession$new(), "Username cannot be NULL")
  expect_error(M2MSession$new(username = "user"), "Token cannot be NULL")

  withr::with_envvar(
    c(M2M_USERNAME = "from_env", M2M_TOKEN = "also_from_env"),
    {
      captured <- with_captured_requests(
        mock_response(data = "an_api_key"),
        m2m_session()
      )
      body <- request_body(captured$requests[[1]])
      expect_equal(body$username, "from_env")
      expect_equal(body$token, "also_from_env")
    }
  )
})

test_that("m2m_session connects and exposes the pipeline entry points", {
  skip_if_no_m2m()

  sess <- test_session()

  expect_s3_class(sess, "M2MSession")
  expect_true(is.function(sess$dataset))
  expect_true(is.function(sess$find_datasets))
  expect_output(print(sess), "M2MSession")
  expect_output(print(sess), "\\$dataset\\(name\\)")
})

test_that("find_datasets returns matching datasets as a tibble", {
  skip_if_no_m2m()

  result <- test_session()$find_datasets(
    "landsat",
    spatial = filter_spatial(ll_lon = -120, ll_lat = 40, ur_lon = -119, ur_lat = 41),
    temporal = filter_temporal("2020-07-01", "2020-07-31")
  )

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true("datasetAlias" %in% names(result))
})

test_that("API failures raise a classed m2m_error rather than returning NULL", {
  skip_if_no_m2m()

  expect_error(
    test_session()$eula(code = "NOT_A_REAL_CODE"),
    class = "m2m_api_error"
  )
})

test_that("eula requires exactly one of code or codes", {
  skip_if_no_m2m()

  sess <- test_session()
  expect_error(sess$eula(), "eula_code or eula_codes")
  expect_error(sess$eula(code = "A", codes = c("A", "B")), "only one of")
})
