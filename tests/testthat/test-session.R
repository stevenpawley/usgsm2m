test_that("m2m_session validates credentials before calling the API", {
  expect_error(m2m_session(username = "", token = "x"), "Username cannot be NULL")
  expect_error(m2m_session(username = "user", token = ""), "Token cannot be NULL")
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
