test_that("ers_dataset requires exactly one of dataset_id or dataset_name", {
  session <- mock_session()

  expect_error(ers_dataset(session), "dataset_id or dataset_name")
  expect_error(
    ers_dataset(session, dataset_id = "123", dataset_name = "landsat_ot_c2_l2"),
    "only one of"
  )
})

test_that("ers_dataset looks up a dataset by name", {
  skip_on_cran()
  skip_if_offline("m2m.cr.usgs.gov")

  session <- ers_session()
  result <- ers_dataset(session, dataset_name = "landsat_ot_c2_l2")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_true("datasetId" %in% names(result))
})

test_that("ers_dataset_filters returns filter fields for a dataset", {
  skip_on_cran()
  skip_if_offline("m2m.cr.usgs.gov")

  session <- ers_session()
  result <- ers_dataset_filters(session, dataset_name = "landsat_ot_c2_l2")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "fieldLabel", "fieldConfig") %in% names(result)))
})
