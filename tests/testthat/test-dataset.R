test_that("dataset() looks up a dataset and exposes its identifiers", {
  skip_if_no_m2m()

  ds <- test_session()$dataset("landsat_ot_c2_l2")

  expect_s3_class(ds, "M2MDataset")
  expect_equal(ds$alias(), "landsat_ot_c2_l2")
  expect_type(ds$id(), "character")
  expect_s3_class(ds$info, "tbl_df")
  expect_equal(nrow(ds$info), 1)
})

test_that("dataset() requires exactly one of name or id", {
  skip_if_no_m2m()

  sess <- test_session()
  expect_error(sess$dataset(), "dataset_id or dataset_name")
  expect_error(
    sess$dataset(name = "landsat_ot_c2_l2", id = "123"),
    "only one of"
  )
})

test_that("dataset print shows the next pipeline step", {
  skip_if_no_m2m()

  ds <- test_session()$dataset("landsat_ot_c2_l2")

  expect_output(print(ds), "M2MDataset")
  expect_output(print(ds), "landsat_ot_c2_l2")
  expect_output(print(ds), "\\$search")
})

test_that("filters() returns metadata filter fields usable as filterIds", {
  skip_if_no_m2m()

  filters <- test_session()$dataset("landsat_ot_c2_l2")$filters()

  expect_s3_class(filters, "tbl_df")
  expect_gt(nrow(filters), 0)
  expect_true(all(c("id", "fieldLabel", "fieldConfig") %in% names(filters)))

  # The id column feeds directly into filter_metadata_value()
  built <- filter_metadata_value(filters$id[[1]], "045")
  expect_equal(built$filterId, filters$id[[1]])
})

test_that("filters() returns one row per field, not one per option", {
  skip_if_no_m2m()

  filters <- test_session()$dataset("landsat_ot_c2_l2")$filters()

  # Select fields carry a valueList of allowed options; it must stay a
  # list-column rather than recycling its field across several rows.
  expect_equal(nrow(filters), nrow(dplyr::distinct(filters, id)))
  expect_true("valueList" %in% names(filters))
  expect_type(filters$valueList, "list")
})

test_that("find_datasets returns one row per dataset", {
  skip_if_no_m2m()

  # landsat_ot_c2_l1 belongs to two catalogs; that must not duplicate its row.
  found <- test_session()$find_datasets("landsat_ot")

  expect_equal(nrow(found), nrow(dplyr::distinct(found, datasetAlias)))
  expect_type(found$catalogs, "list")
})

test_that("a missing dataset and an inaccessible one are told apart", {
  skip_if_no_m2m()

  sess <- test_session()

  # not in the catalogue at all
  expect_error(sess$dataset("definitely_not_a_dataset"), class = "m2m_not_found")

  # exists, but this account has no access - the API answers DATASET_AUTH.
  # find_datasets() omits exactly these, which is what the error points at.
  expect_error(sess$dataset("modis_mod09a1_v61"), class = "m2m_no_access")
  expect_false("modis_mod09a1_v61" %in% sess$find_datasets("modis")$datasetAlias)
})
