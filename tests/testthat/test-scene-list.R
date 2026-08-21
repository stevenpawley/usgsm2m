# A scene list created from a search, shared across the tests below so the
# suite registers one list rather than one per test.
list_fixture <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- test_session()$
        dataset("landsat_ot_c2_l2")$
        search(temporal = filter_temporal("2020-07-01", "2020-07-31"), max_results = 2)$
        scene_list()
    }
    cached
  }
})

test_that("a scene list from a search knows its dataset without a lookup", {
  skip_if_no_m2m()

  scenes <- list_fixture()

  expect_s3_class(scenes, "M2MSceneList")
  expect_equal(scenes$dataset_name, "landsat_ot_c2_l2")
  expect_equal(scenes$dataset(), "landsat_ot_c2_l2")
  expect_output(print(scenes), "landsat_ot_c2_l2")
})

test_that("reattaching by id resolves the dataset from the list summary", {
  skip_if_no_m2m()

  # The API does not report a list's dataset directly, so a reattached list
  # starts out not knowing it. Sending that NA to download-options used to
  # fail obscurely.
  reattached <- test_session()$scene_list(list_fixture()$list_id)

  expect_true(is.na(reattached$dataset_name))
  expect_output(print(reattached), "<unresolved>")

  expect_equal(reattached$dataset(), "landsat_ot_c2_l2")

  # resolution is cached, so the summary is not fetched again
  expect_equal(reattached$dataset_name, "landsat_ot_c2_l2")
  expect_output(print(reattached), "landsat_ot_c2_l2")
})

test_that("products works on a list reattached by id alone", {
  skip_if_no_m2m()

  reattached <- test_session()$scene_list(list_fixture()$list_id)
  opts <- reattached$products()

  expect_s3_class(opts, "M2MDownloadOptions")
  expect_gt(nrow(opts$products), 0)
})

test_that("a supplied dataset_name is used as given", {
  skip_if_no_m2m()

  scenes <- test_session()$scene_list(
    list_fixture()$list_id,
    dataset_name = "landsat_ot_c2_l2"
  )

  expect_equal(scenes$dataset_name, "landsat_ot_c2_l2")
  expect_equal(scenes$dataset(), "landsat_ot_c2_l2")
})

test_that("an unknown scene list summarizes empty rather than erroring", {
  skip_if_no_m2m()

  # The API answers HTTP 200 with no datasets and a null spatialBounds;
  # unnesting that raised a confusing dplyr error about a missing column.
  out <- test_session()$scene_list("no-such-list-xyz")$summary()

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_named(
    out,
    c(
      "datasetName", "sceneCount", "invalidSceneCount", "invalidScenes",
      "listTimeout", "datasetAvailable", "spatialBounds", "temporalExtent"
    )
  )
})

test_that("resolving an unknown scene list explains what is wrong", {
  skip_if_no_m2m()

  scenes <- test_session()$scene_list("no-such-list-xyz")

  expect_error(scenes$dataset(), "reports no dataset")
  expect_error(scenes$products(), "reports no dataset")
})
