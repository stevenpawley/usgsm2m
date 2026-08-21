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

test_that("a search reuses one registration across calls", {
  skip_if_no_m2m()

  found <- test_session()$
    dataset("landsat_ot_c2_l2")$
    search(temporal = filter_temporal("2020-07-01", "2020-07-31"), max_results = 2)

  # Repeated $scene_list()/$products() must not leave a trail of identical
  # scene lists on the server.
  first <- found$scene_list()
  expect_identical(found$scene_list()$list_id, first$list_id)

  found$products()
  found$products()
  expect_identical(found$scene_list()$list_id, first$list_id)

  on.exit(try(found$scene_list()$remove(), silent = TRUE), add = TRUE)
})

test_that("an explicit list_id registers separately from the reused one", {
  skip_if_no_m2m()

  found <- test_session()$
    dataset("landsat_ot_c2_l2")$
    search(temporal = filter_temporal("2020-07-01", "2020-07-31"), max_results = 2)

  reused <- found$scene_list()
  explicit <- found$scene_list(list_id = "USGSm2m_test_explicit_id")
  on.exit(try(explicit$remove(), silent = TRUE), add = TRUE)
  on.exit(try(reused$remove(), silent = TRUE), add = TRUE)

  expect_equal(explicit$list_id, "USGSm2m_test_explicit_id")
  expect_identical(found$scene_list()$list_id, reused$list_id)
})

test_that("a removed list is registered again rather than reused", {
  skip_if_no_m2m()

  found <- test_session()$
    dataset("landsat_ot_c2_l2")$
    search(temporal = filter_temporal("2020-07-01", "2020-07-31"), max_results = 2)

  first <- found$scene_list()
  expect_false(first$removed)

  first$remove()
  expect_true(first$removed)

  second <- found$scene_list()
  on.exit(try(second$remove(), silent = TRUE), add = TRUE)

  expect_false(identical(second$list_id, first$list_id))
  expect_false(second$removed)
})

test_that("a filtered search registers its own list", {
  skip_if_no_m2m()

  found <- test_session()$
    dataset("landsat_ot_c2_l2")$
    search(temporal = filter_temporal("2020-07-01", "2020-07-31"), max_results = 2)

  subset <- found$filter(dplyr::row_number() == 1)

  # a narrowed search is a different set of scenes and must not share the
  # parent's registration
  expect_false(identical(subset$scene_list()$list_id, found$scene_list()$list_id))

  on.exit(try(found$scene_list()$remove(), silent = TRUE), add = TRUE)
  on.exit(try(subset$scene_list()$remove(), silent = TRUE), add = TRUE)
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
