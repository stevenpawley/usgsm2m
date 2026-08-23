# A small search reused across the pipeline tests, so the suite performs one
# scene search rather than one per test.
search_fixture <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- test_session()$
        dataset("landsat_ot_c2_l2")$
        search(
          spatial = filter_spatial(ll_lon = -120, ll_lat = 40, ur_lon = -119.5, ur_lat = 40.5),
          temporal = filter_temporal("2020-07-01", "2020-07-31"),
          cloud = filter_cloud(0, 50),
          max_results = 3
        )
    }
    cached
  }
})

test_that("search() returns scenes and reports total hits", {
  skip_if_no_m2m()

  found <- search_fixture()

  expect_s3_class(found, "M2MSceneSearch")
  expect_s3_class(found$scenes, "tbl_df")
  expect_lte(nrow(found$scenes), 3)
  expect_gt(nrow(found$scenes), 0)
  expect_true("entityId" %in% names(found$scenes))
  expect_gte(found$total_hits, nrow(found$scenes))
})

test_that("search print shows counts and the next step", {
  skip_if_no_m2m()

  expect_output(print(search_fixture()), "M2MSceneSearch")
  expect_output(print(search_fixture()), "landsat_ot_c2_l2")
  expect_output(print(search_fixture()), "\\$products")
})

test_that("search filter() narrows scenes without another API call", {
  skip_if_no_m2m()

  found <- search_fixture()
  one <- found$filter(dplyr::row_number() == 1)

  expect_s3_class(one, "M2MSceneSearch")
  expect_equal(nrow(one$scenes), 1)
  # original is untouched - stage transitions return new objects
  expect_equal(nrow(found$scenes), nrow(search_fixture()$scenes))
})

test_that("products() creates the scene list implicitly and lists bands", {
  skip_if_no_m2m()

  opts <- search_fixture()$filter(dplyr::row_number() == 1)$products()

  expect_s3_class(opts, "M2MDownloadOptions")
  expect_s3_class(opts$products, "tbl_df")

  bands <- opts$bands()
  expect_s3_class(bands, "tbl_df")
  expect_gt(nrow(bands), 0)
  expect_true(all(c("entityId", "id", "displayId") %in% names(bands)))
})

test_that("select_bands narrows the selection by displayId", {
  skip_if_no_m2m()

  opts <- search_fixture()$filter(dplyr::row_number() == 1)$products()
  narrowed <- opts$select_bands("B4")

  expect_s3_class(narrowed, "M2MDownloadOptions")
  expect_lt(nrow(narrowed$bands()), nrow(opts$bands()))
  expect_true(all(grepl("B4", narrowed$bands()$displayId)))
  # the original selection is unchanged
  expect_gt(nrow(opts$bands()), nrow(narrowed$bands()))
})

test_that("request() refuses an empty selection", {
  skip_if_no_m2m()

  opts <- search_fixture()$filter(dplyr::row_number() == 1)$products()

  # selecting nothing warns and says what was available
  expect_warning(
    empty <- opts$select_bands("NOT_A_REAL_BAND_NAME"),
    "Nothing matched"
  )

  expect_equal(nrow(empty$bands()), 0)
  expect_error(empty$request(label = "test"), "No products selected")
})

test_that("products registers its required scene list privately", {
  skip_if_no_m2m()

  search <- search_fixture()$filter(dplyr::row_number() == 1)
  products <- search$products()
  scene_list <- search$.__enclos_env__$private$scene_list_
  on.exit(try(scene_list$remove(), silent = TRUE), add = TRUE)

  expect_s3_class(products, "M2MDownloadOptions")
  expect_false("scene_list" %in% names(search))
})

test_that("download_queue reconnects to a label and reports readiness", {
  skip_if_no_m2m()

  queue <- test_session()$download_queue("nonexistent-label-xyz")

  expect_s3_class(queue, "M2MDownloadQueue")
  expect_s3_class(queue$ready(), "tbl_df")
  expect_s3_class(queue$pending(), "tbl_df")
  expect_true(queue$is_ready())
  expect_error(queue$retrieve(tempdir()), "No downloads are available")
})
