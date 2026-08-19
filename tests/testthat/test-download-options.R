test_that("flattened bands are de-duplicated across product types", {
  skip_if_no_m2m()

  opts <- test_session()$
    dataset("landsat_ot_c2_l2")$
    search(
      spatial = filter_spatial(ll_lon = -120, ll_lat = 40, ur_lon = -119.5, ur_lat = 40.5),
      temporal = filter_temporal("2020-07-01", "2020-07-31"),
      cloud = filter_cloud(0, 50),
      max_results = 2
    )$
    products()

  bands <- opts$bands()

  # The API repeats a scene's secondaryDownloads under each product entry;
  # every file should appear exactly once so it is not queued N times.
  expect_equal(nrow(bands), nrow(dplyr::distinct(bands)))
  expect_equal(nrow(bands), nrow(dplyr::distinct(bands, entityId, id)))
})

test_that("scene_products exposes whole products that have no bands", {
  skip_if_no_m2m()

  # landsat_tm_c2_l1 offers browse imagery and a product bundle alongside the
  # per-band files. The browse products have no secondaryDownloads, so they
  # never appear in $bands() even though they are downloadable.
  opts <- test_session()$dataset("landsat_tm_c2_l1")$search(max_results = 1)$products()

  products <- opts$scene_products()

  expect_s3_class(products, "tbl_df")
  expect_false("secondaryDownloads" %in% names(products))

  # the point of this method: products unreachable through $bands()
  unreachable <- setdiff(products$id, opts$bands()$id)
  expect_gt(length(unreachable), 0)
  expect_true(any(grepl(
    "Browse",
    products$productName[products$id %in% unreachable]
  )))

  # download-request needs an entityId/productId pair; both must be present
  expect_true(all(c("entityId", "id", "productName", "available") %in% names(products)))
  expect_true(any(grepl("Browse", products$productName)))
})

test_that("select_products switches the selection to whole products", {
  skip_if_no_m2m()

  opts <- test_session()$dataset("landsat_tm_c2_l1")$search(max_results = 1)$products()

  bundle <- opts$select_products("Product Bundle")$filter(available)

  expect_s3_class(bundle, "M2MDownloadOptions")
  expect_gt(nrow(bundle$selected()), 0)
  expect_true(all(grepl("Product Bundle", bundle$selected()$productName)))
  expect_true(all(bundle$selected()$available))

  # a bundle is one row per scene, unlike the many files $bands() lists
  expect_lt(nrow(bundle$selected()), nrow(opts$bands()))

  browse <- opts$select_products("Browse")
  expect_gt(nrow(browse$selected()), 0)
  expect_true(all(grepl("Browse", browse$selected()$productName)))
})

test_that("select_bands and select_products each select from their own set", {
  skip_if_no_m2m()

  opts <- test_session()$dataset("landsat_tm_c2_l1")$search(max_results = 1)$products()

  # selecting products then bands must not leave the product rows behind
  reselected <- opts$select_products("Browse")$select_bands("B1")

  expect_gt(nrow(reselected$selected()), 0)
  expect_true(all(grepl("B1", reselected$selected()$displayId)))
  expect_false(any(grepl("Browse", reselected$selected()$productName %||% "")))

  # $selected() and $bands() agree by default
  expect_equal(nrow(opts$selected()), nrow(opts$bands()))
})
