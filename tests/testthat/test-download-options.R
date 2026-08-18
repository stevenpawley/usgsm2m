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
