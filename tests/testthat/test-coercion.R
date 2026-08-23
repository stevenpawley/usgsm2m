# Regression tests for the JSON coercion sites. The M2M API's shapes vary with
# the data it returns, and several bugs here were invisible on small queries.

test_that("scene metadata survives a search spanning ragged field counts", {
  skip_if_no_m2m()

  # Landsat C2 L2 scenes carry either 257 or 258 metadata fields. When the
  # counts are uniform jsonify hands back a matrix, and when they differ a
  # list - indexing the list form as if it were a matrix silently deparsed
  # each scene's metadata into a single "c(\"a\", \"b\")" string.
  found <- test_session()$dataset("landsat_ot_c2_l2")$search(
    temporal = filter_temporal("2020-07-01", "2020-07-31"),
    max_results = 200
  )

  expect_equal(nrow(found$scenes), 200)

  meta_rows <- vapply(found$scenes$metadata, nrow, integer(1))

  # Every scene should have many metadata fields, never a single deparsed row.
  expect_true(all(meta_rows > 50))

  first <- found$scenes$metadata[[1]]
  expect_s3_class(first, "tbl_df")
  expect_named(first, c("id", "fieldName", "value", "dictionaryLink"))
  expect_false(any(grepl("^c\\(", first$id)))
  expect_true("WRS Path" %in% first$fieldName)
})

test_that("scene metadata is also correct when field counts are uniform", {
  skip_if_no_m2m()

  # Small searches hit the matrix branch; both paths must agree in shape.
  found <- test_session()$dataset("landsat_ot_c2_l2")$search(
    temporal = filter_temporal("2020-07-01", "2020-07-31"),
    max_results = 3
  )

  meta_rows <- vapply(found$scenes$metadata, nrow, integer(1))
  expect_true(all(meta_rows > 50))
  expect_false(any(grepl("^c\\(", found$scenes$metadata[[1]]$id)))
})

test_that("products work when only some products carry secondary downloads", {
  skip_if_no_m2m()

  # landsat_tm_c2_l1 mixes products that have bands with browse products that
  # have none. jsonify renders the former as a data.frame and the latter as an
  # empty list, and a list-column holding both cannot be unnested - this used
  # to fail outright with "Can't combine <data.frame> and <list>".
  opts <- test_session()$dataset("landsat_tm_c2_l1")$search(max_results = 3)$products()

  expect_s3_class(opts, "M2MDownloadOptions")

  bands <- opts$bands()
  expect_s3_class(bands, "tbl_df")
  expect_gt(nrow(bands), 0)
  expect_equal(nrow(bands), nrow(dplyr::distinct(bands)))
})

test_that("scene list summary keeps a stable set of columns", {
  skip_if_no_m2m()

  found <- test_session()$dataset("landsat_ot_c2_l2")$search(
    temporal = filter_temporal("2020-07-01", "2020-07-31"),
    max_results = 5
  )
  found$products()
  scenes <- found$.__enclos_env__$private$scene_list_
  on.exit(try(scenes$remove(), silent = TRUE), add = TRUE)

  out <- scenes$summary()

  # listTimeout is NULL for a list with no expiry; passing that straight to
  # tibble() used to drop the column entirely, so the schema depended on data.
  expect_named(
    out,
    c(
      "datasetName", "sceneCount", "invalidSceneCount", "invalidScenes",
      "listTimeout", "datasetAvailable", "spatialBounds", "temporalExtent"
    )
  )

  # invalidScenes must stay a list-column whether or not any are invalid.
  expect_type(out$invalidScenes, "list")
  expect_s3_class(out$invalidScenes[[1]], "tbl_df")
  expect_s3_class(attr(out, "spatialBounds"), "tbl_df")
})
