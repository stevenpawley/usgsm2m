# One search per dataset, shared across the tests below. An unfiltered search
# over a whole dataset is expensive server-side, so do it once.
aerial_fixture <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      ds <- test_session()$dataset("aerial_combin")
      cached <<- list(dataset = ds, entity_id = ds$search(max_results = 1)$scenes$entityId[[1]])
    }
    cached
  }
})

test_that("related_scenes finds secondary scenes on a supporting dataset", {
  skip_if_no_m2m()

  # Most datasets define no secondary relationship; aerial_combin does.
  fx <- aerial_fixture()
  related <- fx$dataset$related_scenes(fx$entity_id, max_results = 5)

  expect_s3_class(related, "M2MSceneSearch")
  expect_s3_class(related$scenes, "tbl_df")
  expect_gt(nrow(related$scenes), 0)
  expect_lte(nrow(related$scenes), 5)
  expect_true("entityId" %in% names(related$scenes))
  expect_gte(related$total_hits, nrow(related$scenes))

  # results must be bound to a dataset so follow-on calls use the right alias
  expect_s3_class(related$dataset(), "M2MDataset")
  expect_type(related$dataset()$alias(), "character")

  # metadata must survive the shared coercion, as it does for scene-search
  expect_false(any(grepl("^c\\(", related$scenes$metadata[[1]]$id)))
})

test_that("related_scenes errors on a dataset without a secondary relationship", {
  skip_if_no_m2m()

  # The dataset rejects the request before the entityId is looked up, so any
  # id will do - no need for a second expensive search to obtain a real one.
  expect_error(
    test_session()$dataset("landsat_ot_c2_l2")$related_scenes("LC90050012026229LGN00"),
    class = "m2m_error"
  )
})

test_that("download_labels lists orders in the queue", {
  skip_if_no_m2m()

  labels <- test_session()$download_labels()

  expect_s3_class(labels, "tbl_df")

  # the account may legitimately have no orders queued
  if (nrow(labels) > 0) {
    expect_true(all(
      c("label", "downloadCount", "totalComplete", "downloadSize") %in% names(labels)
    ))
    expect_type(labels$label, "character")
  }
})

test_that("download queue summary returns the documented shape", {
  skip_if_no_m2m()

  queue <- test_session()$download_queue("nonexistent-label-xyz")
  out <- queue$summary()

  expect_named(
    out,
    c("label", "download_count", "scene_count", "total_estimated_size", "collections")
  )
  expect_equal(out$label, "nonexistent-label-xyz")
  expect_equal(out$download_count, 0)
  expect_s3_class(out$collections, "tbl_df")
})

test_that("download_summary requires a download application", {
  skip_if_no_m2m()

  queue <- test_session()$download_queue("nonexistent-label-xyz")

  # the API answers INPUT_PARAMETER_REQUIRED without it, so refuse up front
  expect_error(queue$summary(download_application = NULL), "download_application is required")
})

test_that("prepare is a no-op for a label with no staged scenes", {
  skip_if_no_m2m()

  # download-order-load mutates server state, so only exercise it against a
  # label that matches nothing - never a real order.
  queue <- test_session()$download_queue("nonexistent-label-xyz")

  expect_s3_class(queue$prepare(), "M2MDownloadQueue")
})
