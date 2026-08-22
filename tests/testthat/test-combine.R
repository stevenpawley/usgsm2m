# Combining searches, so one $products() call and one order covers scenes
# found by several queries.

# A search object built without touching the network. The dataset only needs
# to answer $alias(), which is all $combine() consults.
fake_search <- function(entity_ids, alias = "landsat_ot_c2_l2", total_hits = NULL) {
  dataset <- structure(
    list(alias = function() alias),
    class = c("M2MDataset", "R6")
  )

  M2MSceneSearch$new(
    session = NULL,
    dataset = dataset,
    results = dplyr::tibble(entityId = entity_ids, cloudCover = seq_along(entity_ids)),
    total_hits = total_hits %||% length(entity_ids)
  )
}

test_that("combining unions the scenes", {
  combined <- fake_search(c("A", "B"))$combine(fake_search(c("C", "D")))

  expect_s3_class(combined, "M2MSceneSearch")
  expect_equal(nrow(combined$scenes), 4)
  expect_equal(combined$scenes$entityId, c("A", "B", "C", "D"))
})

test_that("scenes in more than one search are kept once", {
  # searches that overlap must not double-count
  combined <- fake_search(c("A", "B"))$combine(fake_search(c("B", "C")))

  expect_equal(nrow(combined$scenes), 3)
  expect_equal(combined$scenes$entityId, c("A", "B", "C"))
})

test_that("the original searches are left alone", {
  july <- fake_search(c("A", "B"))
  sept <- fake_search(c("C", "D"))

  july$combine(sept)

  expect_equal(nrow(july$scenes), 2)
  expect_equal(nrow(sept$scenes), 2)
})

test_that("several searches can be combined at once, or by chaining", {
  a <- fake_search("A")
  b <- fake_search("B")
  d <- fake_search("D")

  expect_equal(nrow(a$combine(b, d)$scenes), 3)
  expect_equal(nrow(a$combine(b)$combine(d)$scenes), 3)
})

test_that("combining nothing returns the search unchanged", {
  a <- fake_search(c("A", "B"))
  expect_equal(nrow(a$combine()$scenes), 2)
})

test_that("total_hits reflects what the combined search holds", {
  # the API's per-query hit counts cannot be added without double-counting,
  # and a combined set is not something the API can page through
  combined <- fake_search(c("A", "B"), total_hits = 500L)$combine(
    fake_search(c("B", "C"), total_hits = 900L)
  )

  expect_equal(combined$total_hits, 3)
  expect_equal(combined$total_hits, nrow(combined$scenes))
})

test_that("searches of different datasets cannot be combined", {
  # products are discovered per dataset, so a mixed set has no valid $products()
  expect_error(
    fake_search("A", alias = "landsat_ot_c2_l2")$combine(
      fake_search("B", alias = "landsat_tm_c2_l1")
    ),
    "different datasets"
  )
})

test_that("combine rejects things that are not searches", {
  expect_error(fake_search("A")$combine("landsat"), "M2MSceneSearch")
  expect_error(fake_search("A")$combine(42), "M2MSceneSearch")
})

test_that("a combined search still filters", {
  combined <- fake_search(c("A", "B"))$combine(fake_search(c("C", "D")))
  expect_equal(nrow(combined$filter(cloudCover <= 1)$scenes), 2)
})

test_that("a combined search registers its own scene list", {
  # it is a different set of scenes, so it must not inherit a cached
  # registration from either input
  combined <- fake_search(c("A", "B"))$combine(fake_search("C"))
  expect_null(combined$.__enclos_env__$private$scene_list_)
})
