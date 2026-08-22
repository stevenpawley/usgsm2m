# Offline tests for the coercion functions.
#
# Every bug found in this package so far has been an API field whose shape
# varies with the data, coerced as though it were fixed, and most produced
# plausible but wrong output rather than an error. These pin the shapes that
# broke, using hand-built payloads so they run with no credentials and no
# network. See helper-payloads.R.

# --- scene search ------------------------------------------------------------

test_that("scene metadata survives ragged field counts", {
  # jsonify hands back a scenes-by-fields matrix when every scene carries the
  # same number of metadata fields, and a list-column when they differ.
  # Indexing the list form as a matrix silently deparsed each scene's metadata
  # into a single "c(\"a\", \"b\")" string.
  ragged <- coerce_scene_results(list(
    fake_scene(1, n_metadata = 3),
    fake_scene(2, n_metadata = 2)
  ))

  expect_equal(nrow(ragged), 2)
  expect_equal(vapply(ragged$metadata, nrow, integer(1)), c(3L, 2L))
  expect_false(any(grepl("^c\\(", ragged$metadata[[1]]$id)))
  expect_equal(ragged$metadata[[1]]$id, c("m1", "m2", "m3"))
})

test_that("scene metadata is correct when field counts are uniform", {
  # the matrix branch; both must agree in shape
  uniform <- coerce_scene_results(list(
    fake_scene(1, n_metadata = 3),
    fake_scene(2, n_metadata = 3)
  ))

  expect_equal(vapply(uniform$metadata, nrow, integer(1)), c(3L, 3L))
  expect_false(any(grepl("^c\\(", uniform$metadata[[1]]$id)))
  expect_named(uniform$metadata[[1]], c("id", "fieldName", "value", "dictionaryLink"))
})

test_that("scenes are neither duplicated nor dropped", {
  scenes <- coerce_scene_results(lapply(1:3, fake_scene))

  expect_equal(nrow(scenes), 3)
  expect_equal(scenes$entityId, c("E1", "E2", "E3"))
})

test_that("a scene with several browse images stays one row", {
  scenes <- coerce_scene_results(list(
    fake_scene(1, n_browse = 2),
    fake_scene(2, n_browse = 2)
  ))

  expect_equal(nrow(scenes), 2)
})

test_that("an empty result set gives an empty tibble", {
  expect_equal(nrow(coerce_scene_results(list())), 0)
  expect_s3_class(coerce_scene_results(list()), "tbl_df")
})

# --- datasets ----------------------------------------------------------------

test_that("a dataset in several catalogs stays one row", {
  # unnesting the multi-valued `catalogs` column repeated the dataset once per
  # catalog it belongs to
  df <- list(
    fake_dataset("ds_a", catalogs = list("EE", "GV")),
    fake_dataset("ds_b", catalogs = list("EE"))
  ) |>
    jsonify::to_json() |>
    jsonify::from_json() |>
    dplyr::as_tibble() |>
    coerce_dataset_df()

  expect_equal(nrow(df), 2)
  expect_equal(df$datasetAlias, c("ds_a", "ds_b"))
  expect_type(df$catalogs, "list")
})

test_that("a dataset with a null field keeps its row, with NA", {
  # jsonify represents a null field as a zero-length list element. Unnesting
  # those drops the row whenever every unnested column is empty for it, so
  # they are unwrapped to NA instead.
  df <- list(
    fake_dataset("ds_a", acquisition_end = NULL),
    fake_dataset("ds_b")
  ) |>
    jsonify::to_json() |>
    jsonify::from_json() |>
    dplyr::as_tibble() |>
    coerce_dataset_df()

  expect_equal(nrow(df), 2)
  expect_true("ds_a" %in% df$datasetAlias)
  expect_true(is.na(df$acquisitionEnd[df$datasetAlias == "ds_a"]))
})

test_that("a dataset whose every list field is empty keeps its row", {
  # the configuration that actually loses a row under the old unnest: nothing
  # left to expand against, so tidyr drops it entirely
  df <- list(
    fake_dataset("ds_a", catalogs = list(), acquisition_end = NULL),
    fake_dataset("ds_b")
  ) |>
    jsonify::to_json() |>
    jsonify::from_json() |>
    dplyr::as_tibble() |>
    coerce_dataset_df()

  expect_equal(nrow(df), 2)
  expect_true("ds_a" %in% df$datasetAlias)
})

# --- dataset filters ---------------------------------------------------------

test_that("a Select filter field stays one row", {
  # valueList was flattened into the tibble, recycling each field into as many
  # rows as it had permitted values
  fields <- coerce_filter_fields(list(
    fake_filter_field("f1", "WRS Path"),
    fake_filter_field("f2", "Satellite", value_list = list("All", "8", "9")),
    fake_filter_field("f3", "Sensor", value_list = list("A", "B"))
  ))

  expect_equal(nrow(fields), 3)
  expect_equal(fields$id, c("f1", "f2", "f3"))
  expect_type(fields$valueList, "list")
  expect_type(fields$fieldConfig, "list")
})

test_that("no filter fields gives an empty tibble", {
  expect_equal(nrow(coerce_filter_fields(list())), 0)
})

# --- download options --------------------------------------------------------

test_that("products with and without secondary downloads coexist", {
  # jsonify renders secondaryDownloads as a data.frame when the product has
  # files and an empty list when it has none, and a list-column mixing the two
  # cannot be unnested - download-options failed outright for any dataset
  # carrying both kinds
  products <- coerce_products(list(
    fake_product("E1", "Product Bundle", n_files = 2),
    fake_product("E1", "Browse", n_files = 0),
    fake_product("E2", "Product Bundle", n_files = 2)
  ))

  expect_equal(nrow(products), 3)

  opts <- M2MDownloadOptions$new(session = NULL, products = products, scene_list = NULL)
  expect_s3_class(opts$bands(), "tbl_df")
  expect_gt(nrow(opts$bands()), 0)
})

test_that("files repeated across products are counted once", {
  # the API lists a scene's secondaryDownloads under each of its product
  # entries, so flattening without de-duplicating queued every file N times
  products <- coerce_products(list(
    fake_product("E1", "Product Bundle", file_ids = c("a", "b")),
    fake_product("E1", "Band File", file_ids = c("a", "b"))
  ))

  bands <- M2MDownloadOptions$new(
    session = NULL, products = products, scene_list = NULL
  )$bands()

  expect_equal(nrow(bands), 2)
  expect_equal(nrow(bands), nrow(dplyr::distinct(bands)))
})

test_that("scene_products exposes products that have no files", {
  products <- coerce_products(list(
    fake_product("E1", "Product Bundle", n_files = 2),
    fake_product("E1", "Browse", n_files = 0)
  ))

  opts <- M2MDownloadOptions$new(session = NULL, products = products, scene_list = NULL)

  unreachable <- setdiff(opts$scene_products()$id, opts$bands()$id)
  expect_gt(length(unreachable), 0)
  expect_false("secondaryDownloads" %in% names(opts$scene_products()))
})

test_that("no products gives an empty tibble", {
  expect_equal(nrow(coerce_products(list())), 0)
})

test_that("a pattern matching no product warns and lists what was there", {
  # product names differ between datasets - the bundle is "... Product Bundle"
  # for Landsat but "Standard Format" for corona2 - so a pattern carried over
  # from elsewhere selects nothing, and used to do so silently
  products <- coerce_products(list(fake_product("E1", "Standard Format")))
  opts <- M2MDownloadOptions$new(session = NULL, products = products, scene_list = NULL)

  expect_warning(
    narrowed <- opts$select_products("Product Bundle"),
    "Standard Format"
  )
  expect_warning(opts$select_products("Product Bundle"), "Nothing matched")
  expect_equal(nrow(narrowed$selected()), 0)
})

test_that("a pattern matching no band warns", {
  products <- coerce_products(list(fake_product("E1", "Bundle", file_ids = c("B1", "B2"))))
  opts <- M2MDownloadOptions$new(session = NULL, products = products, scene_list = NULL)

  expect_warning(opts$select_bands("NOT_A_BAND"), "Nothing matched")
})

test_that("products are matched exactly, including punctuation", {
  # a name copied out of $scene_products() must select itself. Under pattern
  # matching the parentheses were read as regex groups, so the name matched
  # nothing at all.
  name <- "Full-Resolution Browse (Natural Color) GeoTIFF"
  products <- coerce_products(list(
    fake_product("E1", name, n_files = 0),
    fake_product("E1", "Standard Format")
  ))
  opts <- M2MDownloadOptions$new(session = NULL, products = products, scene_list = NULL)

  expect_equal(nrow(opts$select_products(name)$selected()), 1)

  # and a partial no longer matches, so selection cannot be accidental
  expect_warning(
    partial <- opts$select_products("Browse"),
    "Nothing matched"
  )
  expect_equal(nrow(partial$selected()), 0)
})

test_that("products can be selected by productCode", {
  products <- coerce_products(list(fake_product("E1", "Standard Format")))
  opts <- M2MDownloadOptions$new(session = NULL, products = products, scene_list = NULL)

  code <- opts$scene_products()$productCode[[1]]
  expect_equal(nrow(opts$select_products(code)$selected()), 1)
})

test_that("bands match literal substrings, not patterns", {
  products <- coerce_products(list(
    fake_product("E1", "Bundle", file_ids = c("B1", "B2"))
  ))
  opts <- M2MDownloadOptions$new(session = NULL, products = products, scene_list = NULL)

  # substring matching is the point of select_bands
  expect_equal(nrow(opts$select_bands("B1")$selected()), 1)
  expect_equal(nrow(opts$select_bands(c("B1", "B2"))$selected()), 2)

  # but regex syntax is not honoured - "." is a literal dot, not "any char"
  expect_warning(expect_equal(nrow(opts$select_bands("B.")$selected()), 0))
})

test_that("a matching pattern does not warn", {
  products <- coerce_products(list(fake_product("E1", "Standard Format")))
  opts <- M2MDownloadOptions$new(session = NULL, products = products, scene_list = NULL)

  expect_no_warning(opts$select_products("Standard Format"))
})

# --- scene list summary ------------------------------------------------------

test_that("scene list summary has a fixed set of columns", {
  # listTimeout is NULL for a list with no expiry, and passing NULL to
  # tibble() drops the column silently, making the schema depend on the data
  expected <- c(
    "datasetName", "sceneCount", "invalidSceneCount", "invalidScenes",
    "listTimeout", "datasetAvailable", "spatialBounds", "temporalExtent"
  )

  with_timeout <- coerce_scene_list_summary(fake_list_summary(list_timeout = "2020-08-01"))
  without <- coerce_scene_list_summary(fake_list_summary(list_timeout = NULL))

  expect_named(with_timeout, expected)
  expect_named(without, expected)
  expect_true(is.na(without$listTimeout))
})

test_that("invalidScenes stays a list column when empty", {
  out <- coerce_scene_list_summary(fake_list_summary())

  expect_type(out$invalidScenes, "list")
  expect_s3_class(out$invalidScenes[[1]], "tbl_df")
})

test_that("an unknown scene list summarizes empty rather than erroring", {
  # HTTP 200 with no datasets and a null spatialBounds raised a confusing
  # "Column `coordinates` doesn't exist" from dplyr
  out <- coerce_scene_list_summary(fake_empty_list_summary())

  expect_equal(nrow(out), 0)
  expect_named(
    out,
    c(
      "datasetName", "sceneCount", "invalidSceneCount", "invalidScenes",
      "listTimeout", "datasetAvailable", "spatialBounds", "temporalExtent"
    )
  )
  expect_s3_class(attr(out, "spatialBounds"), "tbl_df")
})

# --- shared helpers ----------------------------------------------------------

test_that("m2m_records_to_tibble handles empty and populated input", {
  expect_equal(nrow(m2m_records_to_tibble(list())), 0)
  expect_equal(nrow(m2m_records_to_tibble(NULL)), 0)

  populated <- m2m_records_to_tibble(list(
    list(a = 1L, b = "x"),
    list(a = 2L, b = "y")
  ))
  expect_equal(nrow(populated), 2)
  expect_equal(populated$b, c("x", "y"))
})
