# Hand-built API payloads for the offline coercion tests.
#
# These reproduce the *shapes* that broke the coercion functions, not the
# volume of a real response: what matters is that field counts differ between
# records, that some fields are null, and that nested members are sometimes a
# data.frame and sometimes an empty list. A real scene-search response
# exhibiting the ragged-metadata case is ~8.7 MB; the equivalent here is ~1 KB
# and, unlike a recorded fixture, it is legible in a diff.

# One scene-search result. `n_metadata` controls how many metadata entries it
# carries, which is what makes a set of scenes ragged or uniform.
fake_scene <- function(i, n_metadata = 3, n_browse = 1) {
  list(
    entityId = paste0("E", i),
    displayId = paste0("D", i),
    cloudCover = 10L,
    publishDate = "2020-07-01",
    browse = lapply(seq_len(n_browse), function(b) {
      list(browseName = paste0("b", b), browsePath = paste0("p", b))
    }),
    metadata = lapply(seq_len(n_metadata), function(j) {
      list(
        id = paste0("m", j),
        fieldName = paste0("f", j),
        value = paste0("v", j),
        dictionaryLink = ""
      )
    }),
    options = list(bulk = TRUE, download = TRUE),
    selected = list(bulk = FALSE),
    spatialBounds = list(type = "Polygon", coordinates = list(1, 2)),
    spatialCoverage = list(type = "Polygon", coordinates = list(1, 2)),
    temporalCoverage = list(startDate = "2020-07-01", endDate = "2020-07-02")
  )
}

# One dataset record. `catalogs` is multi-valued for some datasets, and
# `acquisitionEnd` is null for datasets still being added to.
fake_dataset <- function(alias, catalogs = list("EE"), acquisition_end = "2024-01-01") {
  list(
    abstractText = "an abstract",
    acquisitionStart = "2013-02-11",
    acquisitionEnd = acquisition_end,
    catalogs = catalogs,
    collectionName = paste("Collection", alias),
    datasetId = paste0("id_", alias),
    datasetAlias = alias,
    sceneCount = 100L,
    # a real record carries spatialBounds, which the coercion treats specially
    spatialBounds = list(type = "Polygon", coordinates = list(1, 2)),
    temporalCoverage = "[\"2013-02-11\",\"present\"]",
    supportCloudCover = TRUE
  )
}

# One dataset-filters field. Select fields carry a valueList of options.
fake_filter_field <- function(id, label, value_list = NULL) {
  list(
    id = id,
    legacyFieldId = NULL,
    dictionaryLink = "https://example.org",
    fieldConfig = list(type = if (is.null(value_list)) "Text" else "Select"),
    fieldLabel = label,
    searchSql = paste0(label, " = ?"),
    valueList = value_list
  )
}

# One download-options product. `n_files` of 0 gives a product with no
# secondaryDownloads at all, as browse products have.
fake_product <- function(entity_id, product_name, n_files = 2, file_ids = NULL,
                         checksums = NULL) {
  # not paste0("file", seq_len(n_files)): with n_files = 0 that returns "file",
  # a length-1 vector, so the "no secondary downloads" case would silently
  # carry one
  file_ids <- file_ids %||% if (n_files == 0) character() else paste0("file", seq_len(n_files))

  # `checksums` mirrors how the API reports an optional nested object: a
  # value per file, NA for a checksum object whose value is null, and NULL
  # for a file carrying no checksum object at all
  secondary <- lapply(seq_along(file_ids), function(i) {
    fid <- file_ids[[i]]
    record <- list(
      id = fid,
      entityId = paste0(entity_id, "_", fid),
      displayId = paste0(entity_id, "_", fid),
      bulkAvailable = TRUE,
      filesize = 1024L
    )

    if (!is.null(checksums)) {
      value <- checksums[[i]]
      if (!is.null(value)) {
        record$checksum <- list(
          algorithm = "md5",
          value = if (is.na(value)) NULL else value
        )
      }
    }

    record
  })

  list(
    id = paste0("prod_", entity_id, "_", gsub(" ", "", product_name)),
    entityId = entity_id,
    displayId = entity_id,
    productName = product_name,
    productCode = paste0("PC_", gsub("[^A-Za-z0-9]", "", product_name)),
    available = TRUE,
    bulkAvailable = TRUE,
    filesize = 2048L,
    secondaryDownloads = secondary
  )
}

# A scene-list-summary payload for one dataset.
fake_list_summary <- function(dataset_name = "landsat_ot_c2_l2", list_timeout = NULL) {
  list(
    datasets = list(list(
      invalidSceneCount = 0L,
      invalidScenes = list(),
      datasetName = dataset_name,
      datasetAvailable = TRUE,
      listTimeout = list_timeout,
      spatialBounds = list(north = 1, south = 0),
      temporalExtent = list(min = "2020-07-01", max = "2020-07-31"),
      sceneCount = 5L
    )),
    summary = list(
      spatialBounds = list(type = "Polygon", coordinates = list(1, 2)),
      temporalExtent = list(min = "2020-07-01", max = "2020-07-31"),
      sceneCount = "5"
    )
  )
}

# What an unknown or expired scene list answers with: HTTP 200, no datasets,
# and a null spatialBounds.
fake_empty_list_summary <- function() {
  list(
    datasets = list(),
    summary = list(
      spatialBounds = NULL,
      temporalExtent = list(min = NULL, max = NULL),
      sceneCount = NULL
    )
  )
}
