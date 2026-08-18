#' A dataset in the M2M catalog
#'
#' Returned by `M2MSession$dataset()`. Holds a dataset's metadata and is the
#' starting point for a scene search. Not created directly.
#'
#' @export
M2MDataset <- R6::R6Class(
  "M2MDataset",
  public = list(
    #' @field info A one-row tibble of the dataset's metadata.
    info = NULL,

    #' @description Wrap dataset metadata. Use `M2MSession$dataset()` instead.
    #' @param session The parent [M2MSession].
    #' @param info A one-row tibble of dataset metadata.
    initialize = function(session, info) {
      private$session_ <- session
      self$info <- info
      invisible(self)
    },

    #' @description The dataset's system-friendly alias, e.g.
    #'   `"landsat_ot_c2_l2"`.
    #' @return A string.
    alias = function() {
      self$info$datasetAlias[[1]]
    },

    #' @description The dataset's identifier.
    #' @return A string.
    id = function() {
      self$info$datasetId[[1]]
    },

    #' @description Metadata filter fields available for this dataset. Each
    #'   row's `id` is the `filterId` used when building a metadata filter to
    #'   pass to `$search(metadata = )`.
    #' @return A tibble of filter fields.
    filters = function() {
      ers_dataset_filters(m2m_session_handle(private$session_), self$alias())
    },

    #' @description Search this dataset for scenes. All filters are optional;
    #'   with none supplied every scene in the dataset matches.
    #' @param spatial A spatial filter, see [filter_spatial()].
    #' @param temporal A temporal filter, see [filter_temporal()].
    #' @param cloud A cloud cover filter, see [filter_cloud()].
    #' @param metadata A metadata filter built from `$filters()` field ids.
    #' @param max_results Maximum number of scenes to return. If `NULL` (the
    #'   default) all matching scenes are retrieved by paging through results.
    #' @return An [M2MSceneSearch] object.
    search = function(
      spatial = NULL,
      temporal = NULL,
      cloud = NULL,
      metadata = NULL,
      max_results = NULL
    ) {
      found <- ers_scene_search(
        m2m_session_handle(private$session_),
        dataset_name = self$alias(),
        spatial_filter = spatial,
        temporal_filter = temporal,
        cloud_filter = cloud,
        metadata_filter = metadata,
        max_results = max_results
      )

      M2MSceneSearch$new(
        session = private$session_,
        dataset = self,
        results = found$results,
        total_hits = found$total_hits
      )
    },

    #' @description Print a summary of the dataset.
    #' @param ... Ignored.
    print = function(...) {
      cat("<M2MDataset>\n")
      cat("  Alias:   ", self$alias(), "\n", sep = "")
      cat("  Name:    ", self$info$collectionName[[1]], "\n", sep = "")
      if (!is.null(self$info$sceneCount)) {
        cat("  Scenes:  ", format(self$info$sceneCount[[1]], big.mark = ","), "\n", sep = "")
      }
      m2m_print_next("$search(...)", "$filters()")
      invisible(self)
    }
  ),
  private = list(
    session_ = NULL
  )
)
