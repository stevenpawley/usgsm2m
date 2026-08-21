#' Results of a scene search
#'
#' Returned by `M2MDataset$search()`. Holds the matching scenes and is the
#' bridge to discovering downloadable products. Not created directly.
#'
#' @export
M2MSceneSearch <- R6::R6Class(
  "M2MSceneSearch",
  public = list(
    #' @field scenes A tibble of the matching scenes.
    scenes = NULL,

    #' @field total_hits The total number of scenes the API reported as
    #'   matching, which exceeds `nrow(scenes)` when `max_results` was set.
    total_hits = NULL,

    #' @description Wrap search results. Use `M2MDataset$search()` instead.
    #' @param session The parent [M2MSession].
    #' @param dataset The [M2MDataset] that was searched.
    #' @param results A tibble of matching scenes.
    #' @param total_hits Total matches reported by the API.
    initialize = function(session, dataset, results, total_hits) {
      private$session_ <- session
      private$dataset_ <- dataset
      self$scenes <- results
      self$total_hits <- total_hits
      invisible(self)
    },

    #' @description The dataset these scenes came from.
    #' @return An [M2MDataset] object.
    dataset = function() {
      private$dataset_
    },

    #' @description Subset the scenes before requesting products, using
    #'   `dplyr::filter()` semantics against the `scenes` tibble.
    #' @param ... Expressions passed to `dplyr::filter()`.
    #' @return A new [M2MSceneSearch] with the subset applied.
    filter = function(...) {
      M2MSceneSearch$new(
        session = private$session_,
        dataset = private$dataset_,
        results = dplyr::filter(self$scenes, ...),
        total_hits = self$total_hits
      )
    },

    #' @description Register these scenes as a scene list on the M2M server.
    #'   `$products()` does this for you, so you only need it directly if you
    #'   want the list for its own sake (e.g. bulk metadata).
    #' @param list_id Optional identifier. A unique one is generated if
    #'   omitted.
    #' @return An [M2MSceneList] object.
    scene_list = function(list_id = NULL) {
      if (nrow(self$scenes) == 0) {
        stop("No scenes to add to a scene list")
      }

      list_id <- list_id %||% m2m_new_list_id()

      ers_scene_list_add(
        m2m_session_handle(private$session_),
        dataset_name = private$dataset_$alias(),
        scenes = self$scenes,
        list_id = list_id
      )

      M2MSceneList$new(
        session = private$session_,
        list_id = list_id,
        dataset_name = private$dataset_$alias()
      )
    },

    #' @description Discover the products (bands and related files) available
    #'   for these scenes. This registers a scene list server-side first,
    #'   which the M2M API requires before download options can be listed.
    #' @param band_group Whether to include secondary file groups, i.e. the
    #'   individual bands. `TRUE` by default.
    #' @return An [M2MDownloadOptions] object.
    products = function(band_group = TRUE) {
      self$scene_list()$products(band_group = band_group)
    },

    #' @description Print a summary of the search results.
    #' @param ... Ignored.
    print = function(...) {
      cat("<M2MSceneSearch>\n")
      cat("  Dataset: ", private$dataset_$alias(), "\n", sep = "")
      cat(
        "  Scenes:  ", format(nrow(self$scenes), big.mark = ","),
        " of ", format(self$total_hits, big.mark = ","), " total hits\n",
        sep = ""
      )
      if (nrow(self$scenes) > 0) {
        m2m_print_next("$products()", "$filter(...)", "$scene_list()")
      }
      invisible(self)
    }
  ),
  private = list(
    session_ = NULL,
    dataset_ = NULL
  )
)


#' A scene list registered on the M2M server
#'
#' Scene lists are the M2M API's way of naming a set of scenes so that other
#' endpoints can act on them in bulk: `download-options` and
#' `scene-metadata-list` will not take scene ids inline, only a `listId`.
#'
#' `M2MSceneSearch$products()` creates and uses one for you, so the usual
#' pipeline never mentions scene lists. Reach for this class when the list
#' itself is what you want - `$metadata()` fetches metadata for every scene in
#' one call rather than one call per scene, and `$summary()` reports the set's
#' combined extent.
#'
#' Get one with `M2MSceneSearch$scene_list()`, or reattach to an existing
#' list with `M2MSession$scene_list()`. Not created directly.
#'
#' @export
M2MSceneList <- R6::R6Class(
  "M2MSceneList",
  public = list(
    #' @field list_id The scene list's identifier.
    list_id = NULL,

    #' @field dataset_name The dataset alias the list belongs to, or `NA` if
    #'   not yet known. Reattaching to a list by id leaves this `NA` until
    #'   `$dataset()` resolves it, since the API does not report a list's
    #'   dataset except through its summary.
    dataset_name = NULL,

    #' @description Attach to a scene list. Use `M2MSceneSearch$scene_list()`
    #'   or `M2MSession$scene_list()` instead.
    #' @param session The parent [M2MSession].
    #' @param list_id The scene list identifier.
    #' @param dataset_name The dataset alias the list belongs to, or `NA` if
    #'   unknown.
    initialize = function(session, list_id, dataset_name = NA_character_) {
      private$session_ <- session
      self$list_id <- list_id
      self$dataset_name <- dataset_name %||% NA_character_
      invisible(self)
    },

    #' @description The dataset alias this list belongs to, looked up from the
    #'   list's summary if it is not already known and cached thereafter.
    #'
    #'   A scene list can span several datasets, in which case there is no
    #'   single answer and this errors - pass `dataset_name` to
    #'   `M2MSession$scene_list()` to say which one you mean.
    #' @return The dataset alias, as a string.
    dataset = function() {
      if (!is.na(self$dataset_name)) {
        return(self$dataset_name)
      }

      found <- unique(stats::na.omit(self$summary()$datasetName))

      if (length(found) == 0) {
        stop(
          "Scene list '", self$list_id,
          "' reports no dataset; it may have expired or be empty"
        )
      }

      if (length(found) > 1) {
        stop(
          "Scene list '", self$list_id, "' spans several datasets (",
          paste(found, collapse = ", "),
          "). Say which one with sess$scene_list(list_id, dataset_name = )"
        )
      }

      self$dataset_name <- found[[1]]
      self$dataset_name
    },

    #' @description The entity ids currently in the list.
    #' @return A tibble of scene entity ids.
    scenes = function() {
      ers_scene_list_get(m2m_session_handle(private$session_), self$list_id)
    },

    #' @description Full metadata for every scene in the list.
    #' @return A tibble of scene metadata.
    metadata = function() {
      ers_scene_metadata_list(m2m_session_handle(private$session_), self$list_id)
    },

    #' @description Summarize the list's spatial and temporal extent, and the
    #'   datasets it spans.
    #' @return A tibble, with overall bounds in the `spatialBounds` attribute.
    summary = function() {
      ers_scene_list_summary(m2m_session_handle(private$session_), self$list_id)
    },

    #' @description Discover the products available for the scenes in this
    #'   list.
    #' @param band_group Whether to include secondary file groups (bands).
    #' @return An [M2MDownloadOptions] object.
    products = function(band_group = TRUE) {
      products <- ers_scene_products(
        m2m_session_handle(private$session_),
        # resolve rather than trusting the field: a list reattached by id
        # starts out not knowing its dataset, and sending NA would make
        # download-options fail obscurely
        dataset_name = self$dataset(),
        list_id = self$list_id,
        band_group = band_group
      )

      M2MDownloadOptions$new(
        session = private$session_,
        products = products,
        scene_list = self
      )
    },

    #' @description Delete the scene list from the M2M server. Lists also
    #'   expire on their own after a period of inactivity.
    #' @return The scene list, invisibly.
    remove = function() {
      ers_scene_list_remove(m2m_session_handle(private$session_), self$list_id)
      invisible(self)
    },

    #' @description Print a summary of the scene list.
    #' @param ... Ignored.
    print = function(...) {
      cat("<M2MSceneList>\n")
      cat("  List id: ", self$list_id, "\n", sep = "")
      cat(
        "  Dataset: ",
        if (is.na(self$dataset_name)) "<unresolved>" else self$dataset_name,
        "\n",
        sep = ""
      )
      m2m_print_next("$products()", "$metadata()", "$summary()")
      invisible(self)
    }
  ),
  private = list(
    session_ = NULL
  )
)
