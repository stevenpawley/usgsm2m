#' Products available for download
#'
#' Returned by `M2MSceneSearch$products()`. Holds the downloadable products
#' (bands and related files) for a set of scenes, and lets you narrow them
#' down before placing a download request. Not created directly.
#'
#' @export
M2MDownloadOptions <- R6::R6Class(
  "M2MDownloadOptions",
  public = list(
    #' @field products A scene-level tibble of products, with the individual
    #'   bands nested in a `secondaryDownloads` list-column.
    products = NULL,

    #' @description Wrap download options. Use `M2MSceneSearch$products()`
    #'   instead.
    #' @param session The parent [M2MSession].
    #' @param products A tibble of products from the download-options endpoint.
    #' @param scene_list The [M2MSceneList] the products belong to.
    #' @param selected An optional pre-narrowed band tibble.
    initialize = function(session, products, scene_list, selected = NULL) {
      private$session_ <- session
      private$scene_list_ <- scene_list
      self$products <- products

      private$selected_ <- selected %||% private$flatten_bands(products)
      invisible(self)
    },

    #' @description The individual downloadable files currently selected, one
    #'   row each. This is what `$request()` will queue.
    #' @return A tibble of downloadable files.
    bands = function() {
      private$selected_
    },

    #' @description Narrow the selection to bands whose `displayId` matches
    #'   any of the given patterns.
    #'
    #'   Note this does not filter on availability; chain
    #'   `$filter(bulkAvailable)` if you want only bulk-downloadable files.
    #' @param patterns A character vector of patterns, e.g. `c("B4", "B5")`.
    #' @return A new [M2MDownloadOptions] with the narrowed selection.
    select_bands = function(patterns) {
      matched <- dplyr::filter(
        private$selected_,
        stringr::str_detect(
          .data$displayId,
          stringr::str_c(patterns, collapse = "|")
        )
      )
      private$respawn(matched)
    },

    #' @description Narrow the selection using `dplyr::filter()` semantics
    #'   against the band tibble returned by `$bands()`.
    #' @param ... Expressions passed to `dplyr::filter()`.
    #' @return A new [M2MDownloadOptions] with the narrowed selection.
    filter = function(...) {
      private$respawn(dplyr::filter(private$selected_, ...))
    },

    #' @description Place a download request for the current selection. Files
    #'   the distribution system can serve immediately come back with URLs;
    #'   anything needing preparation is collected later via
    #'   `M2MDownloadQueue$refresh()`.
    #' @param label A label identifying this order in the download queue.
    #'   Required, and used to find the order again later.
    #' @return An [M2MDownloadQueue] object.
    request = function(label) {
      if (nrow(private$selected_) == 0) {
        stop("No products selected to download")
      }

      requested <- ers_download_request(
        m2m_session_handle(private$session_),
        downloads = private$selected_,
        label = label
      )

      M2MDownloadQueue$new(
        session = private$session_,
        label = label,
        available = requested$available,
        n_preparing = requested$n_new
      )
    },

    #' @description Print a summary of the selected products.
    #' @param ... Ignored.
    print = function(...) {
      cat("<M2MDownloadOptions>\n")
      cat("  Scenes:  ", format(nrow(self$products), big.mark = ","), "\n", sep = "")
      cat("  Files:   ", format(nrow(private$selected_), big.mark = ","), " selected\n", sep = "")
      if (nrow(private$selected_) > 0) {
        m2m_print_next("$request(label)", "$select_bands(...)", "$bands()")
      }
      invisible(self)
    }
  ),
  private = list(
    session_ = NULL,
    scene_list_ = NULL,
    selected_ = NULL,

    # Flatten the nested per-scene products into one row per downloadable
    # file. With band_group = FALSE there is no secondaryDownloads column and
    # the products are themselves the downloadable units.
    flatten_bands = function(products) {
      if (nrow(products) == 0) {
        return(tibble::tibble())
      }

      if (!"secondaryDownloads" %in% names(products)) {
        return(products)
      }

      # The API lists a scene's bands under each of its product entries (the
      # bundle and the band-file product both carry the same
      # secondaryDownloads), so unnesting repeats every file once per product
      # type. De-duplicate so a file is queued once rather than N times.
      products %>%
        dplyr::select("secondaryDownloads") %>%
        tidyr::unnest(dplyr::all_of("secondaryDownloads")) %>%
        dplyr::distinct()
    },

    respawn = function(selected) {
      M2MDownloadOptions$new(
        session = private$session_,
        products = self$products,
        scene_list = private$scene_list_,
        selected = selected
      )
    }
  )
)


#' A download order in the M2M queue
#'
#' Returned by `M2MDownloadOptions$request()`, or reconnected to with
#' `M2MSession$download_queue(label)`. Tracks which files are ready to
#' download and which the distribution system is still preparing.
#'
#' @export
M2MDownloadQueue <- R6::R6Class(
  "M2MDownloadQueue",
  public = list(
    #' @field label The label identifying this order.
    label = NULL,

    #' @field available A tibble of files ready to download, with URLs.
    available = NULL,

    #' @field requested A tibble of files still being prepared.
    requested = NULL,

    #' @field queue_size Number of items the API still has queued.
    queue_size = NULL,

    #' @description Attach to a download order. Use
    #'   `M2MDownloadOptions$request()` or `M2MSession$download_queue()`.
    #' @param session The parent [M2MSession].
    #' @param label The order's label.
    #' @param available Optionally seed the ready-to-download files.
    #' @param n_preparing Number of records the API accepted for preparation.
    initialize = function(session, label, available = NULL, n_preparing = NULL) {
      private$session_ <- session
      self$label <- label
      self$available <- available %||% tibble::tibble()
      self$requested <- tibble::tibble()
      self$queue_size <- n_preparing %||% 0L

      # Reconnecting to an existing order: fetch its current state.
      if (is.null(available)) {
        self$refresh()
      }
      invisible(self)
    },

    #' @description Re-poll the M2M queue for this label, picking up files
    #'   that have finished preparing since the last check.
    #'
    #'   Unlike the other methods here, this updates the object in place
    #'   rather than returning a new one - newly ready files are added to
    #'   `$available` and `$requested`/`$queue_size` are replaced.
    #' @return The queue, invisibly.
    refresh = function() {
      queue <- ers_download_queue(
        m2m_session_handle(private$session_),
        label = self$label
      )

      # Accumulate rather than replace: the API drops entries once it has
      # handed them over, so a plain replacement would lose URLs collected
      # by an earlier poll or by the original download request.
      self$available <- private$merge_available(self$available, queue$available)
      self$requested <- queue$requested
      self$queue_size <- queue$queue_size
      invisible(self)
    },

    #' @description Whether every requested file is ready to download.
    #' @return `TRUE` if nothing is still being prepared.
    is_ready = function() {
      nrow(self$requested) == 0 && (self$queue_size %||% 0) == 0
    },

    #' @description Download every currently available file to disk. Call
    #'   `$refresh()` first if `$is_ready()` is `FALSE`.
    #' @param out_dir Directory to write files into. Created if missing.
    #' @return A tibble with one row per file: `entityId`, `url`, `path` and
    #'   `status` (`"downloaded"` or `"failed"`).
    retrieve = function(out_dir) {
      if (nrow(self$available) == 0) {
        stop("No downloads are available yet; try $refresh()")
      }

      ers_download_files(
        m2m_session_handle(private$session_),
        downloads = self$available,
        out_dir = out_dir
      )
    },

    #' @description Cancel this order, removing it from the M2M queue.
    #' @return The queue, invisibly.
    cancel = function() {
      ers_download_remove_order(m2m_session_handle(private$session_), self$label)
      invisible(self)
    },

    #' @description Print a summary of the order.
    #' @param ... Ignored.
    print = function(...) {
      cat("<M2MDownloadQueue>\n")
      cat("  Label:     ", self$label, "\n", sep = "")
      cat("  Available: ", format(nrow(self$available), big.mark = ","), " file(s) ready\n", sep = "")
      cat("  Preparing: ", format(self$queue_size %||% 0, big.mark = ","), "\n", sep = "")
      if (self$is_ready()) {
        m2m_print_next("$retrieve(out_dir)")
      } else {
        m2m_print_next("$refresh()", "$retrieve(out_dir)")
      }
      invisible(self)
    }
  ),
  private = list(
    session_ = NULL,

    merge_available = function(existing, incoming) {
      if (nrow(existing) == 0) {
        return(incoming)
      }
      if (nrow(incoming) == 0) {
        return(existing)
      }

      dplyr::bind_rows(existing, incoming) %>%
        dplyr::distinct(dplyr::across(dplyr::any_of(c("entityId", "url"))), .keep_all = TRUE)
    }
  )
)
