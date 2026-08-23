# Public product-selection and download-queue workflow objects. Transport
# details live in api-downloads.R.

#' Products available for download
#'
#' Returned by `M2MSceneSearch$products()`. Holds what the M2M API will let
#' you download for a set of scenes, and lets you narrow it before placing a
#' request. Not created directly.
#'
#' The API offers downloads at two granularities, and this object exposes
#' both:
#'
#' * **Individual files** - the bands and per-file products nested inside a
#'   product's `secondaryDownloads`, listed by `$bands()`. This is the
#'   default selection.
#' * **Whole products** - the scene-level entries the API lists, such as a
#'   Level-1 Product Bundle (a single `.tar` for the scene) or a
#'   full-resolution browse image, listed by `$scene_products()`. Some of
#'   these have no constituent files at all and so never appear in
#'   `$bands()`.
#'
#' Both are requestable. Use `$select_bands()` to queue individual files, or
#' `$select_products()` to queue whole products; `$selected()` always shows
#' what `$request()` will actually queue.
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
    #' @param selected An optional pre-narrowed band tibble.
    initialize = function(session, products, selected = NULL) {
      private$session_ <- session
      self$products <- products

      private$selected_ <- selected %||% private$flatten_bands(products)
      invisible(self)
    },

    #' @description The rows `$request()` will queue. Individual files by
    #'   default, or whole products after `$select_products()`.
    #' @return A tibble of downloadable items.
    selected = function() {
      private$selected_
    },

    #' @description The currently selected downloads. An alias for
    #'   `$selected()`, kept because the selection is individual bands in the
    #'   common case.
    #' @return A tibble of downloadable items.
    bands = function() {
      private$selected_
    },

    #' @description The whole-product entries the API lists for these scenes,
    #'   one row each, such as a Level-1 Product Bundle or a full-resolution
    #'   browse image.
    #'
    #'   These are requestable in their own right and are a different unit
    #'   from `$bands()`: a bundle is a single `.tar` holding the whole scene,
    #'   where `$bands()` would list its contents as separate files. Products
    #'   with no constituent files, such as browse imagery, appear only here.
    #' @return A tibble of products, without the `secondaryDownloads` column.
    scene_products = function() {
      if (nrow(self$products) == 0) {
        return(tibble::tibble())
      }
      dplyr::select(self$products, -dplyr::any_of("secondaryDownloads"))
    },

    #' @description Select the individual files whose `displayId` matches any
    #'   of the given patterns.
    #'
    #'   Each value is matched as a literal substring of `displayId`, which is
    #'   how `"B4"` picks out `..._SR_B4` without you writing the whole name.
    #'   Nothing is treated as pattern syntax, so a value containing
    #'   punctuation matches itself. See `$bands()` for the values. Matching
    #'   nothing warns and lists what was available.
    #'
    #'   Like `$select_products()`, this selects afresh from everything
    #'   available at that granularity rather than narrowing an existing
    #'   selection - use `$filter()` to narrow. It also does not filter on
    #'   availability; chain `$filter(bulkAvailable)` if you want only
    #'   bulk-downloadable files.
    #' @param patterns A character vector of patterns, e.g. `c("B4", "B5")`.
    #' @return A new [M2MDownloadOptions] with those files selected.
    select_bands = function(patterns) {
      all_bands <- private$flatten_bands(self$products)

      # stringr::fixed() rather than a regex: displayIds and the values people
      # paste in can contain punctuation, and a value should match itself
      # rather than being read as pattern syntax.
      hits <- Reduce(
        `|`,
        lapply(patterns, function(p) {
          stringr::str_detect(all_bands$displayId, stringr::fixed(p))
        })
      )

      matched <- all_bands[hits, , drop = FALSE]

      if (nrow(matched) == 0 && nrow(all_bands) > 0) {
        m2m_warn_no_match(patterns, "file names", all_bands$displayId)
      }

      private$respawn(matched)
    },

    #' @description Switch the selection to whole products rather than the
    #'   individual files inside them, optionally keeping only those whose
    #'   `productName` matches one of the given patterns.
    #'
    #'   Values are matched **exactly**, against either `productName` or
    #'   `productCode`, so a value copied out of `$scene_products()` selects
    #'   what you copied. Names contain characters such as parentheses that
    #'   would otherwise be read as pattern syntax, and they differ between
    #'   datasets - the bundle is "Landsat Collection 2 Level-2 Product
    #'   Bundle" for `landsat_ot_c2_l2` but "Standard Format" for `corona2` -
    #'   so read them off `$scene_products()` rather than assuming. Matching
    #'   nothing warns and lists what was available.
    #'
    #'   For anything looser, chain `$filter()`, which takes arbitrary
    #'   expressions:
    #'   `$select_products()$filter(grepl("Browse", productName))`.
    #'
    #'   As with `$select_bands()` this does not filter on availability;
    #'   chain `$filter(available)` to drop products the API has marked
    #'   unavailable.
    #' @param products An optional character vector of `productName` or
    #'   `productCode` values, matched exactly.
    #' @return A new [M2MDownloadOptions] with the products selected.
    select_products = function(products = NULL) {
      rows <- self$scene_products()

      if (!is.null(products) && nrow(rows) > 0) {
        matched <- rows[
          rows$productName %in% products | rows$productCode %in% products,
          ,
          drop = FALSE
        ]

        if (nrow(matched) == 0) {
          m2m_warn_no_match(products, "product names", rows$productName)
        }

        rows <- matched
      }

      private$respawn(rows)
    },

    #' @description Narrow the current selection using `dplyr::filter()`
    #'   semantics against the tibble returned by `$selected()`.
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

      requested <- api_download_request(
        private$session_$ers_session(),
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

    #' @description Print a summary of the available and selected downloads.
    #' @param ... Ignored.
    print = function(...) {
      n_products <- nrow(self$products)
      n_bands <- nrow(private$flatten_bands(self$products))

      cat("<M2MDownloadOptions>\n")
      cat("  Products: ", format(n_products, big.mark = ","),
          " whole product(s)\n", sep = "")
      cat("  Files:    ", format(n_bands, big.mark = ","),
          " individual file(s)\n", sep = "")
      cat("  Selected: ", format(nrow(private$selected_), big.mark = ","), "\n", sep = "")

      if (nrow(private$selected_) > 0) {
        m2m_print_next("$request(label)", "$select_bands(...)", "$select_products(...)")
      }
      invisible(self)
    }
  ),
  private = list(
    session_ = NULL,
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

    #' @description Attach to a download order. Use
    #'   `M2MDownloadOptions$request()` or `M2MSession$download_queue()`.
    #' @param session The parent [M2MSession].
    #' @param label The order's label.
    #' @param available Optionally seed the ready-to-download files.
    #' @param n_preparing Number of records the API accepted for preparation.
    initialize = function(session, label, available = NULL, n_preparing = NULL) {
      private$session_ <- session
      self$label <- label
      private$available_ <- available %||% tibble::tibble()
      private$requested_ <- tibble::tibble()
      private$queue_size_ <- n_preparing %||% 0L

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
    #'   rather than returning a new one, because it reflects changing
    #'   server-side queue state.
    #' @return The queue, invisibly.
    refresh = function() {
      queue <- api_download_queue(
        private$session_$ers_session(),
        label = self$label
      )

      # Accumulate rather than replace: the API drops entries once it has
      # handed them over, so a plain replacement would lose URLs collected
      # by an earlier poll or by the original download request.
      private$available_ <- private$merge_available(
        private$available_,
        queue$available
      )
      private$requested_ <- queue$requested
      private$queue_size_ <- queue$queue_size
      invisible(self)
    },

    #' @description The files that can be downloaded now. Readiness is based
    #'   on having a URL, regardless of which internal API bucket supplied it.
    #' @return A tibble of downloadable files.
    ready = function() {
      private$with_urls(
        dplyr::bind_rows(private$available_, private$requested_)
      )
    },

    #' @description The files still being prepared, which have no URL yet.
    #' @return A tibble of pending files.
    pending = function() {
      rows <- dplyr::bind_rows(private$available_, private$requested_)
      if (nrow(rows) == 0 || !"url" %in% names(rows)) {
        return(rows)
      }
      rows[!nzchar(rows$url %||% "") | is.na(rows$url), , drop = FALSE]
    },

    #' @description Whether every file in the order can be downloaded now.
    #' @return `TRUE` if nothing is still being prepared.
    is_ready = function() {
      nrow(self$pending()) == 0 && (private$queue_size_ %||% 0) == 0
    },

    #' @description Download every file that has a URL to disk. Call
    #'   `$refresh()` first if `$is_ready()` is `FALSE`.
    #'
    #'   Files are named as the server names them - the `.tar` of a product
    #'   bundle, the `.TIF` of a band - rather than after the `entityId` the
    #'   queue lists them under, which for a scene-level product is the scene
    #'   id and carries no extension.
    #'
    #'   Proxied downloads are reported back to the API afterwards, since it
    #'   does not serve them itself and would otherwise leave them in the
    #'   queue indefinitely.
    #' @param out_dir Directory to write files into. Created if missing.
    #' @return A tibble with one row per file: `entityId`, `downloadId`,
    #'   `url`, `path`, `size` and `status`. A `status` of `"expired"` means
    #'   the signed URL is no longer valid - `$refresh()` and retry.
    #' @param report_proxied Whether to mark proxied downloads complete.
    retrieve = function(out_dir, report_proxied = TRUE) {
      ready <- self$ready()

      if (nrow(ready) == 0) {
        stop("No downloads are available yet; try $refresh()")
      }

      session <- private$session_$ers_session()
      results <- api_download_files(session, downloads = ready, out_dir = out_dir)

      if (report_proxied) {
        proxied <- results[
          results$status == "downloaded" &
            !is.na(results$downloadId) &
            results$downloadId %in% private$proxied_ids(),
          ,
          drop = FALSE
        ]
        api_download_complete_proxied(session, proxied)
      }

      results
    },

    #' @description Move this order's scenes into the queue for processing,
    #'   via the `download-order-load` endpoint.
    #'
    #'   Unlike the other methods here this changes server-side state: it is
    #'   what starts a staged order being prepared. Follow it with
    #'   `$refresh()` to pick up URLs as they become ready.
    #' @param download_application Optional application name to scope the
    #'   order to.
    #' @return The queue, invisibly.
    prepare = function(download_application = NULL) {
      api_download_order_load(
        private$session_$ers_session(),
        label = self$label,
        download_application = download_application
      )
      invisible(self)
    },

    #' @description Remove individual files from this order, leaving the rest
    #'   of it in place. `$cancel()` drops the whole order instead.
    #'
    #'   Ids come from the `downloadId` column of `$ready()` or `$pending()`.
    #' @param download_id A vector of download ids.
    #' @param quiet Suppress the message shown before a large batch.
    #' @return The queue, invisibly.
    remove_items = function(download_id, quiet = FALSE) {
      api_download_remove_items(
        private$session_$ers_session(),
        download_id,
        quiet = quiet
      )
      invisible(self)
    },

    #' @description Cancel this order, removing it from the M2M queue.
    #' @return The queue, invisibly.
    cancel = function() {
      api_download_remove_order(private$session_$ers_session(), self$label)
      invisible(self)
    },

    #' @description Print a summary of the order.
    #' @param ... Ignored.
    print = function(...) {
      cat("<M2MDownloadQueue>\n")
      cat("  Label:     ", self$label, "\n", sep = "")
      cat("  Ready:     ", format(nrow(self$ready()), big.mark = ","), " file(s)\n", sep = "")
      cat("  Preparing: ", format(nrow(self$pending()), big.mark = ","), "\n", sep = "")
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
    available_ = NULL,
    requested_ = NULL,
    queue_size_ = NULL,

    # Rows that carry a usable URL.
    with_urls = function(rows) {
      if (nrow(rows) == 0 || !"url" %in% names(rows)) {
        return(tibble::tibble())
      }
      rows[nzchar(rows$url %||% "") & !is.na(rows$url), , drop = FALSE]
    },

    # Downloads the API is not serving itself, which have to be reported
    # complete once fetched. The queue marks these with a "Proxied" status.
    proxied_ids = function() {
      rows <- dplyr::bind_rows(private$available_, private$requested_)
      if (nrow(rows) == 0 || !all(c("downloadId", "statusText") %in% names(rows))) {
        return(integer())
      }
      rows$downloadId[grepl("proxied", rows$statusText, ignore.case = TRUE)]
    },

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
