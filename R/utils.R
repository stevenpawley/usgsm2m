# Generate a scene list identifier scoped to this package. The M2M API
# requires a server-side scene list before download-options can be called;
# the R6 layer creates these transparently, so the id only needs to be
# unique within the user's account rather than meaningful.
m2m_new_list_id <- function() {
  paste0(
    "usgsm2m_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  )
}


# The host part of a URL, for deciding whether a request is going to the M2M
# API itself or to one of the hosts it proxies downloads to.
m2m_url_host <- function(url) {
  tolower(sub("^[a-z]+://([^/?#]+).*$", "\\1", url))
}


# Warn that a selector matched nothing, showing what was there to match.
#
# Product and band names vary between datasets - the bundle is "Landsat
# Collection 2 Level-2 Product Bundle" for one and "Standard Format" for
# another - so a pattern carried over from another dataset silently selects
# nothing, and the failure only surfaces later as "No products selected".
m2m_warn_no_match <- function(patterns, field, values) {
  available <- unique(values[!is.na(values)])
  shown <- available[seq_len(min(6L, length(available)))]

  warning(
    "Nothing matched ", paste0("\"", patterns, "\"", collapse = " or "),
    ".\n  Available ", field, ": ",
    paste(shown, collapse = ", "),
    if (length(available) > length(shown)) {
      paste0(", ... (", length(available), " in total)")
    },
    call. = FALSE
  )
}


# Convert a list of API records into a tibble, returning an empty tibble
# for an empty/absent record set.
coerce_records <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble())
  }

  records %>%
    jsonify::to_json() %>%
    jsonify::from_json() %>%
    dplyr::as_tibble()
}


# Format a "Next: $method()" hint for the print methods, which exist to make
# the pipeline order discoverable from the console.
m2m_print_next <- function(...) {
  steps <- c(...)
  cat("  Next:    ", paste0(steps, collapse = "  |  "), "\n", sep = "")
}
