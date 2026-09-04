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

  m2m_bind_records(lapply(records, m2m_flatten_record))
}


# Format a "Next: $method()" hint for the print methods, which exist to make
# the pipeline order discoverable from the console.
m2m_print_next <- function(...) {
  steps <- c(...)
  cat("  Next:    ", paste0(steps, collapse = "  |  "), "\n", sep = "")
}


# Flatten one API record into a named list of scalars.
#
# The M2M API returns optional fields inconsistently - a file's `checksum`
# object carries a null `value` for some files and is absent altogether for
# others - and a record set mixing the two cannot be row-bound or unnested,
# which broke `$products()` for any order containing such a scene. Nested
# objects are therefore hoisted into `parent_child` columns holding plain
# scalars, so every record has the same shape whatever the API omitted.
#
# Anything that is not a single object or scalar - an array of values, or an
# array of objects such as a nested `secondaryDownloads` - is wrapped so it
# becomes a list column rather than being spread across columns.
m2m_flatten_record <- function(record, prefix = NULL) {
  if (length(record) == 0) {
    return(list())
  }

  # a bare value where an object was expected has no fields to become
  # columns, and silently dropping it would lose the record
  if (is.null(names(record))) {
    stop("Cannot coerce an unnamed API record into a table row", call. = FALSE)
  }

  names(record) <- if (is.null(prefix)) {
    names(record)
  } else {
    paste0(prefix, "_", names(record))
  }

  flat <- lapply(names(record), function(name) {
    value <- record[[name]]

    if (length(value) == 0) {
      return(stats::setNames(list(NA), name))
    }

    if (is.list(value) && !is.null(names(value))) {
      return(m2m_flatten_record(value, prefix = name))
    }

    if (is.list(value) || length(value) > 1) {
      return(stats::setNames(list(list(value)), name))
    }

    stats::setNames(list(value), name)
  })

  unlist(flat, recursive = FALSE)
}


# Row-bind flattened records, filling fields a record did not carry with NA.
m2m_bind_records <- function(flat) {
  fields <- unique(unlist(lapply(flat, names), use.names = FALSE))

  columns <- lapply(fields, function(field) {
    values <- lapply(flat, function(record) {
      if (is.null(record[[field]])) NA else record[[field]]
    })

    # a field wrapped by m2m_flatten_record() stays a list column; the NA
    # standing in for records that lacked it has to be wrapped to match
    if (any(vapply(values, is.list, logical(1)))) {
      lapply(values, function(value) if (is.list(value)) value[[1]] else value)
    } else {
      unlist(values, use.names = FALSE)
    }
  })

  tibble::as_tibble(stats::setNames(columns, fields))
}


# Write a response body to disk.
#
# A response from req_perform_connection() has not read its body yet, so it is
# streamed a chunk at a time rather than held in memory - a product bundle can
# be several gigabytes. A response that already carries its body in memory
# (nothing to stream) is written in one go.
m2m_write_body <- function(resp, path, chunk_kb = 1024) {
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)

  if (!inherits(resp$body, "StreamingBody")) {
    if (httr2::resp_has_body(resp)) {
      writeBin(httr2::resp_body_raw(resp), con)
    }
    return(invisible(path))
  }

  repeat {
    chunk <- httr2::resp_stream_raw(resp, kb = chunk_kb)
    if (length(chunk) == 0) {
      break
    }
    writeBin(chunk, con)
  }

  invisible(path)
}
