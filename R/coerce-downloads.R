# Normalize API download responses and server-provided filenames.

coerce_download_request <- function(queue) {
  available <- lapply(
    queue$availableDownloads,
    function(x) {
      dplyr::as_tibble(t(x)) |>
        tidyr::unnest(dplyr::everything())
    }
  )

  list(
    available = purrr::list_rbind(available),
    n_new = length(queue$newRecords),
    n_duplicate = length(queue$duplicateProducts)
  )
}

coerce_download_name <- function(resp, url, entityId) {
  from_header <- coerce_content_disposition_name(
    httr2::resp_header(resp, "content-disposition")
  )
  if (!is.null(from_header)) {
    return(from_header)
  }

  from_url <- coerce_url_name(httr2::resp_url(resp) %||% url)
  if (!is.null(from_url)) {
    return(from_url)
  }

  sub(
    "_(TIF|TIFF|JPG|PNG|XML|TXT|JSON|TAR|ZIP|GZ|MET|HDF|IMG)$",
    ".\\1",
    entityId
  )
}


# Extract the preferred filename from a Content-Disposition header.
coerce_content_disposition_name <- function(header) {
  if (is.null(header) || is.na(header) || !nzchar(header)) {
    return(NULL)
  }

  extended <- regmatches(
    header,
    regexpr("filename\\*\\s*=\\s*[^;]+", header, ignore.case = TRUE)
  )
  if (length(extended) == 1) {
    value <- sub("^[^=]*=\\s*", "", extended)
    value <- sub("^[^']*'[^']*'", "", value)
    return(coerce_safe_filename(coerce_url_decode(value)))
  }

  plain <- regmatches(
    header,
    regexpr("filename\\s*=\\s*(\"[^\"]*\"|[^;]+)", header, ignore.case = TRUE)
  )
  if (length(plain) == 1) {
    value <- sub("^[^=]*=\\s*", "", plain)
    value <- gsub("^\"|\"$", "", trimws(value))
    return(coerce_safe_filename(value))
  }

  NULL
}


# Extract a plausible filename from a URL path.
coerce_url_name <- function(url) {
  if (is.null(url) || is.na(url) || !nzchar(url)) {
    return(NULL)
  }

  path <- sub("[?#].*$", "", url)
  path <- sub("^[A-Za-z][A-Za-z0-9+.-]*://[^/]*", "", path)

  if (!nzchar(path) || path == "/") {
    return(NULL)
  }

  name <- coerce_url_decode(basename(path))

  if (!grepl("\\.[A-Za-z0-9]{1,8}$", name)) {
    return(NULL)
  }

  coerce_safe_filename(name)
}


coerce_safe_filename <- function(name) {
  name <- basename(gsub("\\\\", "/", trimws(name)))
  name <- gsub("[/\\\\]", "_", name)

  if (!nzchar(name) || name %in% c(".", "..")) {
    return(NULL)
  }

  name
}


coerce_url_decode <- function(x) {
  tryCatch(utils::URLdecode(x), error = function(e) x)
}
