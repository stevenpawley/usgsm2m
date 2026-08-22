# Coerce raw dataset responses into the stable tibble shapes returned by the
# package. Keeping this separate from api-datasets.R lets that file read as an
# inventory of endpoints rather than mixing transport and response shaping.

coerce_dataset_records <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble())
  }

  records %>%
    jsonify::to_json() %>%
    jsonify::from_json() %>%
    dplyr::as_tibble() %>%
    coerce_dataset_df()
}


# Coerce a raw dataset-record data.frame (as produced by jsonify::from_json())
# into its final tibble shape: unnest list columns, and reshape
# spatialBounds/temporalCoverage/catalogs into nested tibbles/lists.
coerce_dataset_df <- function(df) {
  list_cols <- names(df)[vapply(
    df,
    function(col) is.list(col) && !is.data.frame(col),
    logical(1)
  )]

  for (nm in setdiff(list_cols, "spatialBounds")) {
    col <- df[[nm]]
    lens <- lengths(col)

    if (all(lens <= 1)) {
      col[lens == 0] <- NA
      df[[nm]] <- unlist(col, use.names = FALSE)
    }
  }

  if ("spatialBounds" %in% names(df)) {
    if (inherits(df$spatialBounds, "data.frame")) {
      df$spatialBounds <- apply(
        df$spatialBounds,
        1,
        function(x) dplyr::as_tibble(t(x))
      )
    } else {
      df$spatialBounds <- lapply(
        df$spatialBounds,
        function(x) dplyr::as_tibble(x)
      )
    }
  }

  if ("temporalCoverage" %in% names(df)) {
    format_temporal <- function(x) {
      times <- x %>%
        stringr::str_remove_all("\\[|]|\"") %>%
        stringr::str_split(",")

      dplyr::tibble(start = times[[1]][1], end = times[[1]][2])
    }
    df$temporalCoverage <- lapply(df$temporalCoverage, format_temporal)
  }

  if ("catalogs" %in% names(df) && is.data.frame(df$catalogs)) {
    df$catalogs <- apply(df$catalogs, 1, function(x) x)
  }

  df
}


# Turn the dataset-filters payload into one row per filter field. Nested
# members remain list-columns so they cannot accidentally expand rows.
coerce_filter_fields <- function(filter_fields) {
  if (length(filter_fields) == 0) {
    return(tibble::tibble())
  }

  filter_fields <- lapply(filter_fields, function(field) {
    field <- field[!vapply(field, is.null, logical(1))]

    cells <- lapply(field, function(x) {
      if (length(x) == 1 && !is.list(x)) x else list(x)
    })

    tibble::as_tibble(cells)
  })

  purrr::list_rbind(filter_fields)
}
