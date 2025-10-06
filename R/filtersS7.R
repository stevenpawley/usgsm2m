daterangeFilter <- S7::new_class(
  "daterangeFilter",
  properties = list(
    start = S7::class_character,
    end = S7::class_character,
    filterType = S7::new_property(S7::class_character, default = "DateRange")
  ),
  validator = function(self) {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", self@start) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", self@end)) {
      stop("Start and end dates must be in 'YYYY-MM-DD' format")
    }
  },
  abstract = TRUE
)

S7::method(as.list, daterangeFilter) <- function(x, ...) {
  list(
    filterType = x@filterType,
    start = x@start,
    end = x@end
  )
}


#' @title Aquisition filter
#' @description A filter class for specifying a date range for product aquisition date.
#' @param start A character string representing the start date in 'YYYY-MM-DD' format
#' @param end A character string representing the end date in 'YYYY-MM-DD' format
#' @return An instance of the `daterangeFilter` class
#' @export
aquisitionFilter <- S7::new_class(
  "aquisitionFilter",
  parent = daterangeFilter,
  properties = list(filterType = S7::new_property(default = "aquisitionFilter"))
)


CloudCoverFilter

IngestFilter

MetadataFilter

seasonalFilter

SceneDatasetFilter

SceneFilter

SpatialFilterMbr

SpatialFilterGeoJson

TemporalFilter
