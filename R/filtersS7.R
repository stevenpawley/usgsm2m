DateRange <- S7::new_class(
  "DateRange",
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

S7::method(as.list, DateRange) <- function(x, ...) {
  list(
    filterType = x@filterType,
    start = x@start,
    end = x@end
  )
}


#' @title Acquisition filter
#' @description A filter class for specifying a date range for product acquisition date.
#' @param start A character string representing the start date in 'YYYY-MM-DD' format
#' @param end A character string representing the end date in 'YYYY-MM-DD' format
#' @return An instance of the `daterangeFilter` class
#' @export
AcquisitionFilter <- S7::new_class(
  "acquisitionFilter",
  parent = daterangeFilter,
  properties = list(filterType = S7::new_property(default = "acquisitionFilter"))
)

#' @title Ingest filter
#' @description A filter class for specifying a date range for product ingest date.
#' @param start A character string representing the start date in 'YYYY-MM-DD' format,
#' or 'YYYY-MM-DDT HH:MM:SS' format.
#' @param end A character string representing the end date in 'YYYY-MM-DD' format,
#' or 'YYYY-MM-DDT HH:MM:SS' format.
#' @return An instance of the `IngestFilter` class
IngestFilter <- S7::new_class(
  "IngestFilter",
  properties = list(filterType = S7::new_property(default = "IngestFilter"))
)


#' @title Cloud cover filter
#' @description A filter class for specifying a range of acceptable cloud cover percentages.
#' @param min A numeric value representing the minimum acceptable cloud cover percentage (0-100)
#' @param max A numeric value representing the maximum acceptable cloud cover percentage (0-100)
#' @return An instance of the `CloudCoverFilter` class
CloudCoverFilter <- S7::new_class(
  "CloudCoverFilter",
  properties = list(
    min = S7::class_numeric,
    max = S7::class_numeric,
    filterType = S7::new_property(S7::class_character, default = "CloudCoverFilter")
  ),
  validator = function(self) {
    if (self@min < 0 || self@max > 100 || self@min > self@max) {
      stop("Cloud cover values must be between 0 and 100, and min must be less than or equal to max")
    }
  }
)


#' @title Metadata filter
#' @description A filter class for specifying metadata-based filtering criteria.
#' @param filterId A character string representing the metadata field to filter
#; on (e.g., 'platform', 'instrument').
#' You can discover available filter IDs by submitting a dataset-filters query
#' or viewing metadata in Earth Explorer under the Additional Information tab.
#' @param value A character string representing the value to filter for.
#' @param operand A character string representing the comparison operator. 
#' Can be either '=' for exact matches or 'like' for wildcard matches.
MetadataFilter <- S7::new_class(
  "MetadataFilter",
  properties = list(
    filterType = S7::new_property(S7::class_character, default = "valueFilter"),
    filterId = S7::class_character,
    value = S7::class_character,
    operand = S7::class_character
  ),
  validator = function(self) {
    valid_operands <- c("=", "like")
    if (!(self@operand %in% valid_operands)) {
      stop(paste("Operand must be one of:", paste(valid_operands, collapse = ", ")))
    }
  }
)


seasonalFilter

SceneDatasetFilter

SceneFilter

SpatialFilterMbr

SpatialFilterGeoJson

TemporalFilter
