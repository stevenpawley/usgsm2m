test_that("ers_dataset_search creates correct payload structure", {
  skip_on_cran()
  skip_if_offline("m2m.cr.usgs.gov")
    
  # Test with minimal parameters
  dataset_name <- "landsat"
  spatial_filter <- filter_spatial(ll_lon = -120, ll_lat = 40, ur_lon = -119, ur_lat = 41)
  temporal_filter <- filter_temporal("2020-07-01", "2020-07-31")
    
  expect_type(dataset_name, "character")
  expect_type(spatial_filter, "list")
  expect_type(temporal_filter, "list")

  # Call the function
  session <- ers_session()
  
  result <- ers_dataset_search(
    session,
    dataset_name = dataset_name,
    spatial_filter = spatial_filter,
    temporal_filter = temporal_filter
  )

  # Check that the result is a tibble
  expect_s3_class(result, "tbl_df")
})
