test_that("filter_spatial creates correct spatial filter structure", {
  result <- filter_spatial(
    ll_lat = 40.0, 
    ll_lon = -120.0, 
    ur_lat = 41.0, 
    ur_lon = -119.0
  )
  
  expect_type(result, "list")
  expect_equal(result$filterType, "mbr")
  expect_equal(result$lowerLeft$latitude, 40.0)
  expect_equal(result$lowerLeft$longitude, -120.0)
  expect_equal(result$upperRight$latitude, 41.0)
  expect_equal(result$upperRight$longitude, -119.0)
})

test_that("filter_temporal creates correct temporal filter structure", {
  result <- filter_temporal("2020-01-01", "2020-12-31")
  
  expect_type(result, "list")
  expect_equal(result$start, "2020-01-01")
  expect_equal(result$end, "2020-12-31")
  expect_named(result, c("start", "end"))
})

test_that("filter_cloud creates correct cloud filter with defaults", {
  result <- filter_cloud()
  
  expect_type(result, "list")
  expect_equal(result$min, 0)
  expect_equal(result$max, 100)
  expect_named(result, c("min", "max"))
})

test_that("filter_cloud creates correct cloud filter with custom values", {
  result <- filter_cloud(min = 10, max = 50)
  
  expect_equal(result$min, 10)
  expect_equal(result$max, 50)
})

test_that("filter functions validate input ranges", {
  # These would be enhanced versions of your filter functions with validation
  expect_error(filter_cloud(min = -5), "Minimum cloud cover cannot be negative")
  expect_error(filter_cloud(max = 150), "Maximum cloud cover cannot exceed 100")
  expect_error(filter_cloud(min = 60, max = 40), "Minimum cannot be greater than maximum")
})
