test_that("filter_spatial creates correct spatial filter structure", {
  result <- filter_spatial(
    ll_lon = -120.0,
    ll_lat = 40.0,
    ur_lon = -119.0,
    ur_lat = 41.0
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

test_that("filter_temporal rejects malformed dates", {
  expect_error(filter_temporal("2020/01/01", "2020-12-31"), "YYYY-MM-DD")
  expect_error(filter_temporal("2020-01-01", "Dec 2020"), "YYYY-MM-DD")
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
  expect_error(filter_cloud(min = -5), "Minimum cloud cover cannot be negative")
  expect_error(filter_cloud(max = 150), "Maximum cloud cover cannot exceed 100")
  expect_error(filter_cloud(min = 60, max = 40), "Minimum cannot be greater than maximum")
})

test_that("metadata filters build the documented payload shapes", {
  value <- filter_metadata_value("abc123", "045")
  expect_equal(value$filterType, "value")
  expect_equal(value$filterId, "abc123")
  expect_equal(value$value, "045")
  expect_equal(value$operand, "=")

  between <- filter_metadata_between("abc123", 40, 45)
  expect_equal(between$filterType, "between")
  expect_equal(between$firstValue, 40)
  expect_equal(between$secondValue, 45)

  combined <- filter_metadata_and(value, between)
  expect_equal(combined$filterType, "and")
  expect_length(combined$childFilters, 2)

  expect_equal(filter_metadata_or(value)$filterType, "or")
})

test_that("filter_metadata_value rejects an unsupported operand", {
  expect_error(filter_metadata_value("abc123", "045", operand = "~"))
})
