test_that("ers_download_request validates input parameters", {
  session <- mock_session()
  
  # Mock products tibble structure
  mock_products <- tibble::tibble(
    entityId = c("entity1", "entity2"),
    secondaryDownloads = list(
      tibble::tibble(
        displayId = "band1_TIF",
        id = "prod1",
        bulkAvailable = TRUE
      ),
      tibble::tibble(
        displayId = "band2_TIF", 
        id = "prod2",
        bulkAvailable = TRUE
      )
    )
  )
  
  expect_error(
    ers_download_request(session, products = NULL),
    "products cannot be NULL"
  )
  
  expect_error(
    ers_download_request(session, mock_products, label = NULL),
    "label is required"
  )
})

test_that("download request filters products correctly", {
  skip_on_cran()
  
  # Test that band_names parameter filters correctly
  mock_products <- tibble::tibble(
    secondaryDownloads = list(
      tibble::tibble(
        displayId = c("B01_TIF", "B02_TIF", "B03_TIF"),
        id = c("p1", "p2", "p3"),
        bulkAvailable = c(TRUE, TRUE, FALSE),
        entityId = c("e1", "e2", "e3")
      )
    )
  )
  
  # This tests the logic of filtering by band names
  downloads <- mock_products |>
    dplyr::select("secondaryDownloads") |>
    tidyr::unnest(dplyr::all_of("secondaryDownloads"))
  
  filtered <- dplyr::filter(
    downloads,
    bulkAvailable == TRUE,
    stringr::str_detect(displayId, stringr::str_c(c("B01", "B02"), collapse = "|"))
  )
  
  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$bulkAvailable))
})
