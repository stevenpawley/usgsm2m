test_that("ers_login returns proper session object structure", {
  skip_if_not(nzchar(Sys.getenv("M2M_USERNAME")) && nzchar(Sys.getenv("M2M_TOKEN")),
              "M2M credentials not available")
  
  session <- ers_session()
  
  expect_s3_class(session, "ers_session")
  expect_named(session, c("api_key", "service"))
  expect_type(session$api_key, "character")
  expect_match(session$service, "^https://")
})

test_that("ers_login handles missing credentials gracefully", {
  # Temporarily unset environment variables
  old_username <- Sys.getenv("M2M_USERNAME")
  old_token <- Sys.getenv("M2M_TOKEN")
  
  Sys.unsetenv("M2M_USERNAME")
  Sys.unsetenv("M2M_TOKEN")
  
  expect_error(
    ers_session(username = "", token = ""),
    "Login was unsuccessful"
  )
  
  # Restore environment variables
  if (nzchar(old_username)) Sys.setenv(M2M_USERNAME = old_username)
  if (nzchar(old_token)) Sys.setenv(M2M_TOKEN = old_token)
})

test_that("ers_login validates input parameters", {
  expect_error(ers_session(username = NULL), regexp = "Username cannot be NULL")
  expect_error(ers_session(token = NULL), regexp = "Token cannot be NULL")
})