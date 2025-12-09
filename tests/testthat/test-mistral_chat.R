# tests/testthat/test_mistral_chat.R
test_that("mistral_chat returns a response", {
  # Skip tests if httr is not installed
  testthat::skip_if_not_installed("httr")
  testthat::skip_if_not_installed("jsonlite")

  # Skip if API key is not set
  skip_if(!nzchar(Sys.getenv("MISTRAL_API_KEY")))

  # Test: Does the function return a character string?
  response <- mistral_chat("Hello")
  expect_type(response, "character")

  # Test: Does the function return a non-empty response?
  expect_false(nchar(response) == 0)

  # Test: Does the function throw an error if API key is missing?
  expect_error(
    mistral_chat("Hello", api_key = ""),
    "API key not provided"
  )
})

