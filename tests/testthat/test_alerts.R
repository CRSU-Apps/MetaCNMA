# Unit test (non-shiny)

context("Test error alert (non-shiny)")
test_that("Error alert produces output when shiny not running", {
  expect_output(error_alert("test", "Error"), paste("Error: test"))
})