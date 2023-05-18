# Unit Tests

local_dfWide <- data.frame(
  "Study" = "Field 2023",
  "Components.1" = "Control",
  "Events.1" = 16,
  "Total.1" = 32,
  "Components.2" = "A+B",
  "Events.2" = 12,
  "Total.2" = 24,
  "Components.3" = "B+C",
  "Events.3" = 3,
  "Total.3" = 6
)

context("Testing Column Validation validateColumnNames()")

test_that("validateColumn works", {
  local_testDf <- data.frame(
    "Study" = "Field 2023",
    "Components" = "Control",
    "Events" = "6",
    "Total" = "12"
  )
  
  local_requiredColumns <- list("Study", "Components", "Events", "Total")
  expect_true(validateColumnNames(local_testDf, local_requiredColumns))
  
  local_requiredColumns <- list("study", "components", "events", "total")
  expect_true(validateColumnNames(local_testDf, local_requiredColumns))
  
  local_requiredColumns <- list("Study", "Components.1", "Events.1", "Total.1")
  expect_output(expect_false(validateColumnNames(local_testDf, local_requiredColumns)))
  
  local_requiredColumns <- list("Components", "Event", "Total")
  expect_output(expect_false(validateColumnNames(local_testDf, local_requiredColumns)))
  
  requiredColumns <- list("Study", "Component", "Event", "Total")
  expect_output(expect_false(validateColumnNames(local_testDf, local_requiredColumns)))
  
  expect_output(expect_false(validateColumnNames(NULL, local_requiredColumns)))
  
  
})

context("Testing file format validation isValidFileFormat()")

test_that("file format validation works",{
  expect_true(isValidFileFormat("csv"))
  expect_true(isValidFileFormat("xlsx"))
  expect_false(isValidFileFormat("xlx"))
  expect_false(isValidFileFormat("docx"))
  expect_false(isValidFileFormat("pdf"))
})

context("Testing validator helper function")

test_that("splitWidecolumns() works", {
  local_columns = list("components.1", "components.2", "components.3")
  local_columns.split <- splitWideColumns(local_columns)
  expect_equal(local_columns.split$cols, list("components", "components", "components"))
  expect_equal(local_columns.split$colNumbers, list(1, 2, 3))
  expect_equal(local_columns.split$nArms, 3)
})

test_that("validateWide() works", {
  local_wideRequiredColumns <- list("Study", "Components.1", "Events.1", "Total.1")
  expect_true(validateWide(local_dfWide, local_wideRequiredColumns))
  local_dfWideFail <- local_dfWide %>% mutate("Events.5" = 7)
  expect_output(expect_false(validateWide(local_dfWideFail, local_wideRequiredColumns)))
  local_dfWideFail <- local_dfWideFail %>% mutate("Components.5" = "A+C", "Total.5" = 14)
  expect_output(expect_false(validateWide(local_dfWideFail, local_wideRequiredColumns)))
  local_dfWidePass <- local_dfWide %>% mutate("OtherColumn.1" = "Something", "OtherColumn.2" = "Something", "OtherColumn.3" = "Something")
  expect_true(validateWide(local_dfWidePass, local_wideRequiredColumns))
})
