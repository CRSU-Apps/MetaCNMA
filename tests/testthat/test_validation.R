# Unit Tests

local_df_wide <- data.frame(
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

context("Testing Column Validation validate_column_names()")

test_that("validateColumn works", {
  local_test_df <- data.frame(
    "Study" = "Field 2023",
    "Components" = "Control",
    "Events" = "6",
    "Total" = "12"
  )

  local_required_columns <- list("Study", "Components", "Events", "Total")
  expect_true(validate_column_names(local_test_df, local_required_columns))

  local_required_columns <- list("study", "components", "events", "total")
  expect_true(validate_column_names(local_test_df, local_required_columns))

  local_required_columns <- list("Study", "Components.1", "Events.1", "Total.1")
  expect_output(
    expect_false(
      validate_column_names(local_test_df, local_required_columns)
    )
  )

  local_required_columns <- list("Components", "Event", "Total")
  expect_output(
    expect_false(
      validate_column_names(local_test_df, local_required_columns)
    )
  )

  required_columns <- list("Study", "Component", "Event", "Total")
  expect_output(
    expect_false(
      validate_column_names(local_test_df, local_required_columns)
    )
  )

  expect_output(
    expect_false(
      validate_column_names(NULL, local_required_columns)
    )
  )

})

context("Testing file format validation is_valid_file_format()")

test_that("file format validation works",{
  expect_true(is_valid_file_format("csv"))
  expect_true(is_valid_file_format("xlsx"))
  expect_false(is_valid_file_format("xlx"))
  expect_false(is_valid_file_format("docx"))
  expect_false(is_valid_file_format("pdf"))
})

context("Testing validator helper function")

test_that("splitWidecolumns() works", {
  local_columns <- list("components.1", "components.2", "components.3")
  local_columns.split <- split_wide_columns(local_columns) # nolint: object_name
  expect_equal(
    local_columns.split$cols, list("components", "components", "components")
  )
  expect_equal(local_columns.split$col_numbers, list(1, 2, 3))
  expect_equal(local_columns.split$n_arms, 3)
})

test_that("validate_wide() works", {
  local_wide_required_columns <-
    list("Study", "Components.1", "Events.1", "Total.1")
  expect_true(validate_wide(local_df_wide, local_wide_required_columns))
  local_df_wide_fail <- local_df_wide %>% mutate("Events.5" = 7)
  expect_output(
    expect_false(
      validate_wide(local_df_wide_fail, local_wide_required_columns)
    )
  )
  local_df_wide_fail <- local_df_wide_fail %>%
    mutate("Components.5" = "A+C", "Total.5" = 14)
  expect_output(
    expect_false(
      validate_wide(local_df_wide_fail, local_wide_required_columns)
    )
  )
  local_df_wide_pass <- local_df_wide %>%
    mutate(
      "OtherColumn.1" = "Something",
      "OtherColumn.2" = "Something",
      "OtherColumn.3" = "Something"
    )
  expect_true(validate_wide(local_df_wide_pass, local_wide_required_columns))
})
