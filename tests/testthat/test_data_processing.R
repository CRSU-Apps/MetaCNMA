# Unit Tests

# Scope magrittr pipe
`%>%` <- magrittr::`%>%`

# Define test df
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

# Define expected output for test df
local_df_wide_exp <- data.frame(
  "study" = c(
    rep("Field 2023", 3)
  ),
  "arm" = c(
    "1",
    "2",
    "3"
  ),
  "components" = c(
    "Control",
    "A+B",
    "B+C"
  ),
  "events" = c(
    16,
    12,
    3
  ),
  "total" = c(
    32,
    24,
    6
  )
)

local_df_continuous_long <- import("../../data/continuous.Rds")
local_df_binary_long <- import("../../data/binary.Rds")

context("Testing data_processing")

test_that("get_required_columns() works as intended", {
  testthat::expect_error(
    get_required_columns(
      "unknown",
      FALSE,
      local_df_continuous_long
    ),
    "An error occured.+"
  )
  testthat::expect_warning(
    tryCatch({
      get_required_columns(
        "unknown",
        TRUE,
        local_df_wide
      )
    },
    error = function(e) {
      # Do Nothing
    })
    , "NAs+"
  )
  testthat::expect_error(
    suppressWarnings( # Expect Warning
      get_required_columns(
        "unknown",
        TRUE,
        local_df_wide
      )
    )
    , "An error occured.+"
  )
})

test_that("format_data() works with wide format", {
  # Wide
  testthat::expect_equal(
    format_data(local_df_wide, "binary"),
    local_df_wide_exp
  )
  #Extra Column
  tmp_df <- cbind(
    local_df_wide,
    "arm" = c(
      1
    )
  )
  testthat::expect_equal(
    format_data(local_df_wide, "binary"),
    local_df_wide_exp
  )
})

test_that("format_data() works with long format", {
  # Continuous
  # Create Expected data.frame
  local_df_continuous_long_exp <- local_df_continuous_long
  names(local_df_continuous_long_exp) <-
    tolower(names(local_df_continuous_long_exp))
  local_df_continuous_long_exp <-
    dplyr::select(
      local_df_continuous_long_exp, study, components, mean, sd, total
    )
  # Test
  expect_equal(
    format_data(local_df_continuous_long, "continuous"),
    local_df_continuous_long_exp
  )

  # Binary
  # Create Expected data.frame
  local_df_binary_long_exp <- local_df_binary_long
  names(local_df_binary_long_exp) <-
    tolower(names(local_df_binary_long_exp))
  local_df_binary_long_exp <-
    dplyr::select(
      local_df_binary_long_exp, study, components, events, total
    )
  # Test
  expect_equal(
    format_data(local_df_binary_long, "binary"),
    local_df_binary_long_exp
  )
})

# test_that("format_data() works with reactive", {
#   server <- function(input, output, session) {
#     reactive_data <- Data$new()$reactive()
#     reactive_freq <- Freq$new()$reactive()
#     reactive_data()$data_type("continuous")
#     reactive_data()$format("long")
#     reactive_data()$data(local_df_continuous_long)
#     reactive_data()$valid(TRUE)
#   }
#   testServer(server, {
#     format_data(reactive_data, reactive_freq)
#     expect_false(is.null(reactive_data()$data()))
#     tmp_df <- local_df_continuous_long
#     names(tmp_df) <- tolower(names(tmp_df))
#     expect_equal(data_reactives$formatted_data(), tmp_df)
#   })

#   server <- function(input, output, session) {
#     reactive_data <- Data$new()$reactive()
#     reactive_freq <- Freq$new()$reactive()
#     reactive_data()$data_type("binary")
#     reactive_data()$format("wide")
#     reactive_data()$data(local_df_wide)
#     reactive_data()$valid(TRUE)
#   }
#   testServer(server, {
#     format_data(reactive_data, reactive_freq)
#     expect_false(is.null(reactive_data()$data()))
#     tmp_df <- local_df_wide
#     names(tmp_df) <- tolower(names(tmp_df))
#     tmp_df <- tmp_df %>% dplyr::select(order(colnames(tmp_df)))
#     expect_equal(
#       dplyr::select(
#         data_reactives$formatted_data(),
#         order(colnames(data_reactives$formatted_data()))
#       ), tmp_df
#     )
#   })
# })