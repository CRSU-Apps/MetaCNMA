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

local_df_continuous_long <- import("../../data/continuous.Rds")
local_df_binary_long <- import("../../data/binary.Rds")

context("Testing data_processing")

test_that("format_data() works with reactive", {
  server <- function(input, output, session) {
    reactive_data <- Data$new()$reactive()
    reactive_freq <- Freq$new()$reactive()
    reactive_data()$data_type("continuous")
    reactive_data()$format("long")
    reactive_data()$data(local_df_continuous_long)
    reactive_data()$valid(TRUE)
  }
  testServer(server, {
    format_data(reactive_data, reactive_freq)
    expect_false(is.null(reactive_data()$data()))
    tmp_df <- local_df_continuous_long
    names(tmp_df) <- tolower(names(tmp_df))
    expect_equal(data_reactives$formatted_data(), tmp_df)
  })

  server <- function(input, output, session) {
    reactive_data <- Data$new()$reactive()
    reactive_freq <- Freq$new()$reactive()
    reactive_data()$data_type("binary")
    reactive_data()$format("wide")
    reactive_data()$data(local_df_wide)
    reactive_data()$valid(TRUE)
  }
  testServer(server, {
    format_data(reactive_data, reactive_freq)
    expect_false(is.null(reactive_data()$data()))
    tmp_df <- local_df_wide
    names(tmp_df) <- tolower(names(tmp_df))
    tmp_df <- tmp_df %>% dplyr::select(order(colnames(tmp_df)))
    expect_equal(
      dplyr::select(
        data_reactives$formatted_data(),
        order(colnames(data_reactives$formatted_data()))
      ), tmp_df
    )
  })
})