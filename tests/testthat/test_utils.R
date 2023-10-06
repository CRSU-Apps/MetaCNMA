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

context("Testing Utils")

test_that("invalidate_data() works as should", {
  server <- function(input, output, session){
    reactive_data <- Data$new()$reactive()
    reactive_freq <- Freq$new()$reactive()
  }
  testServer(server, {
    invalidate_reactive(reactive_data, reactive_freq)
    expect_false(reactive_data()$valid())
    expect_false(reactive_freq()$valid())
  })
})
