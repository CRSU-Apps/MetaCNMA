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

#print(getwd())
local_dfContinuousLong <- import("../../data/continuous.Rds")
local_dfBinaryLong <- import("../../data/binary.Rds")

context("Testing data_processing")

test_that("formatData() works with reactive", {
  server <- function(input, output, session){
    globalData <- reactiveValues()
    globalFreq <- reactiveValues()
    globalData$type = "continuous"
    globalData$format <- "long"
    globalData$data <- local_dfContinuousLong
    globalData$valid <- TRUE
  }
  testServer(server, {
    formatData(globalData, globalFreq)
    expect_false(is.null(globalFreq$data))
    tmpDf <- local_dfContinuousLong
    names(tmpDf) <- tolower(names(tmpDf))
    expect_equal(globalFreq$data, tmpDf)
  })
  
  server <- function(input, output, session){
    globalData <- reactiveValues()
    globalFreq <- reactiveValues()
    globalData$type = "binary"
    globalData$format <- "wide"
    globalData$data <- local_dfWide
    globalData$valid <- TRUE
  }
  testServer(server, {
    formatData(globalData, globalFreq)
    expect_false(is.null(globalFreq$data))
    tmpDf <- local_dfWide
    names(tmpDf) <- tolower(names(tmpDf))
    tmpDf <- tmpDf %>% select(order(colnames(tmpDf)))
    expect_equal(select(globalFreq$data, order(colnames(globalFreq$data))), tmpDf)
  })
  
})