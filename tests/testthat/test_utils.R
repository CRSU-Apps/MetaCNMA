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
local_dfContinousLong <- import("../../data/continous.Rds")
local_dfBinaryLong <- import("../../data/binary.Rds")

context("Testing Utils")

test_that("isDataValid() works as should", {
  local_globalData <- list(type = "continous")
  expect_false(isDataValid(local_globalData))
  local_globalData$format <- "long"
  expect_false(isDataValid(local_globalData))
  local_globalData$data <- local_dfWide
  expect_false(isDataValid(local_globalData))
  local_globalData$valid <- TRUE
  expect_true(isDataValid(local_globalData))
})

test_that("isDataValid() works with reactive", {
  server <- function(input, output, session){
    globalData <- reactiveValues()
    globalFreq <- reactiveValues()
  }
  testServer(server, {
    globalData$type = "continous"
    expect_false(isDataValid(globalData))
    globalData$format <- "long"
    expect_false(isDataValid(globalData))
    globalData$data <- local_dfWide
    expect_false(isDataValid(globalData))
    globalData$valid <- TRUE
    expect_true(isDataValid(globalData))
  })
})

test_that("invalidateData() works as should", {
  server <- function(input, output, session){
    globalData <- reactiveValues()
    globalFreq <- reactiveValues()
  }
  testServer(server, {
    invalidateData(globalData, globalFreq)
    expect_false(globalData$valid)
    expect_false(globalFreq$valid)
  })
  
})

test_that("formatData() works with reactive", {
  server <- function(input, output, session){
    globalData <- reactiveValues()
    globalFreq <- reactiveValues()
    globalData$type = "continous"
    globalData$format <- "long"
    globalData$data <- local_dfContinousLong
    globalData$valid <- TRUE
  }
  testServer(server, {
    formatData(globalData, globalFreq)
    expect_false(is.null(globalFreq$data))
    tmpDf <- local_dfContinousLong
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