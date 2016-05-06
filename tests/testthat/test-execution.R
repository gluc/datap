context("execution")


test_that("basic", {
  contextString <-"

Tap:
  type: tap
  Pipe:
    type: pipe
    Processor:
      type: processor
      function: identity
      arguments:
        - 29
  "
  context <- Load(textConnection(contextString))
  result <- context$Tap$tap()

  expect_equal(result, 29)

})



test_that("parameter in tap", {
  contextString <-"
Fabricated:
  type: structure
  variables:
    startDateDefault: 1990-01-01
  Ones:
    type: tap
    parameters:
      startDate: '@startDateDefault'
      endDate: '@Today()'
    GetOnes:
      type: processor
      function: Ones
      arguments:
        startDate: '@startDate'
        endDate: '@endDate'
        colname: Close

  "
  context <- Load(textConnection(contextString))
  result <- context$Fabricated$Ones$tap(startDate = "2015-01-02", endDate = "2015-01-05")

  expect_equal(length(result), 4)
  expect_true(all(result == 1))
  expect_equal(names(result), "Close")

})



test_that("junction", {
  contextString <- "
A:
  type: tap
  Pipe:
    type: pipe
    Sum:
      type: processor
      function: sum
      arguments:
        - '@inflow'
    Junction:
      type: junction
      function: as.numeric
      arguments:
        - '@inflow'
      B:
        type: processor
        function: identity
        arguments:
          - 2
      C:
        type: processor
        function: identity
        arguments:
          - 3
"


  context <- Load(textConnection(contextString))

  result <- context$A$tap()

  expect_equal(result, 5)

})



test_that("variables", {
  contextString <- "
A:
  type: tap
  Pipe:
    type: pipe
    Sum:
      type: processor
      function: sum
      arguments:
        - '@inflow'
    Junction:
      type: junction
      function: as.numeric
      arguments:
        - '@inflow'
      variables:
        secondArg: 4
      B:
        type: processor
        function: identity
        arguments:
          - 2
      C:
        type: processor
        function: identity
        arguments:
          - '@secondArg'
"


  context <- Load(textConnection(contextString))

  result <- context$A$tap()

  expect_equal(result, 6)

})



