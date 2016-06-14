context("execution")



test_that("GetVariableVal", {
  contextString <- "

Tap:
  type: tap
  variables:
    x: a
    'y': 3
    z: sum(3, 5)
  Processor:
    type: processor
    function: identity(29)
"

  context <- Load(textConnection(contextString))
  context$Tap$variablesE %>% is.list %>% expect_true
  context$Tap$variablesE %>% names %>% expect_equal(c("x", "y", "z"))
  context$Tap$variablesE %>% lapply(function(e) e$value) %>% expect_equal(list(x = "a", y = 3, z = 8))

  context$Tap$Processor$functionE$value %>% expect_equal(29)

})



test_that("basic", {
  contextString <-"

Tap:
  type: tap
  Processor:
    type: processor
    function: .identity(29)
  "
  context <- Load(textConnection(contextString))
  result <- context$Tap$tap()

  expect_equal(result, 29)

})



test_that("basic pipe", {
  contextString <-"

Tap:
  type: tap
  Pipe:
    type: pipe
    Processor:
      type: processor
      function: .identity(29)
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
      startDate: '$startDateDefault'
      endDate: '$Today()'
    GetOnes:
      type: processor
      function: Ones(startDate = $startDate, endDate = $endDate, colname = 'Close')

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
      function: sum($inflow)
    Junction:
      type: junction
      function: as.numeric($inflow)
      B:
        type: processor
        function: identity(2)
      C:
        type: processor
        function: identity(3)
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
      function: sum($inflow)
    Junction:
      type: junction
      function: as.numeric($inflow)
      variables:
        secondArg: 4
      B:
        type: processor
        function: identity(2)
      C:
        type: processor
        function: identity($secondArg)
"


  context <- Load(textConnection(contextString))

  result <- context$A$tap()

  expect_equal(result, 6)

})



test_that("basic", {
  contextString <-"

Tap:
  type: tap
  parameters:
    p1:
    ...:
  Processor:
    type: processor
    function: sum($p1, $...)
  "
  context <- Load(textConnection(contextString))
  result <- context$Tap$tap(2, 3)

  expect_equal(result, 5)


  result <- context$Tap$tap(2, 3, 4)

  expect_equal(result, 9)

})

