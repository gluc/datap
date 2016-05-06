context("condition")


test_that("parameter condition on processor", {
  contextString <- "
Tap:
  type: tap
  parameters:
    number:
    doProd: TRUE
  Pipe:
    type: pipe
    Prod:
      type: processor
      function: prod
      arguments:
        - '@inflow'
        - 2
      condition: '@doProd'
    Sum:
      type: processor
      function: sum
      arguments:
        - '@number'
        - 3
"

  context <- Load(textConnection(contextString))

  res <- context$Tap$tap(6, TRUE)

  expect_equal(res, 18)


  res <- context$Tap$tap(6, FALSE)

  expect_equal(res, 9)


})




test_that("variable condition on processor", {
  contextString <- "
Tap:
  type: tap
  parameters:
    number:
  variables:
    doProd: TRUE
    doSubtract: FALSE
  Pipe:
    type: pipe
    Subtract:
      type: processor
      function: subtract
      arguments:
        - '@inflow'
        - 4
      condition: '@doSubtract'
    Prod:
      type: processor
      function: prod
      arguments:
        - '@inflow'
        - 2
      condition: '@doProd'
    Sum:
      type: processor
      function: sum
      arguments:
        - '@number'
        - 3
"

  context <- Load(textConnection(contextString))

  res <- context$Tap$tap(6)

  expect_equal(res, 18)


})




test_that("parameter condition on pipe", {
  contextString <- "
Tap:
  type: tap
  parameters:
    number:
    doPipe: TRUE
  Pipe:
    type: pipe
    ConditionalPipe:
      type: pipe
      condition: '@doPipe'
      Subtract:
        type: processor
        function: subtract
        arguments:
          - '@inflow'
          - 4
      Prod:
        type: processor
        function: prod
        arguments:
          - '@inflow'
          - 2
    Sum:
      type: processor
      function: sum
      arguments:
        - '@number'
        - 3
"

  context <- Load(textConnection(contextString))

  res <- context$Tap$tap(6)
  expect_equal(res, 14)

  res <- context$Tap$tap(6, TRUE)
  expect_equal(res, 14)

  res <- context$Tap$tap(6, FALSE)
  expect_equal(res, 9)


})





test_that("parameter condition on warning", {
  contextString <- "
Tap:
  type: tap
  parameters:
    number:
    doWarn: TRUE
  Pipe:
    type: pipe
    Warn:
      type: warning
      function: equals
      arguments:
        - '@inflow'
        - 4
      condition: '@doWarn'
    Sum:
      type: processor
      function: sum
      arguments:
        - '@number'
        - 3
"

  context <- Load(textConnection(contextString))

  expect_warning(context$Tap$tap(3))

  res <- context$Tap$tap(3, FALSE)

  expect_equal(res, 6)


})




test_that("parameter condition on factory", {
  contextString <- "
Tap:
  type: tap
  parameters:
    doCache: TRUE
  Pipe:
    type: pipe
    Cache:
      type: factory
      function: Cache
      arguments:
        f: '@inflowfun'
        timeout: 3600
      condition: '@doCache'
    Random:
      type: processor
      function: rnorm
      arguments:
        'n': 1
        mean: 0
        sd: 1
"

  context <- Load(textConnection(contextString))

  res1 <- context$Tap$tap(TRUE)
  res2 <- context$Tap$tap(TRUE)
  expect_equal(res2, res1)

  res3 <- context$Tap$tap(FALSE)
  expect_false(res3 == res1)


})


