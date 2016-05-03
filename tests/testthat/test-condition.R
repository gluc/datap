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
    Pipe:
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
    Prod:
      type: warning
      condition: '@doWarn'
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


