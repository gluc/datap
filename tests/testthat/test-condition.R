context("condition")

#devtools::test(filter = "condition")

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
      function: prod($inflow, 2)
      condition: $doProd
    Sum:
      type: processor
      function: sum($number, 3)
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
      function: subtract($inflow, 4)
      condition: $doSubtract
    Prod:
      type: processor
      function: prod($inflow, 2)
      condition: $doProd
    Sum:
      type: processor
      function: sum($number, 3)
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
      condition: $doPipe
      Subtract:
        type: processor
        function: subtract($inflow, 4)
      Prod:
        type: processor
        function: prod($inflow, 2)
    Sum:
      type: processor
      function: sum($number, 3)
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
      function: equals($inflow, 4)
      condition: $doWarn
    Sum:
      type: processor
      function: sum($number, 3)
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
      type: processor
      function: .Cache(joint = $joint, timeout = 3600)
      condition: $doCache
    Random:
      type: processor
      function: rnorm(n = 1, mean = 0, sd = 1)
"

  context <- Load(textConnection(contextString))

  res1 <- context$Tap$tap(TRUE)
  res2 <- context$Tap$tap(TRUE)
  expect_equal(res2, res1)

  res3 <- context$Tap$tap(FALSE)
  expect_false(res3 == res1)


})



test_that("parameter condition on factory", {

  contextString <- "
TapToBeCalledByRef:
  type: tap
  parameters:
    doCache: TRUE
  Pipe:
    type: pipe
    Cache:
      type: processor
      function: .Cache(joint = $joint, timeout = 3600)
      condition: $doCache
    Random:
      type: processor
      function: rnorm(n = 1, mean = 0, sd = 1)
TapRef:
  type: tap
  TapCall:
    type: processor
    function: Tap(context = $context, tapPath = 'TapToBeCalledByRef', doCache = FALSE)
"

  context <- Load(textConnection(contextString))

  r1 <- context$TapRef$tap()
  r2 <- context$TapRef$tap()

  expect_false(r2 == r1)

})




test_that("parameter condition on factory memoisation", {

  contextString <- "
Tap:
  type: tap
  parameters:
    doCache: TRUE
  Pipe:
    type: pipe
    Cache:
      type: processor
      function: .Cache(joint = $joint, timeout = 3600)
      condition: $doCache
    Forget:
      type: processor
      function: ForgetCache(joint = $joint, inflow = $inflow)
    Random:
      type: processor
      function: rnorm(n = 1, mean = 0, sd = 1)
"

  context <- Load(textConnection(contextString))

  res1 <- context$Tap$tap(TRUE)
  res2 <- context$Tap$tap(TRUE)
  expect_equal(res2, res1)

  res3 <- context$Tap$tap(FALSE)
  expect_false(res3 == res1)

  res4 <- context$Tap$tap(TRUE)
  expect_false(res3 == res4)

  res5 <- context$Tap$tap()
  expect_equal(res5, res4)


})



test_that("parameter condition on memoisation different params", {

  contextString <- "
Tap:
  type: tap
  parameters:
    mean:
    doCache: TRUE
  Pipe:
    type: pipe
    Cache:
      type: processor
      function: .Cache(joint = $joint, timeout = 3600)
      condition: $doCache
    Forget:
      type: processor
      function: ForgetCache(joint = $joint, inflow = $inflow)
    Random:
      type: processor
      function: rnorm(n = 1, mean = $mean, sd = 1)
"

  context <- Load(textConnection(contextString))

  res1 <- context$Tap$tap(mean = 2)
  res2 <- context$Tap$tap(mean = 2)
  expect_equal(res2, res1)

  res3 <- context$Tap$tap(FALSE, mean = 2)
  expect_false(res3 == res1)

  res4 <- context$Tap$tap(mean = 2)
  expect_false(res3 == res4)

  res5 <- context$Tap$tap(mean = 3)
  expect_false(res5 == res4)


})





test_that("parameter condition multiple", {

  contextString <- "


Tap:
  type: tap
  parameters:
    doA: TRUE
    doBandE: TRUE
    doC: TRUE
    doD: TRUE
    doF: TRUE
  Pipe:
    type: pipe
    A:
      type: processor
      function: sum( $inflow, 16)
      condition: $doA
    B:
      type: processor
      function: sum($inflow, 8)
      condition: $doBandE
    C:
      type: pipe
      condition: $doC
      D:
        type: processor
        function: sum($inflow, 4)
        condition: $doD
      E:
        type: processor
        function: sum($inflow, 2)
        condition: $doBandE
    F:
      type: processor
      function: identity(1)
      condition: $doF
"

  context <- Load(textConnection(contextString))

  r1 <- context$Tap$tap()
  expect_equal(r1, 31)

  r2 <- context$Tap$tap(doA = FALSE)
  expect_equal(r2, 15)

  r3 <- context$Tap$tap(doBandE = FALSE )
  expect_equal(r3, 21)

  r4 <- context$Tap$tap(doC = FALSE)
  expect_equal(r4, 25)

  r5 <- context$Tap$tap(doC = FALSE, doD = FALSE)
  expect_equal(r5, 25)

  r6 <- context$Tap$tap(doBandE = FALSE, doC = FALSE)
  expect_equal(r6, 17)

  r7 <- context$Tap$tap(doA = FALSE, doBandE = FALSE, doC = FALSE)
  expect_equal(r7, 1)

})

