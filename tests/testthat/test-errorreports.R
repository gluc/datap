context("datap syntax")

test_that("syntax error report", {
  contextString <- "
SPX:
  type: tap
  attributes:
    longname: 'S&P 500 daily close'
    description: |
      Quandl, fill missing values with Yahoo.
      Backfill weekends and holidays.
      Cache for an hour.
      Warn if newest value older than a day.
  parameters:
    #parameterName: defaultArgument
    dteRange: 1990-01-01/2010-01-01
  variables:
    #variableName: value
    series: '@series'
    maxNaRatio: '@maxNaRatioDefault'
    yahooSymbol: '^GSPC'
    quandlCode: 'YAHOO/INDEX_GSPC'
"

  errorReport <- CheckSyntax(textConnection(contextString))
  expect_true(errorReport$`.hasErrors`, "Expected error 1000")
  expect_equal(errorReport$errorCount, 1)
  expect_equal(errorReport$SPX$`.errors`$upstream$mincount$code, "1000")

})


test_that("syntax error report no error", {
  contextString <- "
SPX:
  type: tap
  pipe:
    type: pipe
    source:
      type: processor
      function: DoSomething
"

  errorReport <- CheckSyntax(textConnection(contextString))

  expect_true(!errorReport$`.hasErrors`, "No error expected")

})

test_that("reference error report ok", {
  contextString <- "
SPX:
  type: tap
  pipe:
    type: pipe
    source:
      type: processor
      function: DoSomething
      arguments:
        arg1: 25
"

  errorReport <- CheckReferences(textConnection(contextString))
  expect_true(!errorReport$`.hasErrors`, "No reference errors expected")


})



test_that("reference error report error", {
  contextString <- "
SPX:
  type: tap
  pipe:
    type: pipe
    source:
      type: processor
      function: DoSomething
      arguments:
        arg1: '@value1'
"

  errorReport <- CheckReferences(textConnection(contextString))

  expect_true(errorReport$`.hasErrors`, "Expected error 3000")
  expect_equal(errorReport$errorCount, 1)
  expect_equal(errorReport$SPX$pipe$source$`.errors`$references$`@value1`$code, "3000")

})


test_that("reference error report multiple error", {
  contextString <- "
SPX:
  type: tap
  pipe:
    type: pipe
    source:
      type: processor
      function: DoSomething
      arguments:
        arg1: '@value1'
        arg2: '@value2'
"

  errorReport <- CheckReferences(textConnection(contextString))

  expect_true(errorReport$`.hasErrors`, "Expected error 3000")
  expect_equal(errorReport$errorCount, 1)
  expect_equal(errorReport$SPX$pipe$source$`.errors`$references$`@value1`$code, "3000")
  expect_equal(errorReport$SPX$pipe$source$`.errors`$references$`@value2`$code, "3000")

})



test_that("aggregation error", {
  contextString <- "
SPX:
  type: tap
  pipe:
    type: pipe
    source:
      type: processor
      function: DoSomething
    junction:
      type: junction
      function: join
      doA:
        type: processor
        function: doA
      doB:
        type: processor
        function: doB
    illegal:
      type: processor
      function: doIllegal
"

  errorReport <- CheckAggregation(textConnection(contextString))

  expect_true(errorReport$`.hasErrors`, "Expected error 4000")
  expect_equal(errorReport$errorCount, 1)
  expect_equal(errorReport$SPX$pipe$illegal$`.errors`$aggregation$downstream$code, "4000")

})




test_that("aggregation complex error", {
  contextString <- "
SPX:
  type: tap
  pipe:
    type: pipe
    source:
      type: processor
      function: DoSomething
    pipe1:
      type: pipe
      doA:
        type: processor
        function: doA
      doB:
        type: processor
        function: doB
      pipe2:
        type: pipe
        doC:
          type: processor
          function: doC
        junction:
          type: junction
          function: doJunction
          pipe3:
            type: pipe
            doD:
              type: processor
              function: doD
            doE:
              type: processor
              function: doE
          doF:
            type: processor
            function: doF
    illegal:
      type: processor
      function: doIllegal
"

  errorReport <- CheckAggregation(textConnection(contextString))

  expect_true(errorReport$`.hasErrors`, "Expected error 4000")
  expect_equal(errorReport$errorCount, 1)
  expect_equal(errorReport$SPX$pipe$illegal$`.errors`$aggregation$downstream$code, "4000")

})


