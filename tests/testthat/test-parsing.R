context("parsing")


test_that("type missing", {
  contextString <- "
SPX:
  type: tap
  Doit:
    type: processor
    function: DoSomething
"

  errorReport <- CheckSyntax(textConnection(contextString))
  expect_true(!errorReport$hasErrors, "No syntax errors expected")

})




test_that("type missing", {
  contextString <- "
SPX:
  type: tap
  Doit:
    function: DoSomething
"

  errorReport <- CheckSyntax(textConnection(contextString))
  expect_true(errorReport$hasErrors, "Syntax error 2000 expected")

})
