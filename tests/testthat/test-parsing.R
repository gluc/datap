context("parsing of tree")


#devtools::test(filter = "parsing")

test_that("type missing", {
  contextString <- "
SPX:
  type: tap
  Doit:
    type: processor
    function: DoSomething()
"

  errorReport <- CheckSyntax(textConnection(contextString))
  expect_true(!errorReport$`.hasErrors`, "No syntax errors expected")

  context <- Load(textConnection(contextString))

  expect_equal(context$SPX$parameters %>% length, 0)


})







test_that("module", {
  contextString <- "
modules:
  type: module
  M1: &M1
    type: pipe
    Prod:
      type: processor
      function: prod($inflow, $prod)
    Sum:
      type: processor
      function: sum($number, $add)
Tap:
  type: tap
  Pipe: *M1
"

  context <- Load(textConnection(contextString))
  res <- context$Tap$tap(number = 3, prod = 4, add = 0)
  expect_equal(res, 12)

  res <- context$Tap$tap(5, prod = 4, add = 1)
  expect_equal(res, 24)

  expect_equal(formals(context$Tap$Pipe$Prod$tap), alist(number = , add = , prod = ) %>% as.pairlist())

  expect_equal(formals(context$Tap$Pipe$Sum$tap), alist(number = , add = ) %>% as.pairlist())


})




test_that("joint", {


  contextString <- "
Tap:
  type: tap
  Pro:
    type: processor
    function: GetJointName($joint)
  "

  context <- Load(textConnection(contextString))
  r1 <- context$Tap$tap()

  expect_equal(r1, "Pro")

})




test_that("joint", {
  contextString <- "
RandomCache:
  type: tap
  Pipe:
    type: pipe
    Cache:
      type: processor
      function: .Cache(joint = $joint, timeout = 3600)
    Random:
      type: processor
      function: rnorm(1)
"

  context <- Load(textConnection(contextString))
  r1 <- context$RandomCache$tap()
  r2 <- context$RandomCache$tap()

  expect_equal(r1, r2)

})


