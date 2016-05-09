context("parameters")



test_that("parameter in tap", {
  contextString <-"

Tap:
  type: tap
  parameters:
    p1: 10
    p2: extra
    p3:
    p4: FALSE
  Pipe:
    type: pipe
    variables:
      v1: '$p3'
    GetData:
      type: processor
      function: GetData
      arguments:
        mp1: '$v1'
        mp2: '$p1'
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parameters %>% length, 2 )

  expect_equal(context$Tap$parameters , list(p1 = 10, p3 = NULL) )
  expect_equal(formals(context$Tap$tap), alist(p1 = 10, p3 = ) %>% as.pairlist())


})


test_that("parameter in tap", {
  contextString <-"

Tap:
  type: tap
  parameters:
    p1: 10
    p2: extra
    p3:
    p4: FALSE
    ...:
  Pipe:
    type: pipe
    variables:
      f1v1: '$p4'
    F1:
      type: processor
      function: DoF1
      arguments:
        f1p1: '$p2'
    F2:
      type: processor
      function: DoF2
      arguments:
        f2p1: '$p1'
        ...: '$...'
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parameters %>% length, 4 )

  expect_equal(context$Tap$parameters , list(p1 = 10, p2 = "extra", p4 = FALSE, `...` = NULL) )
  expect_equal(formals(context$Tap$tap), alist(p1 = 10, p2 = "extra", p4 = FALSE, ... =) %>% as.pairlist())

  expect_equal(context$Tap$Pipe$F1$parameters , list(p1 = 10, p2 = "extra", `...` = NULL) )
  expect_equal(formals(context$Tap$Pipe$F1$fun), alist(p1 = 10, p2 = "extra", ... =) %>% as.pairlist())

  expect_equal(context$Tap$Pipe$F2$parameters , list(p1 = 10, `...` = NULL) )
  expect_equal(formals(context$Tap$Pipe$F2$fun), alist(p1 = 10, ... =) %>% as.pairlist())

})


