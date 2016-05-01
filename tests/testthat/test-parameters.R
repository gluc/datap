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
    p5: ...
  Pipe:
    type: pipe
    variables:
      v1: '@p3'
    GetData:
      type: processor
      function: GetData
      arguments:
        mp1: '@v1'
        mp2: '@p1'
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parameters %>% length, 2 )
  expect_equal(context$Tap$parameters , list(p1 = 10, p3 = NULL) )
  expect_equal(formals(context$Tap$tap), alist(p1 = 10, p3 = ) %>% as.pairlist())

})
