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
    GetData:
      type: processor
      function: GetData
      arguments:
        mp1: '@p1'
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parameters %>% length, 4 )
  expect_equal(context$Tap$parameters , list(p1 = 10, p2 = "extra", p3 = NULL, p4 = FALSE, p5 = "...") )
  expect_equal(formals(context$Tap$tap), list(p1 = 10, p2 = "extra", p3 = NULL, p4 = FALSE, p5 = "..."))

})
