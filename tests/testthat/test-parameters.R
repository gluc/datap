context("parameters")



test_that("parameter in tap", {
  contextString <-"

Tap:
  type: tap
  Pipe:
    type: pipe
    variables:
      v1: $p3
      v2: 3
    GetData:
      type: processor
      function: GetData(mp1 = $v1, mp2 = $p1, mp3 = $v2)
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parameters %>% length, 2 )

  expect_equal(context$Tap$parameters , c("p3", "p1") )
  expect_equal(formals(context$Tap$tap), alist(p3 = , p1 = ) %>% as.pairlist())


})


test_that("parameter in tap", {
  contextString <-"

Tap:
  type: tap
  Pipe:
    type: pipe
    variables:
      v1: $p4
    F1:
      type: processor
      function: DoF1(f1p1 = $p2)
    F2:
      type: processor
      function: DoF2(f2p1 = $p1, $v1)
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parameters %>% length, 3 )

  expect_equal(context$Tap$parameters , c("p1", "p4", "p2") )
  expect_equal(formals(context$Tap$tap), alist(p1 = , p4 = , p2 = ) %>% as.pairlist())


  expect_equal(context$Tap$Pipe$parameters , c("p1", "p4", "p2") )
  expect_equal(formals(context$Tap$Pipe$fun), alist(p1 = , p4 = , p2 = ) %>% as.pairlist())

  expect_equal(context$Tap$Pipe$F1$parameters , c("p1", "p4", "p2") )
  expect_equal(formals(context$Tap$Pipe$F1$fun), alist(p1 = , p4 = , p2 = ) %>% as.pairlist())

  expect_equal(context$Tap$Pipe$F2$parameters , c("p1", "p4") )
  expect_equal(formals(context$Tap$Pipe$F2$fun), alist(p1 = , p4 = ) %>% as.pairlist())


})





