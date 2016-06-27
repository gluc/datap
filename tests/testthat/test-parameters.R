context("parameters")


test_that("auto parameter", {
  contextString <-"

Tap:
  type: tap
  GetData:
    type: processor
    function: GetData($v1)
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parametersE %>% length, 1 )

  expect_equal(context$Tap$parametersE , list(v1 = NULL) )
  expect_equal(formals(context$Tap$tap), alist(v1 = ) %>% as.pairlist())


})



test_that("parameter in tap", {
  contextString <-"

Tap:
  parameters:
    v1: 3
  type: tap
  GetData:
    type: processor
    function: GetData($v1)
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parametersE %>% length, 1 )

  expect_equal(context$Tap$parametersE$v1$children[[1]]$value , 3 )
  expect_equal(formals(context$Tap$tap), alist(v1 = 3) %>% as.pairlist())


})





test_that("unused parameter in tap", {
  contextString <-"

Tap:
  parameters:
    v1: 3
    v2: FALSE
  type: tap
  GetData:
    type: processor
    function: GetData($v1)
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parametersE %>% length, 1 )

  expect_equal(context$Tap$parametersE$v1$children[[1]]$value , 3 )
  expect_equal(formals(context$Tap$tap), alist(v1 = 3) %>% as.pairlist())


})





test_that("parameter order", {
  contextString <-"

Tap:
  parameters:
    v2: 3
    v1: FALSE
  type: tap
  GetData:
    type: processor
    function: GetData($v1, $v2)
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parametersE %>% length, 2 )

  expect_equal(formals(context$Tap$tap), alist(v2 = 3, v1 = FALSE) %>% as.pairlist())


})





test_that("adding parameter", {
  contextString <-"

Tap:
  parameters:
    v2: 3
  type: tap
  GetData:
    type: processor
    function: GetData($v1, $v2)
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$parametersE %>% length, 2 )

  expect_equal(formals(context$Tap$tap), alist(v2 = 3, v1 = ) %>% as.pairlist())


})







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

  expect_equal(context$Tap$parametersE %>% length, 2 )

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

  expect_equal(context$Tap$parametersE %>% length, 3 )

  expect_equal(context$Tap$parametersE , list(p1 = NULL, p4 = NULL, p2 = NULL) )
  expect_equal(formals(context$Tap$tap), alist(p1 = , p4 = , p2 = ) %>% as.pairlist())


  expect_equal(context$Tap$Pipe$parametersE , list(p1 = NULL, p4 = NULL, p2 = NULL) )
  expect_equal(formals(context$Tap$Pipe$tap), alist(p1 = , p4 = , p2 = ) %>% as.pairlist())

  expect_equal(context$Tap$Pipe$F1$parametersE , list(p1 = NULL, p4 = NULL, p2 = NULL) )
  expect_equal(formals(context$Tap$Pipe$F1$tap), alist(p1 = , p4 = , p2 = ) %>% as.pairlist())

  expect_equal(context$Tap$Pipe$F2$parametersE , list(p1 = NULL, p4 = NULL) )
  expect_equal(formals(context$Tap$Pipe$F2$tap), alist(p1 = , p4 = ) %>% as.pairlist())


})





test_that("build time parameter expression", {
  contextString <-"

Tap:
  parameters:
    p1: Sys.Date()
  type: tap
  GetData:
    type: processor
    function: identity(as.Date($p1))
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$tap %>% formals, pairlist(p1 = as.character(Sys.Date())))

})



test_that("tap time parameter expression", {
  contextString <-"

Tap:
  parameters:
    p1: .Sys.Date()
  type: tap
  GetData:
    type: processor
    function: identity($p1)
"
  context <- Load(textConnection(contextString))

  expect_equal(context$Tap$tap %>% formals, alist(p1 = Sys.Date()) %>% as.pairlist)


})

