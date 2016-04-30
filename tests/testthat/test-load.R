context("load")


test_that("aggregation junction", {
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
"

  context <- Load(textConnection(contextString))

  flow <- context$Get(function(joint) joint$Navigate(joint$downstream)$name, filterFun = isNotRoot)
  expected <- c(SPX = "context", pipe = "SPX", source = "pipe", junction = "source", doA = "junction", doB = "junction")
  expect_equal(flow, expected)

})




test_that("aggregation sub-pipe", {
  contextString <- "
SPX:
  type: tap
  pipe1:
    type: pipe
    source:
      type: processor
      function: DoSomething
    pipe2:
      type: pipe
      doA:
        type: processor
        function: doA
      doB:
        type: processor
        function: doB
    final:
      type: processor
      function: DoFinal
"

  context <- Load(textConnection(contextString))

  flow <- context$Get(function(joint) joint$Navigate(joint$downstream)$name, filterFun = isNotRoot)
  expected <- c(SPX = "context", pipe1 = "SPX", source = "pipe1", pipe2 = "source", doA = "pipe2", doB = "doA", final = "doB")
  expect_equal(flow, expected)

})


test_that("aggregation pipe in junction", {
  contextString <- "
SPX:
  type: tap
  pipe1:
    type: pipe
    source:
      type: processor
      function: DoSomething
    junction:
      type: junction
      function: DoJunction
      pipe1.1:
        type: pipe
        doA:
          type: processor
          function: doA
        doB:
          type: processor
          function: doB
      doC:
        type: processor
        function: doC


"

  context <- Load(textConnection(contextString))

  flow <- context$Get(function(joint) joint$Navigate(joint$downstream)$name, filterFun = isNotRoot)
  expected <- c(SPX = "context", pipe1 = "SPX", source = "pipe1", junction = "source", `pipe1.1` = "junction", doA = "pipe1.1", doB = "doA", doC = "junction")
  expect_equal(flow, expected)

})



test_that("aggregation junction in junction", {
  contextString <- "
SPX:
  type: tap
  pipe1:
    type: pipe
    source:
      type: processor
      function: DoSomething
    junction:
      type: junction
      function: DoJunction
      pipe1.1:
        type: pipe
        doA:
          type: processor
          function: doA
        doB:
          type: processor
          function: doB
      doC:
        type: junction
        function: doC
        doD:
          type: processor
          function: doD
        doE:
          type: processor
          function: doE
        doF:
          type: processor
          function: doF


"

  context <- Load(textConnection(contextString))

  flow <- context$Get(function(joint) joint$Navigate(joint$downstream)$name, filterFun = isNotRoot)
  expected <- c(SPX = "context", pipe1 = "SPX", source = "pipe1", junction = "source", `pipe1.1` = "junction", doA = "pipe1.1", doB = "doA", doC = "junction", doD = "doC", doE = "doC", doF = "doC")
  expect_equal(flow, expected)

})




test_that("structure", {
  contextString <- "
Closing:
  type: structure
  Old:
    type: structure
    X1:
      type: tap
      X1:
        type: processor
        function: DoIt
  New:
    type: structure
    X2:
      type: tap
      X2:
        type: processor
        function: DoIt
"

  context <- Load(textConnection(contextString))
  flow <- context$Get(function(joint) joint$Navigate(joint$downstream)$name, filterFun = isNotRoot)
  expected <- c(Closing = "context", Old = "Closing", X1 = "Old", X1 = "X1", New = "Closing", X2 = "New", X2 = "X2") #fill in
  expect_equal(flow, expected)
})
