context("load")


test_that("aggregation 1", {
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
  expected <- c(SPX = "SPX", pipe = "SPX", source = "pipe", junction = "source", doA = "junction", doB = "junction")
  expect_equal(flow, expected)

})

