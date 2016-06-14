context("function parsing")

#devtools::test(filter = "function")


test_that("dot no arg", {

  funString <- "Sys.Date()"

  ex <- ParseExpression(funString)

  expect_equal(class(ex), c("Node", "R6"))
  expect_equal(ex$type, "fun")
  expect_equal(ex$funName, "Sys.Date")
  expect_equal(ex$height, 1)
  expect_is(datapR:::Evaluate(ex, list()), "Date")
})


test_that("single number", {

  funString <- "3"

  ex <- ParseExpression(funString)

  expect_equal(class(ex), c("Node", "R6"))
  expect_equal(ex$type, "R")

  expect_equal(datapR:::Evaluate(ex, list(variable0 = 1, variable1 = 2)), 3)
})


test_that("Variable parsing", {

  funString <- "$variable1"

  ex <- ParseExpression(funString)

  expect_equal(class(ex), c("Node", "R6"))
  expect_equal(ex$type, "variable")
  expect_equal(datapR:::Evaluate(ex, list(variable0 = 1, variable1 = 2)), 2)
})


test_that("Variable parsing ws", {

  funString <- " $variable1 "

  ex <- ParseExpression(funString)

  expect_equal(class(ex), c("Node", "R6"))
  expect_equal(ex$type, "variable")
  expect_equal(datapR:::Evaluate(ex, list(variable0 = 1, variable1 = 2)), 2)
})



test_that("Function parsing", {

  funString <- ".identity(29)"

  fun <- ParseExpression(funString)

  expect_equal(class(fun), c("Node", "R6"))
  expect_equal(fun$funName, "identity")
  expect_equal(fun$count, 1)
  expect_equal(fun$children[[1]]$children[[1]]$expression, 29)
  expect_equal(fun$executionTime, "tap")
  expect_equal(datapR:::Evaluate(fun, list()), 29)
})



test_that("Function parsing default", {

  funString <- "identity(29)"

  fun <- ParseExpression(funString)

  expect_equal(class(fun), c("Node", "R6"))
  expect_equal(fun$funName, "identity")
  expect_equal(fun$count, 1)
  expect_equal(fun$children[[1]]$children[[1]]$expression, 29)
  expect_equal(fun$executionTime, "aeap")
  expect_equal(datapR:::Evaluate(fun, list()), 29)
})


test_that("Function parsing ws ", {

  funString <- " .identity( 29 ) "

  fun <- ParseExpression(funString)

  expect_equal(class(fun), c("Node", "R6"))
  expect_equal(fun$funName, "identity")
  expect_equal(fun$count, 1)
  expect_equal(fun$children[[1]]$children[[1]]$expression, 29)
  expect_equal(fun$executionTime, "tap")
  expect_equal(datapR:::Evaluate(fun, list()), 29)
})


test_that("Function parsing named param", {

  funString <- ".identity(x = 29)"

  fun <- ParseExpression(funString)

  expect_equal(class(fun), c("Node", "R6"))
  expect_equal(fun$funName, "identity")
  expect_equal(fun$count, 1)
  expect_equal(fun$children[[1]]$children[[1]]$expression, 29)
  expect_equal(fun$children[[1]]$argumentName, "x")
  expect_equal(fun$executionTime, "tap")
  expect_equal(datapR:::Evaluate(fun, list()), 29)

})


test_that("Function parsing string", {

  funString <- ".identity(x = '29')"

  fun <- ParseExpression(funString)

  expect_equal(class(fun), c("Node", "R6"))
  expect_equal(fun$funName, "identity")
  expect_equal(fun$count, 1)
  expect_equal(fun$children[[1]]$children[[1]]$expression, "'29'")
  expect_equal(fun$children[[1]]$argumentName, "x")
  expect_equal(fun$executionTime, "tap")
  expect_equal(datapR:::Evaluate(fun, list()), '29')

})



test_that("Function parsing string", {
  str <- "y = .identity(z = $p1)"
  funString <- paste0(".identity(x = '", str, "')")

  fun <- ParseExpression(funString)
  #ToDataFrameTree(fun, "type", "value", "level")
  expect_equal(class(fun), c("Node", "R6"))
  expect_equal(fun$funName, "identity")
  expect_equal(fun$count, 1)
  expect_equal(fun$`1`$`1`$expression, paste0("'", str, "'"))
  expect_equal(fun$children[[1]]$argumentName, "x")
  expect_equal(fun$executionTime, "tap")
  expect_equal(datapR:::Evaluate(fun, list(p1 = 25)), str)

})



test_that("Function parsing multi", {

  funString <- ".sum(x = 29, y = 23 , 3.2)"

  fun <- ParseExpression(funString)

  expect_equal(class(fun), c("Node", "R6"))
  expect_equal(fun$funName, "sum")
  expect_equal(fun$count, 3)
  expect_equal(fun$children[[1]]$children[[1]]$expression, 29)
  expect_equal(fun$children[[1]]$argumentName, "x")
  expect_equal(fun$executionTime, "tap")
  expect_equal(datapR:::Evaluate(fun, list()), 55.2)


})



test_that("Function variable", {

  funString <- ".identity(x = $p1)"

  fun <- ParseExpression(funString)

  expect_equal(class(fun), c("Node", "R6"))
  expect_equal(fun$funName, "identity")
  expect_equal(fun$count, 1)
  expect_equal(fun$children[[1]]$argumentName, "x")
  expect_equal(fun$executionTime, "tap")

  expect_equal(datapR:::Evaluate(fun, list(p1 = 'myval')), 'myval')
  expect_equal(datapR:::Evaluate(fun, list(p1 = 27)), 27)


})



test_that("Function nested", {

  funString <- ".sum(x = .c(3, 4))"

  fun <- ParseExpression(funString)

  expect_equal(class(fun), c("Node", "R6"))
  expect_equal(fun$funName, "sum")
  expect_equal(fun$count, 1)

  #ToDataFrameTree(fun, "type", "value", "level")
  expect_equal(fun$`1`$`1`$count, 2)

  expect_equal(fun$children[[1]]$argumentName, "x")
  expect_equal(fun$executionTime, "tap")

  expect_equal(datapR:::Evaluate(fun, list(p1 = 27)), 7)


})



test_that("Function nested", {

  funString <- ".sum(x = .c(3, .prod(3,4)))"

  fun <- ParseExpression(funString)

  #ToDataFrameTree(fun, "type", "value", "level")

  expect_equal(datapR:::Evaluate(fun, list()), 15)

})



test_that("Function nested", {

  funString <- ".sum(x = .c(3, y =.prod(  $param , name = 4)))"

  fun <- ParseExpression(funString)

  #ToDataFrameTree(fun, "type", "value", "level")

  expect_equal(datapR:::Evaluate(fun, list(param = 4)), 19)

})




test_that("Function parsing nested multi", {

  funString <- ".sum(x = 2, y = .c($p1, $p2), z = c(2, 4))"
  fun <- ParseExpression(funString)
  expect_equal(fun$executionTime, "tap")
  expect_equal(fun$`2`$executionTime, "tap")
  expect_equal(fun$`3`$executionTime, "tap")
  expect_equal(fun$`2`$`1`$executionTime, "tap")
  expect_equal(fun$`3`$`1`$executionTime, "aeap")

  expect_equal(datapR:::Evaluate(fun, list(p1 = 4, p2 = 3)), 15)

})




