


#'@export
ParseExpression <- function(expressionString) {

  vecs <- Node$new("function",
                   originalExpressionString = expressionString)

  expressionString <- gsub(" ", "", expressionString, fixed = TRUE)
  vecs$expressionV <- strsplit(expressionString, "")[[1]]

  vecs$pos <- matrix(FALSE, ncol = 8, nrow = length(vecs$expressionV))
  colnames(vecs$pos) <- c("tapTimeFun", "buildTimeFun", "open", "close", "arg", "eq" ,"str", "var")
  vecs$pos[, 'tapTimeFun'] <- (vecs$expressionV == ".")
  vecs$pos[, 'buildTimeFun'] <- (vecs$expressionV == ":")
  vecs$pos[, 'open'] <- (vecs$expressionV == "(")
  vecs$pos[, 'close'] <- (vecs$expressionV == ")")
  vecs$pos[, 'arg'] <- (vecs$expressionV == ",")
  vecs$pos[, 'eq'] <- (vecs$expressionV == "=")
  vecs$pos[, 'str'] <- (vecs$expressionV == "'" | vecs$expressionV == '"')
  vecs$pos[, 'var'] <- (vecs$expressionV == "$")

  .ParseExpression(vecs)
  return (vecs)
}

.ParseExpression <- function(node) {
  #is this named?
  idx <- ParseFindArgumentName(node)

  #what is this?
  #function, argument, variable, R expression
  node$type <- ParseFindType(node, idx)

  if (node$type == "fun") {
    ParseFunction(node, idx)
  } else if (node$type == "argument") {
    ParseArgument(node, idx)
  } else if (node$type == "variable") {
    node$variableName <- ParseVariableName(node, idx)
  } else if (node$type == "R") {
    node$expression <- paste0(node$expressionV, collapse = "")
  } else stop (paste0("Unkown node type ", node$type))

}


ParseArgument <- function(node, idx) {
  child <- node$AddChild(name = (node$count + 1))
  child$pos <- node$pos[idx:nrow(node$pos), , drop = FALSE]
  child$expressionV <- node$expressionV[idx:nrow(node$pos)]
  .ParseExpression(child)

}


ParseVariableName <- function(node, idx) {
  return (paste0(node$expressionV[-(1:idx)], collapse = ""))
}


ParseFunction <- function(node, idx) {
  idx <- ParseFunExecutionTime(node, idx)
  idx <- ParseFunName(node, idx)
  repeat {
    argEndIdx <- ParseFindArgEndIdx(node, idx + 1)

    child <- node$AddChild(name = (node$count + 1))
    child$pos <- node$pos[(idx + 1):(argEndIdx - 1), , drop = FALSE]
    child$expressionV <- node$expressionV[(idx + 1):(argEndIdx - 1)]
    .ParseExpression(child)
    if (argEndIdx == nrow(node$pos)) break
    idx <- argEndIdx
  }
}


Evaluate <- function(expressionTree, variablesList) {
  if (expressionTree$type == "R") {
    value <- eval(parse(text = expressionTree$expression))
    return (value)
  }
  if (expressionTree$type == "variable") {
    if (!expressionTree$variableName %in% names(variablesList)) stop (paste0("Variable $", expressionTree$variableName, " unknown!"))
    return (variablesList[[expressionTree$variableName]])
  }
  if (expressionTree$type == "argument") return (Evaluate(expressionTree$children[[1]], variablesList))
  if (expressionTree$type == "fun") {
    argList <- Get(nodes = expressionTree$children, attribute = Evaluate, simplify = FALSE, variablesList)
    argListNames <- Get(expressionTree$children, function(node) if (is.null(node$argumentName)) return("") else return (node$argumentName), simplify = FALSE)
    if (!all(argListNames == "")) names(argList) <- argListNames
    else names(argList) <- NULL
    res <- do.call(expressionTree$funName, argList)
    return (res)
  }
  stop (paste0("Unknown expression type ", expressionTree$type))

}


ParseFindType <- function(node, idx) {
  if (!node$isRoot && node$parent$type == "fun") return ("argument")
  if (any(node$pos[idx, c('tapTimeFun', 'buildTimeFun')])) return ("fun")
  if (node$pos[idx, 'var']) return("variable")
  else return ("R")
}


ParseFindArgumentName <- function(node) {
  for (i in 1:length(node$expressionV)) {
    if (node$pos[i, 'eq']) {
      nme <- paste0(node$expressionV[1:(i-1)], collapse = "")
      node$argumentName <- nme
      return (i + 1)
    }
    if (any(node$pos[i, ])) return (1)
  }
  return (1)
}



ParseFindArgEndIdx <- function(vecs, idx) {
  str <- FALSE
  funLvl <- 1
  for (i in idx:nrow(vecs$pos)) {
    if (str && vecs$pos[i, 'str']) str <- FALSE
    else if (!str && vecs$pos[i, 'str']) str <- TRUE

    if (!str) {

      if (vecs$pos[i, 'open']) funLvl <- funLvl + 1
      else if (vecs$pos[i, 'close'] && funLvl > 1) funLvl <- funLvl - 1
      else if (funLvl == 1 && (vecs$pos[i, 'arg'] || vecs$pos[i, 'close'])) return(i)
    }

  }
}

ParseFunExecutionTime <- function(vecs, idx) {
  if (vecs$pos[idx, 'tapTimeFun']) vecs$executionTime <- "tap"
  else if(vecs$pos[idx, 'buildTimeFun']) vecs$executionTime <- "build"
  else stop (paste0("Unknown function starting char '", vec$expressionV[idx]))
  return (2)
}

ParseFunName <- function(vecs, idx) {
  idxo <- which(vecs$pos[, 'open'])[1]
  vecs$funName <- paste0(vecs$expressionV[idx:(idxo-1)], collapse = "")
  return (idxo)
}

