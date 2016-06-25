



ParseExpression <- function(expressionString) {
  expressionString <- as.character(expressionString)
  vecs <- Node$new("expression",
                   originalExpressionString = expressionString)

  vecs$expressionV <- strsplit(expressionString, "")[[1]]

  vecs$pos <- matrix(FALSE, ncol = 8, nrow = length(vecs$expressionV))
  colnames(vecs$pos) <- c("tapTimeFun", "open", "close", "arg", "eq" ,"str", "var", "ws")
  vecs$pos[, 'tapTimeFun'] <- (vecs$expressionV == ".")
  vecs$pos[, 'open'] <- (vecs$expressionV == "(")
  vecs$pos[, 'close'] <- (vecs$expressionV == ")")
  vecs$pos[, 'arg'] <- (vecs$expressionV == ",")
  vecs$pos[, 'eq'] <- (vecs$expressionV == "=")
  vecs$pos[, 'str'] <- (vecs$expressionV == "'" | vecs$expressionV == '"')
  vecs$pos[, 'var'] <- (vecs$expressionV == "$")
  vecs$pos[, 'ws'] <- (vecs$expressionV == " ")

  vecsChild <- Clone(vecs)
  vecs$AddChildNode(vecsChild)
  vecs$type <- "expressionTree"
  vecs$executionTime <- "aeap"

  .ParseExpression(vecsChild)
  return (vecs)
}

.ParseExpression <- function(expressionNode) {
  #is this named?
  idx <- ParseFindArgumentName(expressionNode)

  #what is this?
  #function, argument, variable, R expression
  expressionNode$type <- ParseFindType(expressionNode, idx)
  idx <- ParseExecutionTime(expressionNode, idx)
  if (expressionNode$type == "fun") {
    ParseFunction(expressionNode, idx)
  } else if (expressionNode$type == "argument") {
    ParseArgument(expressionNode, idx)
  } else if (expressionNode$type == "variable") {
    expressionNode$variableName <- ParseVariableName(expressionNode, idx)
  } else if (expressionNode$type == "R") {
    expressionNode$expression <- paste0(expressionNode$expressionV, collapse = "") %>% trimws %>% type.convert(as.is = TRUE)
    if (is.character(expressionNode$expression) && ! grepl("['\"].*['\"]$", expressionNode$expression, perl = TRUE)) expressionNode$expression <- paste0("'", expressionNode$expression, "'")
  } else stop (paste0("Unkown expressionNode type ", expressionNode$type))

}


ParseArgument <- function(expressionNode, idx) {
  while(expressionNode$pos[idx, 'ws']) idx <- idx + 1
  child <- expressionNode$AddChild(name = (expressionNode$count + 1))
  child$pos <- expressionNode$pos[idx:nrow(expressionNode$pos), , drop = FALSE]
  child$expressionV <- expressionNode$expressionV[idx:nrow(expressionNode$pos)]
  .ParseExpression(child)

}


ParseVariableName <- function(expressionNode, idx) {
  nme <- paste0(expressionNode$expressionV[-(1:idx)], collapse = "")
  nme <- trimws(nme)
  return (nme)
}


ParseFunction <- function(expressionNode, idx) {
  while(expressionNode$pos[idx, 'ws']) idx <- idx + 1
  idx <- ParseFunName(expressionNode, idx)
  repeat {
    argEndIdx <- ParseFindArgEndIdx(expressionNode, idx + 1)
    while(expressionNode$pos[idx, 'ws']) idx <- idx + 1
    if (idx + 1 < argEndIdx) {
      child <- expressionNode$AddChild(name = (expressionNode$count + 1))
      child$pos <- expressionNode$pos[(idx + 1):(argEndIdx - 1), , drop = FALSE]
      child$expressionV <- expressionNode$expressionV[(idx + 1):(argEndIdx - 1)]
      .ParseExpression(child)
    }
    idx <- argEndIdx
    while(expressionNode$pos[idx, 'ws']) idx <- idx + 1
    if (idx == nrow(expressionNode$pos) || (nrow(expressionNode$pos) > idx && all(expressionNode$pos[(idx + 1):nrow(expressionNode$pos), 'ws']))) break

  }
}




# This should be called on tap time. The expression tree
# is not changed.
Evaluate <- function(expressionTree, variablesList) {

  if (expressionTree$type == "R") {
    value <- eval(parse(text = expressionTree$expression))
    return (value)
  }

  else if (expressionTree$type == "expressionTree") {
    return (Evaluate(expressionTree$children[[1]], variablesList))
  }

  else if (expressionTree$type == "variable") {

    if (!expressionTree$variableName %in% names(variablesList)) stop (paste0("Variable $", expressionTree$variableName, " unknown!"))
    return (variablesList[[expressionTree$variableName]])
  }

  else if (expressionTree$type == "argument") return (Evaluate(expressionTree$children[[1]], variablesList))

  else if (expressionTree$type == "fun") {
    argList <- Get(nodes = expressionTree$children, attribute = Evaluate, simplify = FALSE, variablesList)
    argListNames <- Get(expressionTree$children, function(node) if (is.null(node$argumentName)) return("") else return (node$argumentName), simplify = FALSE)

    if (!all(argListNames == "")) names(argList) <- argListNames
    else names(argList) <- NULL

    if (is.null(argList)) argList <- list()

    res <- do.call(expressionTree$funName, argList)
    return (res)
  }

  else if (expressionTree$type == "value") {
    return (expressionTree$value)
  }



  stop (paste0("Unknown expression type ", expressionTree$type))

}


Deparse <- function(expressionTree) {
  if (expressionTree$type == "R") {
    return (expressionTree$value)
  }

  else if (expressionTree$type == "expressionTree") {
    return (Deparse(expressionTree$children[[1]]))
  }

  else if (expressionTree$type == "variable") {

    stop (paste0("Variable $", expressionTree$variableName, " unknown!"))
  }

  else if (expressionTree$type == "argument") return (Deparse(expressionTree$children[[1]]))

  else if (expressionTree$type == "fun") {
    argList <- Get(nodes = expressionTree$children, attribute = Deparse, simplify = FALSE)
    argListNames <- Get(expressionTree$children, function(node) if (is.null(node$argumentName)) return("") else return (node$argumentName), simplify = FALSE)

    if (!all(argListNames == "")) names(argList) <- argListNames
    else names(argList) <- NULL

    if (is.null(argList)) argList <- list()

    args <- paste(argList, sep = " = ", collapse = ", ")
    fun <- paste0(expressionTree$funName, "(", args, ")")

    return (fun)
  }

  else if (expressionTree$type == "value") {
    res <- expressionTree$value
    if (NeedsQuotes(res)) res <- paste0("'", res, "'")
    #res <- paste0("'", res, "'")
    return (res)
  }



  stop (paste0("Unknown expression type ", expressionTree$type))

}

NeedsQuotes <- function(value) {
  if (is.character(value)) return (TRUE)
  if (is.numeric(value)) return (FALSE)
  if (is.logical(value)) return (FALSE)
  return (TRUE)
}

# This is called build time. Expressions are evaluated where possible
# and variables are fetched from joint and ancestors. This changes
# the expression tree upon evaluation.
EvaluateExpressionBuild <- function(expressionTree, joint, doConst) {
  expressionTree$Do(EvaluateNodeBuild, traversal = "post-order", joint, doConst)
}

EvaluateNodeBuild <- function(expressionTree, joint, doConst) {
  if (expressionTree$executionTime == "tap") return()
  if (expressionTree$type == "R") {
    expressionTree$value <- eval(parse(text = expressionTree$expression))
    expressionTree$type <- "value"
    expressionTree$children <- list()
  } else if (expressionTree$type == "variable") {
    if (doConst) {
      if (expressionTree$variableName == "joint") {
        expressionTree$type = "value"
        expressionTree$value = joint
      } else if (expressionTree$variableName == "context") {
        expressionTree$type = "value"
        expressionTree$value = joint$root
      }
    } else {
      val <- GetVariableValueBuild(joint, expressionTree$variableName)
      if (!is.null(val)) {
        val$children[[1]] %>% Clone -> val
        val$name <- expressionTree$name
        expressionTree$name <- "tmp"
        val <- expressionTree$AddSiblingNode(val)
        val$parent$RemoveChild("tmp")
      }
    }
  } else if (expressionTree$type == "argument") {
    #we need the arg construct because of name
    #EvaluateNodeBuild(expressionTree$children[[1]], joint, doConst)
  } else if (expressionTree$type == "fun") {
    #debug
    nme <- joint$name

    Get(expressionTree$children, function(arg) arg$children[[1]]$type == "value") %>%
      all ->
      evaluatable

    if (evaluatable) {

      argList <- Get(expressionTree$children, function(e) e$children[[1]]$value, simplify = FALSE)
      argListNames <- Get(expressionTree$children, function(expressionNode) if (is.null(expressionNode$argumentName)) return("") else return (expressionNode$argumentName), simplify = FALSE)
      if (!all(argListNames == "")) names(argList) <- argListNames
      if (is.null(argList)) argList <- list()
      else names(argList) <- NULL
      res <- do.call(expressionTree$funName, argList)
      if (is.function(res)) {
        expressionTree$type <- "fun"
        expressionTree$children <- list()
        expressionTree$funName <- res #not really the name, but the actual function in this case
      } else {
        expressionTree$type <- "value"
        expressionTree$children <- list()
        expressionTree$value <- res
      }
    }
  } else if (expressionTree$type == "expressionTree") {
    #EvaluateNodeBuild(expressionTree$children[[1]], joint, doConst)
  } else if (expressionTree$type == "value") {
    return ()
  } else {
    stop (paste0("Unknown expression type ", expressionTree$type))
  }



}


GetVariableValueBuild <- function(joint, name) {

  if (!is.null(joint$variablesE)) {
    cand <- joint$variablesE[[name]]
    if (!is.null(cand)) {
      #res <- EvaluateExpressionBuild(cand, node = node)
      #if (cand$type == "value") return (cand$value)
      #else return (NULL)
      return (cand)
    }

  }
  if (joint$isRoot) return (NULL)
  return (GetVariableValueBuild(joint$parent, name))
}



GetVariablesInExpression <- function(expression) {
  if (length(expression) == 0) return (list())
  expression$Get(function(node) node$variableName, filterFun = function(node) node$type == "variable", simplify = FALSE)
}

ParseFindType <- function(expressionNode, idx) {
  while(expressionNode$pos[idx, 'ws']) idx <- idx + 1
  if (!expressionNode$isRoot && expressionNode$parent$type == "fun") return ("argument")

  for (i in idx:length(expressionNode$expressionV)) {
    if (expressionNode$pos[i, 'open']) return ("fun")
    if (any(expressionNode$pos[i, c("close", "arg", "eq" ,"str", "var") ])) break
  }

  if (expressionNode$pos[i, 'var']) return("variable")
  else return ("R")
}


ParseFindArgumentName <- function(expressionNode) {
  idx <- 1
  while(expressionNode$pos[idx, 'ws']) idx <- idx + 1
  for (i in idx:length(expressionNode$expressionV)) {
    if (expressionNode$pos[i, 'eq']) {
      nme <- paste0(expressionNode$expressionV[idx:(i-1)], collapse = "")
      expressionNode$argumentName <- trimws(nme)
      return (i + 1)
    }
    if (any(expressionNode$pos[i, c("tapTimeFun", "open", "close", "arg", "eq" ,"str", "var") ])) return (idx)
  }
  return (idx)
}



ParseFindArgEndIdx <- function(vecs, idx) {
  while(vecs$pos[idx, 'ws']) idx <- idx + 1
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

ParseExecutionTime <- function(vecs, idx) {
  while(vecs$pos[idx, 'ws']) idx <- idx + 1
  if (vecs$type == "argument") {
    vecs$executionTime <- vecs$parent$executionTime
  } else if(vecs$pos[idx, 'tapTimeFun']) {
    vecs$executionTime <- "tap"
    idx <- idx + 1
  } else {
    #defaults: as early as possible (aeap)
    vecs$executionTime <- "aeap"
  }

  return (idx)
}

ParseFunName <- function(vecs, idx) {
  while(vecs$pos[idx, 'ws']) idx <- idx + 1
  idxo <- which(vecs$pos[, 'open'])[1]
  vecs$funName <- paste0(vecs$expressionV[idx:(idxo-1)], collapse = "")
  return (idxo)
}


ContainsVariable <- function(expression, variableName) {
  expression$Get(function(n) n$type == "variable" && n$variableName == variableName) %>% any
}

