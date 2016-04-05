
#' Loads a meta definition.
#'
#' @param con a connection containing the meta data
#'
#' @examples
#' filePath <- system.file("extdata", "sample_metadata.yaml", package="finPrice")
#' context <- Load(filePath)
#'
#' @importFrom yaml yaml.load
#' @importFrom data.tree FromListExplicit Do
#' @export
Load <- function(con) {
  yamlString <- paste0(readLines(con), collapse = "\n")
  lol <- yaml.load(yamlString)
  lol <- list(children = lol)
  tree <- FromListExplicit(lol)

  class(tree) <- c("context", class(tree))
  tree$Do(function(node) class(node) <- c("timeseries", class(node)), filterFun = function(x) x$level == 2)

  #replace function
  #tree$Do(fun = ParseFun,
  #        filterFun = function(node) !is.null(node$fun)
  #        )

  #return
  return (tree)
}

#' Plot a timeseries
#'
#' @examples
#' filePath <- system.file("extdata", "sample_metadata.yaml", package="finPrice")
#' context <- Load(filePath)
#' plot(context$SPX)
#'
#' @export
plot.timeseries <- function(x, ..., direction = c("climb", "descend"), pruneFun = NULL, engine = "dot") {
  x <- Clone(x)
  SetGraphStyle(x, rankdir = "BT")
  SetEdgeStyle(x, dir = "back", penwidth = 2)

  SetNodeStyle(x, style = "filled,rounded", shape = "component", fillcolor = "Seashell1",
               fontname = "helvetica", tooltip = GetPlotTooltip, penwidth = 2)
  x$Do(function(x) SetNodeStyle(x, shape = "invhouse", inherit = FALSE, keepExisting = TRUE), filterFun = function(x) x$type == "instruction")
  x$Do(function(node) SetNodeStyle(node, shape = "box", fillcolor = "Khaki1", inherit = FALSE), filterFun = function(node) node$type == "warning")
  x$Do(function(node) SetNodeStyle(node, shape = "box", fillcolor = "Tomato1", inherit = FALSE), filterFun = function(node) node$type == "error")

  data.tree:::plot.Node(x, ..., direction, pruneFun, engine)
  #callNextMethod()
}



GetPlotTooltip <- function(node) {
  if (node$isRoot) return (GetDefaultTooltip(node))
  res <- paste(paste("type:", node$type),
               paste("fun:", node$fun),
               paste("arguments:"),
               paste(paste0("  ", names(node$arguments)), node$arguments, sep = ": ", collapse = "\n"),
               sep = "\n")

  if (!is.null(node$description)) {
    res <- paste(res, paste("description:", node$description), sep = "\n")
  }

  return (res)

}


#' Get a timeseries
#'
#' @param id the id of the timeseries
#' @param context the context object
#' @param ... and additional parameters that need to be passed to the function
#'
#' @export
GetData <- function(id, context, ...) {
  context$Climb(id)$fun(...)
}


ParseFun <- function(node) {
  prse <- parse(text = node$fun)

  if (length(prse) == 1 && class(prse[[1]]) == "call") {


  } else if (length(prse) == 1 && class(prse[[1]]) == "name") {
    fun <- eval(prse)
  } else {
    stop("Unknown expression")
  }

  if (is.function(fun)) {
    frmls <- formals(fun)
    newfun <- function() {}
    formals(newfun) <- setdiff(names(formals(fun)), names(frmls))
    argsToSet <- intersect(names(formals(fun)), names(frmls))

    arglist <- sapply(argsToSet, function(argToSet) paste0(argToSet, " = ", argToSet ))
    formals(fun) <- eval(parse(text = paste0("alist(", paste0(arglist, collapse = ", "), ")")))


  }


  #node$fun <- fun

}
