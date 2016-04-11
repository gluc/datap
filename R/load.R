
#' Loads a meta definition.
#'
#' @param con a connection containing the meta data
#'
#' @examples
#' filePath <- system.file("extdata", "context1.yaml", package="CodaPipeR")
#' context <- Load(filePath)
#'
#' @importFrom yaml yaml.load
#' @importFrom data.tree FromListExplicit Do isNotRoot
#' @export
Load <- function(con) {
  yamlString <- paste0(readLines(con), collapse = "\n")
  lol <- yaml.load(yamlString)

  tree <- CreateTree(lol)

  class(tree) <- c("context", class(tree))
  tree$Do(function(node) class(node) <- c("specification", class(node)), filterFun = function(x) x$level == 2)

  tree$Do(fun = ParseFun,
          traversal = "post-order",
          filterFun = isNotRoot)

  #return
  return (tree)
}



#' @export
CreateTree <- function(lol) {

  rawTree <- FromListSimple(lol, nameName = NULL)

  #only children of pipe and junction are true nodes
  rawTree$Do(function(node) {
    parent <- node$parent
    parent$RemoveChild("arguments")
    nd <- as.list(node)
    nd <- nd[node$fields]
    parent$arguments <- nd
  },
  filterFun = function(node) node$name == "arguments")

  rawTree$Do(function(node) {
    parent <- node$parent
    parent$RemoveChild("variables")
    nd <- as.list(node)
    nd <- nd[node$fields]
    parent$variables <- nd

  },
  filterFun = function(node) node$name == "variables")

  rawTree$Do(function(pipeNode) {
                parent <- pipeNode$children[[1]]
                for (child in pipeNode$children[-1]) {
                  pipeNode$RemoveChild(child$name)
                  parent$AddChildNode(child)
                  parent <- child
                }

              },
             filterFun = function(node) !is.null(node$type) && node$type == "pipe")

  rawTree$Do(function(pipeNode) {
                child <- pipeNode$children[[1]]
                pipeNode$RemoveChild(child$name)
                pipeNode$parent$AddChildNode(child)
                pipeNode$parent$RemoveChild(pipeNode$name)
            },
            filterFun = function(node) node$level > 3 &&
                                       !is.null(node$type) &&
                                       node$type == "pipe" &&
                                       !is.null(node$parent$type) &&
                                       node$parent$type != "junction")

  ctx <- rawTree$data
  ctx$parent <- NULL
  ctx$name <- "Context"
  return (ctx)

}





#' Plot a timeseries or a module
#'
#' @examples
#' filePath <- system.file("extdata", "sample_metadata.yaml", package="finPrice")
#' context <- Load(filePath)
#' plot(context$SPX)
#' plot(context$`NA Handling`)
#'
#' @importFrom data.tree Clone SetGraphStyle SetEdgeStyle SetNodeStyle
#' @export
plot.specification <- function(x, ..., direction = c("climb", "descend"), pruneFun = NULL, engine = "dot") {
  x <- Clone(x)
  SetGraphStyle(x, rankdir = "BT")
  SetEdgeStyle(x, dir = "back", penwidth = 2)

  SetNodeStyle(x, style = "filled,rounded", fontname = "helvetica", tooltip = GetPlotTooltip, penwidth = 2)

  x$Do(function(node) SetNodeStyle(node, shape = "invhouse", fillcolor = "Seashell1", inherit = FALSE, keepExisting = TRUE), filterFun = function(node) node$type %in% c("transformation", "function"))
  x$Do(function(node) SetNodeStyle(node, shape = "box", fillcolor = "Khaki1", inherit = FALSE), filterFun = function(node) node$type == "warning")
  x$Do(function(node) SetNodeStyle(node, shape = "box", fillcolor = "Tomato1", inherit = FALSE), filterFun = function(node) node$type == "error")
  x$Do(function(node) SetNodeStyle(node, shape = "larrow", fillcolor = "Seashell1", inherit = FALSE), filterFun = function(node) node$type == "moduleref")
  x$Do(function(node) SetNodeStyle(node, shape = "rarrow", fillcolor = "Seashell1", inherit = TRUE, keepExisting = TRUE), filterFun = function(node) node$type == "module")
  x$Do(function(node) SetNodeStyle(node, shape = "component", fillcolor = "Seashell1", inherit = TRUE, keepExisting = TRUE), filterFun = function(node) node$type == "timeseries")
  x$Do(function(node) SetNodeStyle(node, shape = "component", fillcolor = "Seashell1", inherit = TRUE, keepExisting = TRUE), filterFun = function(node) node$type == "timeseriesref")

  data.tree:::plot.Node(x, ..., direction, pruneFun, engine)
  #callNextMethod()
}


#' @importFrom data.tree GetDefaultTooltip
GetPlotTooltip <- function(node) {
  if (node$isRoot) return (GetDefaultTooltip(node))
  res <- paste(paste("type:", node$type),
               paste("function:", node$funName),
               paste("arguments:"),
               #paste(paste0("  ", names(node$arguments)), node$arguments, to = "ASCII", sub = "" , sep = ": ", collapse = "\n"), #causes error in DiagrammeR. Need to escape @ and ^ and possibly \
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
#'
#' @export
GetData <- function(context, id) {
  context$Climb(id)$fun(...)
}



#' @importFrom assertthat assert_that
ParseFun <- function(node) {

  # warning, error and transformation
    funNme <- node$`function`
    funArgs <- node$arguments
    funArgsNms <- names(funArgs)

    print (node$name)
    if (node$type %in% c("warning", "error")) {

      Wrn <- function() {
        children <- lapply(node$children, function(node) node$fun())
        if ("@next" %in% funArgs) {
          if (node$count == 1) children <- children[[1]]
          funArgs[[which(funArgs == "@next")]] <- children
        }

        ok <- do.call.intrnl(funNme, funArgs)
        if (!ok[[1]]) {
          if (node$type == "warning") warning(paste0("step ", node$name, " raised warning:", ok[[2]]))
          if (node$type == "error") stop(paste0("step ", node$name, " raised error:", ok[[2]]))
        }
        print(paste0("Processed ", node$name))
        return (children)
      }
      node$fun <- Wrn
    } else if (node$type %in% c("transformation", "junction")) {
      CallStep <- function() {
        if ("@next" %in% funArgs) {
          children <- lapply(node$children, function(node) node$fun())
          if (node$count == 1) children <- children[[1]]
          funArgs[[which(funArgs == "@next")]] <- children
        }

        #if (node$name == "Combine") browser()
        res <- do.call.intrnl(funNme, funArgs)
        print(paste0("Processed ", node$name))
        return (res)
      }

      node$fun <- CallStep
    } else if (node$type == "function") {
        if ("@next" %in% funArgs) {
          children <- lapply(node$children, function(node) node$fun)
          if (node$count == 1) children <- children[[1]]
          funArgs[[which(funArgs == "@next")]] <- children
        }
        res <- do.call.intrnl(funNme, funArgs)
        node$fun <- res
    } else if (node$type == "pipe") {
      node$fun <- function() node$children[[1]]$fun()
    } else if (node$type == "timeseries") {

      CallTimeseries <- function() {
        #assert_that(node$count == 1)
        node$children[[1]]$fun()
      }

      node$fun <- CallTimeseries

    } else {
       stop(paste0("Unknown node type ", node$type))

    }


}




do.call.intrnl <- function(what, args) {
  if(is.character(what)) {
    fn <- strsplit(what, "::")[[1]]
    if(length(fn)==1) {
      fun <- fn[[1]]
      envir <- parent.frame()
    } else {
      fun <- fn[[2]]
      envir <- asNamespace(fn[[1]])
    }
  }

  do.call(fun, as.list(args), envir = envir)
}
