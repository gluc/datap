#' Plot a timeseries or a module
#'
#' @param x the tap to plot
#' @param ... additional parameters to pass to the plot.Node method
#' @param pruneFun a function to prune the plot. See \code{\link[data.tree]{Prune}}
#'
#' @examples
#' filePath <- system.file("extdata", "context1.yaml", package="datapR")
#' context <- Load(filePath)
#' plot(context$FindNode('SPX'))
#'
#' @importFrom data.tree Clone SetGraphStyle SetEdgeStyle SetNodeStyle
#' @export
plot.tap <- function(x, ..., pruneFun = NULL) {
  x <- Clone(x)
  SetGraphStyle(x, rankdir = "LR")
  SetEdgeStyle(x, dir = "back", penwidth = 2)

  SetNodeStyle(x, style = "filled,rounded", fontname = "helvetica", tooltip = GetPlotTooltip, penwidth = 2)

  x$Do(function(node) SetNodeStyle(node, shape = "house", fillcolor = "Green", rank = node$rank, inherit = FALSE, keepExisting = TRUE), filterFun = function(node) node$type %in% c("processor", "factory", "junction"))
  x$Do(function(node) SetNodeStyle(node, shape = "box", fillcolor = "Khaki1", rank = node$rank, inherit = FALSE), filterFun = function(node) node$type == "warning")
  x$Do(function(node) SetNodeStyle(node, shape = "box", fillcolor = "Tomato1", rank = node$rank, inherit = FALSE), filterFun = function(node) node$type == "error")
  x$Do(function(node) SetNodeStyle(node, shape = "Mcircle", fillcolor = "Seashell1", rank = node$rank, inherit = FALSE), filterFun = function(node) node$type == "pipe")

  #x$Do(function(node) SetNodeStyle(node, shape = "diamond", fillcolor = "Seashell1", inherit = TRUE, keepExisting = TRUE), filterFun = function(node) node$type == "module")
  x$Do(function(node) SetNodeStyle(node, shape = "component", fillcolor = "Seashell1", rank = node$rank, inherit = TRUE, keepExisting = TRUE), filterFun = function(node) node$type == "tap")
  #x$Do(function(node) SetNodeStyle(node, shape = "component", fillcolor = "Seashell1", inherit = TRUE, keepExisting = TRUE), filterFun = function(node) node$type == "timeseriesref")

  data.tree:::plot.Node(x, direction = "climb", pruneFun = pruneFun, engine = "dot")

  #callNextMethod()
}






#' @importFrom data.tree GetDefaultTooltip
GetPlotTooltip <- function(node) {
  return ("bla")
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

