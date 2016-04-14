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
