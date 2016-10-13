#' Plot a timeseries or a module
#'
#' @param x the tap to plot
#' @param ... additional parameters to pass to the plot.Node method
#' @param pruneFun a function to prune the plot. See \code{\link[data.tree]{Prune}}
#'
#' @examples
#' filePath <- system.file("extdata", "context1.yaml", package="datap")
#' context <- Load(filePath)
#' plot(data.tree::FindNode(context, 'SPX'))
#'
#' @importFrom data.tree Clone SetGraphStyle SetEdgeStyle SetNodeStyle
#' @importFrom DiagrammeR grViz
#' @export
plot.tap <- function(x, ..., pruneFun = NULL) {
  x <- Clone(x)
  SetGraphStyle(x, rankdir = "LR")
  SetEdgeStyle(x, dir = "back", penwidth = 2)

  SetNodeStyle(x, style = "filled,rounded", fontname = "helvetica", tooltip = GetPlotTooltip, penwidth = 2)

  x$Do(function(node) SetNodeStyle(node, shape = "house", fillcolor = "Green", rank = node$rank, inherit = FALSE, keepExisting = TRUE), filterFun = function(node) node$type %in% c("processor", "junction"))
  x$Do(function(node) SetNodeStyle(node, shape = "box", fillcolor = "Khaki1", rank = node$rank, inherit = FALSE), filterFun = function(node) node$type == "warning")
  x$Do(function(node) SetNodeStyle(node, shape = "box", fillcolor = "Tomato1", rank = node$rank, inherit = FALSE), filterFun = function(node) node$type == "error")
  x$Do(function(node) SetNodeStyle(node, shape = "Mcircle", fillcolor = "Seashell1", rank = node$rank, inherit = FALSE), filterFun = function(node) node$type == "pipe")

  #x$Do(function(node) SetNodeStyle(node, shape = "diamond", fillcolor = "Seashell1", inherit = TRUE, keepExisting = TRUE), filterFun = function(node) node$type == "module")
  x$Do(function(node) SetNodeStyle(node, shape = "component", fillcolor = "Seashell1", rank = node$rank, inherit = TRUE, keepExisting = TRUE), filterFun = function(node) node$type == "tap")
  #x$Do(function(node) SetNodeStyle(node, shape = "component", fillcolor = "Seashell1", inherit = TRUE, keepExisting = TRUE), filterFun = function(node) node$type == "timeseriesref")


  dotLng <- ToGraphViz(x, direction = "climb", pruneFun = pruneFun)
  grViz(dotLng, engine = "dot")
}



#' @importFrom data.tree AreNamesUnique ToDataFrameNetwork
#' @importFrom DiagrammeR create_nodes create_edges create_graph
ToGraphViz <- function(root, direction = c("climb", "descend"), pruneFun = NULL) {
  # get all node styles

  ns <- unique(unlist(sapply(root$Get(function(x) attr(x, "nodeStyle"), simplify = FALSE), names)))

  tr <- Traverse(root, pruneFun = pruneFun)
  anu <- AreNamesUnique(root)
  myargs <- list(nodes = Get(tr, ifelse(anu, "name", "pathString")))
  #need to add label if not all names are unique
  if (!anu && !"label" %in% ns ) ns <- c(ns, "label")
  for (style in ns) {
    myargs[[style]] <- Get(tr, function(x) {
      myns <- data.tree:::GetStyle(x, style, "node")
      if (style == "label" && !anu && length(myns) == 0) myns <- x$name
      if (is.null(myns)) myns <- ""
      myns
    })
  }
  #names(myargs) <- c("nodes", ns)

  nodes <- do.call(create_nodes, myargs)
  if (!anu && !"label" %in% ns) nodes$label <- Get(tr, "name")
  #nodes <- nodes[!names(nodes)=="tooltip"]

  ns <- unique(unlist(sapply(root$Get(function(x) attr(x, "edgeStyle"), simplify = FALSE), names)))


  myargs <- list()
  #see http://stackoverflow.com/questions/19749923/function-factory-in-r
  for (style in ns) {
    myargs[[style]] <- data.tree:::GetEdgeStyleFactory(style)
  }

  #nodes <- do.call(create_nodes, myargs)

  edges <- do.call("ToDataFrameNetwork", c(root,
                                          direction = direction,
                                          pruneFun = pruneFun,
                                          myargs,
                                          papa = function(node) {
                                                    ds <- data.tree::Navigate(node, node$downstream)
                                                    if (anu) res <- ds$name
                                                    else res <- ds$pathString
                                                    return (res)
                                                 }
                                          )
          )

  edges <- edges[c("papa", "to", "dir", "penwidth")]
  names(edges) <- c("from", "to", "dir", "penwidth")

  graphStyle <- attr(root, "graphStyle")
  if (!is.null(graphStyle)) graphAttributes <- paste(names(graphStyle), paste0("'", graphStyle, "'"), sep = " = ", collapse = ", ")
  else graphAttributes <- ""
  nodeAttributes <- data.tree:::GetDefaultStyles(root, type = "node")
  edgeAttributes <- data.tree:::GetDefaultStyles(root, type = "edge")
  graph <- create_graph(nodes, edges, graph_attrs = graphAttributes, node_attrs = nodeAttributes, edge_attrs = edgeAttributes)

  #return (graph)
  #render_graph(graph)
  #cat(graph$dot_code)
  return (graph$dot_code)

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

