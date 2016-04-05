
#' Loads a meta definition.
#'
#' @param con a connection containing the meta data
#'
#' @examples
#' filePath <- system.file("extdata", "sample_metadata.yaml", package="finPrice")
#' db <- Load(filePath)
#'
#' @importFrom yaml yaml.load
#' @importFrom data.tree FromListExplicit Do
#' @export
Load <- function(con) {
  yamlString <- paste0(readLines(con), collapse = "\n")
  lol <- yaml.load(yamlString)
  lol <- list(children = lol)
  tree <- FromListExplicit(lol)

  #replace function
  tree$Do(fun = ParseFun,
          filterFun = function(node) !is.null(node$fun)
          )

  #return
  return (tree)
}


#' Get a timeseries
#'
#' @param code the code of the timeseries
#' @param db the db object
#' @param ... and additional parameters that need to be passed to the function
#'
#' @export
Get <- function(code, db, ...) {
  db$Climb(code)$fun(...)
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
