#' @importFrom data.tree ToDataFrameTree
#' @export
print.context <- function(x, ...) {
  df <- ToDataFrameTree(x, pruneFun = function(node) {
    !identical(node$parent$type, "tap")
  })
  print(df)
}


#' @export
print.tap <- function(x, ...) {

  args <- list(x,
               NULL,
               "type",
               downstream = function(n) paste(n$downstream, collapse = "/")
               )

  #variables
  nvar <- x$Get(function(j) length(j$variables)) %>% max

  if (nvar > 0) {
    vs <- lapply(1:nvar, function(index) {
      function(node) {
        if (length(node$variables) < index) return ("")
        nm <- names(node$variables)[index]
        vl <- node$variables[[index]]
        if (length(nm) > 0 && length(vl) > 0) sep = ": "
        else sep = ""
        res <- paste(nm, vl, sep = sep)
        return (res)
      }
    })
    names(vs) <- paste0("var", as.character(1:nvar))
  } else vs <- NULL

  #args
  nargs <- x$Get(function(j) length(j$arguments)) %>% max

  if (nargs > 0) {
    as <- lapply(1:nargs, function(index) {
      function(node) {
        if (length(node$arguments) < index) return ("")
        nm <- names(node$arguments)[index]
        vl <- node$arguments[[index]]
        if (length(nm) > 0 && length(vl) > 0) sep = ": "
        else sep = ""
        res <- paste(nm, vl, sep = sep)
        return (res)
      }
    })
    names(as) <- paste0("arg", as.character(1:nargs))
  } else as <- NULL


  #parameters
  npar <- x$Get(function(j) length(j$parameters)) %>% max

  if (npar > 0) {
    ps <- lapply(1:npar, function(index) {
      function(node) {
        if (length(node$parameters) < index) return ("")
        nm <- names(node$parameters)[index]
        vl <- node$parameters[[index]]
        if (length(nm) > 0 && length(vl) > 0) sep = ": "
        else sep = ""
        res <- paste(nm, vl, sep = sep)
        return (res)
      }
    })
    names(ps) <- paste0("par", as.character(1:npar))
  } else ps <- NULL


  args <- c(args, "condition", ps, vs, "function", as)
  do.call(NextMethod, args)

}


#' @export
print.dataperrorreport <- function(x, ...) {
  NextMethod(x,
             NULL, "type", "errorCount", "code", "message",
             pruneFun = function(joint) joint$`.hasErrors`
             )
}
