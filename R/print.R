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

  printTap <- function(node) formals(node$tap) %>% paste(names(.), ., sep = " = ", collapse = ", ") %>% paste0("tap(", ., ")")

  args <- c(args, "condition", vs, "function", tap = printTap)
  do.call(NextMethod, args)

}


#' @export
print.dataperrorreport <- function(x, ...) {
  NextMethod(x,
             NULL, "type", "errorCount", "code", "message",
             pruneFun = function(joint) joint$`.hasErrors`
             )
}
