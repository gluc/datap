

#' @importFrom assertthat assert_that
#' @importFrom data.tree Traverse Get GetAttribute
ParseFun <- function(node) {


  funNme <- node$`function`
  funArgs <- node$arguments
  funArgsNms <- names(funArgs)
  xprs <- parse(text = "list(...)") #required to avoid warning in CHECK

  #print (node$name)
  #if (node$name == "Cache") browser()
  CallStep <- function() {
    #if (node$name == "Cache") browser()
    #print (node$name)
    #parse parameters
    parameterNames <- ls()
    myArgs <- lapply(parameterNames, function(x) get(x))
    names(myArgs) <- parameterNames

    if ("..." %in% names(formals())) ellipsis <- eval(xprs)
    else ellipsis <- list()

    if(CheckCondition(node, myArgs, ellipsis)) {

      funArgs <- SubstituteParameters(node, funArgs, myArgs, ellipsis)
      inflow <- SubstituteInflow(node, funArgs, myArgs, ellipsis)
      funArgs <- SubstituteInflowfun(node, inflow$funArgs)

      res <- do.call.intrnl(funNme, funArgs)
      if (node$type == "warning" || node$type == "error") {
        if (!res[[1]]) {
          msg <- paste0("Joint '", node$name, "' raised ", node$type)
          if (length(res) > 1) mst <- paste0(msg,  ":", res[[2]])
          if (node$type == "warning") warning(msg)
          if (node$type == "error") stop(msg)
        }
        return (inflow$upstreamResults)
      }
      #if (!node$type == "factory") print(paste0("Processed ", node$name))
      return (res)
    } else {

      # condition cannot be on junction, so only one upstream joint
      upstream <- node$Navigate(GetSourcesPath(node, path = "."))$upstream[[1]]
      upstreamArguments <- GetUpstreamFunArguments(node, upstream, myArgs, ellipsis)
      res <- do.call(upstream$fun, upstreamArguments)
      return (res)
    }

  }

  if (node$type == "factory") {
    fun <- CallStep()
  } else {
    CallStep <- SetFormals(CallStep, node)
    fun <- CallStep
  }

  return (fun)

}



# Replaces variables in funArgs that point to a tap parameter
# with the respective argument
#
# @param node the node
# @param funArgs the arguments of the processing step, i.e. the
# function to be called inside the node$fun
# @param myArgs the arguments of the node$fun (coming from the parameters
# declaration)
# @param ellipsis the ellipsis arguments
SubstituteParameters <- function(node, funArgs, myArgs, ellipsis) {

  if (!is.null(myArgs)) {
    for (i in 1:length(funArgs)) {
      v <- funArgs[[i]]
      if (identical(v, "@...")) {
        funArgs <- c(funArgs, ellipsis)
        funArgs <- funArgs[-i]
      } else if (!v %in% paste0('@', VARIABLE_RESERVED_NAMES_CONST) && identical(substr(v, 1, 1), "@")) {
        v <- substr(v, 2, nchar(v))
        if (!is.null(myArgs[[v]])) {
          funArgs[[i]] <- myArgs[[v]]
        }
      } else if (identical(v, '@context')) {
        funArgs[[i]] <- node$root
      }

    }
  }
  return (funArgs)
}


SubstituteInflow <- function(node, funArgs, myArgs, ellipsis) {
  if ("@inflow" %in% node$dynamicVariables) {
    upstreamJoints <- node$upstream
    if (length(upstreamJoints) == 0) stop(paste0("Cannot find @inflow for ", node$name))
    upstreamResults <- lapply(upstreamJoints, function(upstreamJoint) {
      upstreamArguments <- GetUpstreamFunArguments(node, upstreamJoint, myArgs, ellipsis)
      do.call(upstreamJoint$fun, upstreamArguments)
    })
    if (length(upstreamJoints) == 1) upstreamResults <- upstreamResults[[1]]

    funArgs[[which(funArgs == "@inflow")]] <- upstreamResults
  } else upstreamResults <- NULL
  return (list(funArgs = funArgs, upstreamResults = upstreamResults))
}


SubstituteInflowfun <- function(node, funArgs) {
  if ("@inflowfun" %in% node$dynamicVariables) {
    upstream <- node$upstream
    if (length(upstream) == 0) stop(paste0("Cannot find @inflowfun for ", node$name))
    children <- lapply(upstream, function(child) child$fun)
    if (length(upstream) == 1) children <- children[[1]]
    funArgs[[which(funArgs == "@inflowfun")]] <- children
  }
  return (funArgs)
}


SetFormals <- function(CallStep, node) {

  if (length(node$parameters) > 0) {
    parametersList <- node$parameters
    GetParam <- function(name, defaultValue) {
      if (is.character(defaultValue)) defaultValue <- paste0("'", defaultValue, "'")
      if (is.null(defaultValue)) return (paste0(name, " ="))
      return (paste(name, defaultValue, sep = " = "))
    }

    1:length(parametersList) %>%
      lapply(function(i) GetParam(names(parametersList)[[i]], parametersList[[i]])) %>%
      paste(collapse = ", ") %>%
      paste0("alist(", ., ")") %>%
      parse(text = .) %>%
      eval -> formals(CallStep)

  }
  return (CallStep)
}

