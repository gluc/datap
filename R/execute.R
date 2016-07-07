

#' @importFrom assertthat assert_that
#' @importFrom data.tree Traverse Get GetAttribute
ParseFun <- function(joint) {

  funNme <- joint$functionE$funName

  Get(joint$functionE$children, "name") %>%
    unname ->
    funArgNames

  CallStep <- function() {
    # CallStep is called by downstream, providing parameters
    # It's the R function representation of joint$functionE
    #myArgs <- lapply(joint$parameters, get)
    #browser()
    myArgs <- list()
    for(mvar in names(joint$parametersE)) myArgs <- append(myArgs, get(mvar))
    names(myArgs) <- names(joint$parametersE)
    myArgs$joint <- joint

    if (joint$type == "warning" ||
        joint$type == "error" ||
        ContainsVariable(joint$functionE, "inflow")) {

      inflow <- GetInflow(joint, myArgs)

      myArgs$inflow <- inflow

    }

    res <- Evaluate(joint$functionE, myArgs)

    if (joint$type == "warning" || joint$type == "error") {
      if (!res[[1]]) {
        msg <- paste0("Joint '", joint$name, "' raised ", joint$type)
        if (length(res) > 1) mst <- paste0(msg,  ":", res[[2]])
        if (joint$type == "warning") warning(msg)
        if (joint$type == "error") stop(msg)
      }
      return (inflow)
    }
    return (res)

  }


  formals(CallStep) <- GetFormals(joint)

  return (CallStep)
}



GetFormals <- function(joint) {
  joint$parametersE %>%
    names %>%
    lapply(GetParameterFormals, joint) %>%
    paste(collapse = ", ") %>%
    paste0("alist(", ., ")") %>%
    parse(text = .) %>%
    eval
}


GetParameterFormals <- function(name, joint) {
  prm <- joint$parametersE[[name]]
  defaultArg <- ""
  if (!is.null(prm)) defaultArg <- Deparse(prm)
  paste(name, "= ", defaultArg)
}





GetInflow <- function(joint, myArgs) {
  upstreamJoints <- GetConditionalUpstreamJoints(joint$upstream, myArgs)
  if (length(upstreamJoints) == 0) stop(paste0("Cannot find @inflow for ", joint$name))

  upstreamResults <- lapply(upstreamJoints, function(upstreamJoint) {
    upstreamArguments <- myArgs[names(upstreamJoint$parametersE)]
    res <- do.call(upstreamJoint$tap, upstreamArguments)
    return (res)
  })
  if (length(upstreamJoints) == 1) upstreamResults <- upstreamResults[[1]]

  return (upstreamResults)
}






GetConditionalUpstreamJoints <- function(upstreamJoints, myArgs) {
  upstreamJoints <- sapply(upstreamJoints, function(upstreamJoint) {
    isCondition <- CheckCondition(upstreamJoint, myArgs)
    #if (!is.logical(isCondition)) browser()
    if(isCondition) return (upstreamJoint)
    upstreamJoints <- upstreamJoint$Navigate(GetSourcesPath(upstreamJoint, path = "."))$upstream
    return (GetConditionalUpstreamJoints(upstreamJoints, myArgs))
  })
}


CheckCondition <- function(joint, myArgs) {
  if (length(joint$conditionE) == 0) return (TRUE)
  Evaluate(joint$conditionE, myArgs)
}









