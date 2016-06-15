

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
    myArgs <- list()
    for(mvar in joint$parameters) myArgs <- append(myArgs, get(mvar))
    names(myArgs) <- joint$parameters

    if (joint$type == "warning" ||
        joint$type == "error" ||
        joint$functionE$Get(function(n) n$type == "variable" && n$variableName == "inflow") %>% any) {

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
  joint$parameters %>%
    lapply(function(i) paste(i, "= ")) %>%
    paste(collapse = ", ") %>%
    paste0("alist(", ., ")") %>%
    parse(text = .) %>%
    eval
}


GetInflow <- function(joint, myArgs) {
  upstreamJoints <- GetConditionalUpstreamJoints(joint$upstream, myArgs)
  if (length(upstreamJoints) == 0) stop(paste0("Cannot find @inflow for ", joint$name))

  upstreamResults <- lapply(upstreamJoints, function(upstreamJoint) {
    upstreamArguments <- myArgs[upstreamJoint$parameters]
    res <- do.call(upstreamJoint$fun, upstreamArguments)
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
  if (length(joint$condition) == 0) return (TRUE)
  if (is.logical(joint$condition)) return (joint$condition)
  if (joint$condition %>% substr(1, 1) %>% identical('$')) {
    vname <- joint$condition %>% substr(2, nchar(joint$condition))
    if (vname %in% names(myArgs)) return ( myArgs[[vname]] )
  }
  stop(paste0("Cannot resolve condition '", joint$condition, "' on joint '", joint$name, "'"))
}









