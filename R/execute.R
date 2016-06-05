

#' @importFrom assertthat assert_that
#' @importFrom data.tree Traverse Get GetAttribute
ParseFun <- function(joint) {

  funNme <- joint$`function`
  funArgs <- joint$arguments

  xprs <- parse(text = "list(...)") #parse is required to avoid warning in CHECK

  #print (joint$name)
  #if (joint$name == "Cache") browser()
  CallStep <- function() {
    #if (joint$name == "Cache") browser()
    #browser()
    #print (joint$name)
    #parse parameters
    parameterNames <- ls()
    myArgs <- lapply(parameterNames, function(x) get(x))
    names(myArgs) <- parameterNames

    if ("..." %in% names(formals())) ellipsis <- eval(xprs)
    else ellipsis <- list()

    funArgs <- SubstituteParameters(joint, funArgs, myArgs, ellipsis)
    inflow <- SubstituteInflow(joint, funArgs, myArgs, ellipsis)
    funArgs <- inflow$funArgs

    res <- do.call.intrnl(funNme, funArgs)
    if (joint$type == "warning" || joint$type == "error") {
      if (!res[[1]]) {
        msg <- paste0("Joint '", joint$name, "' raised ", joint$type)
        if (length(res) > 1) mst <- paste0(msg,  ":", res[[2]])
        if (joint$type == "warning") warning(msg)
        if (joint$type == "error") stop(msg)
      }
      return (inflow$upstreamResults)
    }
    return (res)

  }

  if (joint$type == "factory") {
    fun <- CallStep()
  } else {
    CallStep <- SetFormals(CallStep, joint)
    fun <- CallStep
  }

  return (fun)

}




# Replaces variables in funArgs that point to a tap parameter
# with the respective argument
#
# @param joint the joint
# @param funArgs the arguments of the processing step, i.e. the
# function to be called inside the joint$fun
# @param myArgs the arguments of the joint$fun (coming from the parameters
# declaration)
# @param ellipsis the ellipsis arguments
SubstituteParameters <- function(joint, funArgs, myArgs, ellipsis) {

  if (!is.null(myArgs)) {
    for (i in 1:length(funArgs)) {
      v <- funArgs[[i]]
      if (is(v, 'variable') && identical(as.character(v), '$...')) {
        funArgs <- c(funArgs, ellipsis)
        funArgs <- funArgs[-i]
      } else if (!v %in% paste0('$', VARIABLE_RESERVED_NAMES_CONST) && is(v, 'variable')) {
        v <- substr(v, 2, nchar(v))
        if (!is.null(myArgs[[v]])) {
          funArgs[[i]] <- myArgs[[v]]
        }
      } else if (v == '$context') {
        funArgs[[i]] <- joint$root
      } else if (v == '$joint') {
        funArgs[[i]] <- joint
      }

    }
  }
  return (funArgs)
}


SubstituteInflow <- function(joint, funArgs, myArgs, ellipsis) {
  if ("$inflow" %in% joint$dynamicVariables) {

    upstreamJoints <- GetConditionalUpstreamJoints(joint$upstream, myArgs, ellipsis)
    if (length(upstreamJoints) == 0) stop(paste0("Cannot find @inflow for ", joint$name))

    upstreamResults <- lapply(upstreamJoints, function(upstreamJoint) {
      upstreamArguments <- GetUpstreamFunArguments(joint, upstreamJoint, myArgs, ellipsis)
      res <- do.call(upstreamJoint$fun, upstreamArguments)
      return (res)
    })
    if (length(upstreamJoints) == 1) upstreamResults <- upstreamResults[[1]]

    funArgs[[which(funArgs == "$inflow")]] <- upstreamResults
  } else upstreamResults <- NULL
  return (list(funArgs = funArgs, upstreamResults = upstreamResults))
}




GetConditionalUpstreamJoints <- function(upstreamJoints, myArgs, ellipsis) {
  upstreamJoints <- sapply(upstreamJoints, function(upstreamJoint) {
    isCondition <- CheckCondition(upstreamJoint, myArgs, ellipsis)
    #if (!is.logical(isCondition)) browser()
    if(isCondition) return (upstreamJoint)
    upstreamJoints <- upstreamJoint$Navigate(GetSourcesPath(upstreamJoint, path = "."))$upstream
    return (GetConditionalUpstreamJoints(upstreamJoints, myArgs, ellipsis))
  })
}


CheckCondition <- function(joint, myArgs, ellipsis) {
  if (length(joint$condition) == 0) return (TRUE)
  if (is.logical(joint$condition)) return (joint$condition)
  if (joint$condition %>% substr(1, 1) %>% identical('$')) {
    vname <- joint$condition %>% substr(2, nchar(joint$condition))
    if (vname %in% names(myArgs)) return ( myArgs[[vname]] )
    if (vname %in% names(ellipsis)) return (ellipsis[[vname]])
  }
  stop(paste0("Cannot resolve condition '", joint$condition, "' on joint '", joint$name, "'"))
}




SetFormals <- function(CallStep, joint) {

  if (length(joint$parameters) > 0) {
    parametersList <- joint$parameters
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




GetVariableValue <- function(node, name) {
  if (node$isRoot) return (NULL)
  if (!is.null(node$variablesE)) {
    cand <- node$variablesE[name]
    if (!is.null(cand)) return(cand)
  }
  return (GetVariableValue(node$parent, name))
}

