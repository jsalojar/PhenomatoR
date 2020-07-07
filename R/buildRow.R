#' @title Compile outputs from functions into a single-row matrix or data.frame
#'
#' @description Applies specified functions to the object in the first argument
#'   and then compiles the output into a single-row data.frame.
#'
#' @param plant object to apply functions to. In rsml context, the plant object
#'   stored within the main rsml list.
#' @param single.value.functions character vector of names of functions to be
#'   called that will output a single value
#' @param single.value.function.labels character vector of column names for the
#'   output of the functions specified in \code{single.value.functions}
#' @param multi.value.functions character vector of names of functions to be
#'   called that will output a single-row matrix or data.frame
#'
#' @return A single-row matrix or data.frame with number of columns depending on the number of functions applied. Values pertain to the outputs of the functions applied.
#'
#' @examples
#' data("sparse", package = "PhenomatoR")
#'
#' ##apply PhenomatoR's functions
#' buildRow(sparse$scene$plant,
#'          single.value.functions = c("rootSysLength", "nDescendants"),
#'          single.value.function.labels = c("total root length", "total no. of roots"),
#'          multi.value.functions = "rootOntology")
#'
#' ##apply base functions
#' buildRow(sparse$scene$plant, single.value.functions = c("is.list", "is.data.frame", "is.vector"))
#'
#' @export
buildRow <- function(plant,
                     single.value.functions = NULL, single.value.function.labels = NULL,
                     multi.value.functions = NULL) {
  #stop if functions are not character strings or no functions are indicated at all
  if (!is.null(single.value.functions) && !is.character(single.value.functions)) {
    stop("arguments single.value.functions accepts only character vectors of names of functions")
  }
  if (!is.null(multi.value.functions) && !is.character(multi.value.functions)) {
    stop("argument multi.value.functions accepts only character vectors of names of functions")
  }
  if (is.null(single.value.functions) && is.null(multi.value.functions)) {
    stop("please input at least 1 function name")
  }

  #apply each function in argument single.value.function on object in first argument
  if (!is.null(single.value.functions)) {
    single.evals <- lapply(1:length(single.value.functions), function(x){
      eval <- tryCatch(get(single.value.functions[x])(plant), error = function(e) {NA})
      if (TRUE %in% is.na(eval)) {warning("errors are printed as NA")}
      eval
    })
    if (is.null(single.value.function.labels)) {names(single.evals)[1:length(single.value.functions)] <- single.value.functions}
    else {names(single.evals) <- single.value.function.labels}
  }
  else {single.evals <- NULL}

  #apply each function in argument multi.value.function on object in first argument
  if (!is.null(multi.value.functions)) {
    multi.evals <- lapply(1:length(multi.value.functions), function(x){
      eval <- tryCatch(get(multi.value.functions[x])(plant), error = function(e) {NA})
      if (TRUE %in% is.na(eval)) {warning("errors are printed as NA")}
      eval
    })
  }
  else {multi.evals <- NULL}

  #compile into a single row
  evals <- do.call("cbind", c(single.evals, multi.evals))
  return(evals)
}
