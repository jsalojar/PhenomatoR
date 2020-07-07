#' @title Find the number of roots
#'
#' @description Functions find the number of times the name specified in \code{childname}
#'   appears in the \code{parent} list object. By default, the functions search
#'   for the name, "root", therefore, in the context of rsml files, they find the
#'   number of roots.
#'
#'   \code{nChild} finds only the direct lateral roots from the parent.
#'   \code{nDescendants} is a wrapper of \code{nChild}, and finds all roots nested within the parent.
#'
#' @section
#'
#' @param parent an rsml list containing nested root objects
#' @param childname character string of the name you want to search for
#'
#' @return A numeric value
#'
#' @examples
#' data("sparse")
#'
#' nChild(parent = sparse$scene$plant)
#' nChild(parent = sparse$scene$plant$root)
#'
#' nDescendants(parent = sparse$scene$plant)
#'
#' @name nRoots
NULL
#' @rdname nRoots
#' @export
nChild <- function(parent, childname = "root") {
  count <- sum(names(parent) == childname)
  return(count)
}
#' @section
#' @rdname nRoots
#' @export
nDescendants <- function(parent, childname = "root") {

  #set parameters to enter subsequent while loop
  descendants <- 0 #container for number of descendants
  children <- 1 #set arbitrary starting value above 0 to call nchild() at least once
  times <- 1 #write childname once

  #compute number of descendants
  while (children > 0) {
    children <- nChild(parent, childname = paste(rep(childname, times = times), collapse = "."))
    descendants <- descendants + children

    #adjust parameters for next generation
    parent <- unlist(parent, recursive = FALSE)
    times <- times + 1
  }
  return(descendants)
}
