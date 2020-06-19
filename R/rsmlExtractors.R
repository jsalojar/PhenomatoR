#' @title Extract specific objects from a nested list
#'
#' @description From the list specified in argument \code{rsml}, nested objects
#'   with the name specified in the first character element of argument
#'   \code{route} are searched for. For every object found, elements are
#'   accessed based on the subsequent elements specified in \code{route} after
#'   the first, if any.
#'
#'   \code{rsmlExtract} is a wrapper of \code{rsmlExtract0}. The difference
#'   between the 2 functions is that \code{rsmlExtract} accepts a nested list
#'   in its argument \code{route}, unlike \code{rsmlExtract0}. List nesting
#'   must occur if there are multiple objects with the same name to be accessed.
#'
#' @section
#'
#' @param rsml a list object with rsml's nested document structure
#' @param route a list wherein character elements of the objects' names are
#'   specified in the order in which they are accessed
#'
#' @return A list. The first and second levels of the list correspond to the
#'   order and number of objects with the name specified in the first element of
#'   \code{route} respectively. The third level would be the object accessed in
#'   the final element of \code{route}.
#'
#' @examples
#' ##extract coordinates from all roots
#' res1 <- rsmlExtract0(sparse$scene$plant, list("root", "geometry", "polyline"))
#' View(res1)
#'
#' ##extract only x coordinates from all roots
#' res2 <- rsmlExtract(sparse$scene$plant, list("root", "geometry", "polyline", list("point", "x")))
#' View(res2)
#'
#' ##When you do not specify a nested list in route argument, only the first point is accessed
#' res3 <- rsmlExtract(sparse$scene$plant, list("root", "geometry", "polyline", "point", "x"))
#' View(res3)
#'
#' @name rsmlExtractors
NULL
#' @rdname rsmlExtractors
#' @export
rsmlExtract0 <- function(rsml, route) {
  #ensure first element in route is a name found in rsml
  if (is.character(route[[1]]) && !route[[1]] %in% names(rsml)) {
    stop(route[[1]], " is not found in names(", deparse(substitute(rsml)), ")")
  }

  first.route <- route[[1]] #save first element of route
  container <- list() #compile extracted values from rsml

  #specify parameters to enter subsequent while loop
  children <- 1 #arbitrary number but must be > 0
  order <- 1 #begin by extracting values from first-order roots

  #extract values from rsml as specified by route
  while(children > 0) {
    route[[1]] <- paste(rep(first.route, times = order), collapse = ".") #rewrite first element of route after every loop
    index <- which(names(rsml) %in% route[[1]]) #record all indexes with the same name as the first element in route

    #access rsml according to route
    container[[paste("order", order)]] <- lapply(index, function(i) {
      rsml <- rsml[[i]] #access each index
      if (length(route) >= 2) {
        x <- 2
        while (x <= length(route)) {
          #again, ensure first element in route is a name found in rsml
          if (is.character(route[[x]]) && !route[[x]] %in% names(rsml)) {
            stop(route[[x]], " not found after ", route[[x-1]])
          }
          rsml <- rsml[[route[[x]]]] #access elements (elements after the first) specified in route
          x <- x + 1
        }
      }
      rsml
    })

    #adjust parameters for next cycle
    rsml <- unlist(rsml, recursive = FALSE)
    children <- sum(names(rsml) == paste(rep(first.route, times = order + 1), collapse = "."))
    order <- order + 1
  }

  #assign names to roots under each order
  container <- lapply(container, function(x) {
    names(x) <- paste(first.route, 1:length(x))
    x
  })
  return(container)
}
#' @section
#' @rdname rsmlExtractors
#' @export
rsmlExtract <- function(rsml, route) {
  #if route is not a nested list, simply pass to rsmlExtract0
  if (length(route) == 1) {rsml <- rsmlExtract0(rsml = rsml, route = route)}
  if (length(route) > 1 &&
      is.list(route[[length(route)]]) == FALSE) {rsml <- rsmlExtract0(rsml = rsml, route = route)}

  #if route is a nested list
  if (is.list(route[[length(route)]]) == TRUE && length(route) > 1) {
    rsml <- rsmlExtract0(rsml = rsml, route = route[1:(length(route)-1)]) #extract values along route before the last element of the first level

    while (is.list(route[[length(route)]])) { #proceed if last element of the first level of route is a list
      route <- route[[length(route)]] #rewrite route to remove first level of route

      #after rewriting route, proceed if last element of first level of route is still a list
      if (is.list(route[[length(route)]]) == TRUE) {
        #for each root in each order, extract values along route before the last element of the first level
        rsml <- lapply(rsml, function(order) {
          lapply(order, function(root) {
            #unlist to prevent additional layers in rsml list
            unlist(unlist(rsmlExtract0(rsml = root, route = route[1:(length(route)-1)]),
                          recursive = FALSE, use.names = FALSE),
                   recursive = FALSE)
          })
        })
      }
      #after rewriting route, proceed if last element of first level of route is no longer a list
      else {
        #for each root in each order, extract values along all elements in route
        rsml <- lapply(rsml, function(order) {
          lapply(order, function(root) {
            #unlist to prevent additional layers in rsml list
            unlist(unlist(rsmlExtract0(rsml = root, route = route[1:length(route)]),
                          recursive = FALSE, use.names = FALSE),
                   recursive = FALSE)
          })
        })
      }
    }
  }
  return(rsml)
}
