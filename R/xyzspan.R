#' @title Find x, y or z span of root(s)
#'
#' @description Calculates how long the root(s) spans along the x, y or z axis.
#'   \code{rootXSpan}, \code{rootYSpan} and \code{rootZSpan} finds the x, y and
#'   z span of a single root respectively. \code{rootSysXSpan},
#'   \code{rootSysYSpan} and \code{rootSysZSpan} finds the x, y and z span of
#'   the entire root system respectively.
#'
#' @section
#'
#' @param root root object stored within main rsml list
#' @param route list of sequence of characters that specify the order of objects
#'   to access. See \code{\link{rsmlExtract}} for more details.
#' @param plant plant object stored within main rsml list
#'
#' @return A numeric value
#'
#' @examples
#' data("sparse", package = "PhenomatoR")
#'
#' rootXSpan(root = sparse$scene$plant$root)
#' rootSysXSpan(plant = sparse$scene$plant)
#'
#' rootYSpan(root = sparse$scene$plant$root)
#' rootSysYSpan(plant = sparse$scene$plant)
#'
#' rootZSpan(root = sparse$scene$plant$root)
#' rootSysZSpan(plant = sparse$scene$plant)
#'
#' @name xyzspan
NULL
#' @rdname xyzspan
#' @export
rootXSpan <- function(root, route = list("geometry", "polyline", list("point", "x"))) {
  x <- rsmlExtract(rsml = root, route = route)
  x <- as.numeric(unlist(x))
  xspan <- range(x)[2] - range(x)[1]
  return(xspan)
}
#' @section
#' @rdname xyzspan
#' @export
rootYSpan <- function(root, route = list("geometry", "polyline", list("point", "y"))) {
  y <- rsmlExtract(rsml = root, route = route)
  y <- as.numeric(unlist(y))
  yspan <- range(y)[2] - range(y)[1]
  return(yspan)
}
#' @rdname xyzspan
#' @export
rootZSpan <- function(root, route = list("geometry", "polyline", list("point", "z"))) {
  z <- rsmlExtract(rsml = root, route = route)
  z <- as.numeric(unlist(z))
  zspan <- range(z)[2] - range(z)[1]
  return(zspan)
}
#' @rdname xyzspan
#' @export
rootSysXSpan <- function(plant, route = list("root", "geometry", "polyline", list("point", "x"))) {
  x <- rsmlExtract(rsml = plant, route = route)
  x <- as.numeric(unlist(x))
  xspan <- range(x)[2] - range(x)[1]
  return(xspan)
}
#' @rdname xyzspan
#' @export
rootSysYSpan <- function(plant, route = list("root", "geometry", "polyline", list("point", "y"))) {
  y <- rsmlExtract(rsml = plant, route = route)
  y <- as.numeric(unlist(y))
  yspan <- range(y)[2] - range(y)[1]
  return(yspan)
}
#' @rdname xyzspan
#' @export
rootSysZSpan <- function(plant, route = list("root", "geometry", "polyline", list("point", "z"))) {
  z <- rsmlExtract(rsml = plant, route = route)
  z <- as.numeric(unlist(z))
  zspan <- range(z)[2] - range(z)[1]
  return(zspan)
}
