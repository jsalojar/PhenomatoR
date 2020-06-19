#' @title Calculate root length
#'
#' @description Calculate the length of the root by summing distances between
#'   all points of the polyline.
#'
#'   \code{rootLength} calculates the length of a single root.
#'   \code{rootLengthList} calculates the individual length of all roots.
#'   \code{rootSysLength} calculates the cumulative length of all roots.
#'
#' @section
#'
#' @param root root object stored within main rsml list
#' @param route list of sequence of characters that specify the order of objects
#'   to access. See \code{\link{rsmlExtract}} for more details.
#' @param plant plant object stored within main rsml list
#'
#' @return \code{rootLength} and \code{rootSysLength} each returns a single
#'   numeric value, while \code{rootLengthList} returns a list.
#'
#' @examples
#' ##length of primary root
#' rootLength(root = sparse$scene$plant$root)
#'
#' ##length of each root
#' res <- rootLengthList(plant = sparse$scene$plant)
#' View(res)
#'
#' ##cumulative length of all roots
#' rootSysLength(plant = sparse$scene$plant)
#'
#' @name rootLengths
NULL
#' @rdname rootLengths
#' @export
rootLength <- function(root, route = list("geometry", "polyline")) {
  polyline <- rsmlExtract(rsml = root, route = route)
  polyline <- unlist(unlist(polyline, recursive = FALSE), recursive = FALSE)
  polyline <- do.call("rbind", polyline) #convert list to matrix
  polyline <- apply(polyline, 2, as.numeric) #convert data type from character to numeric
  #calculate distance between point in row n and point in row n+1
  #2D
  if (!"z" %in% colnames(polyline)) {
    distances <- lapply(1:(nrow(polyline)-1), function(n) {
      sqrt(((polyline[n, "x"] - polyline[n+1, "x"])^2) + (polyline[n, "y"] - polyline[n+1, "y"])^2)
    })
  }
  #3D
  else {
    distances <- lapply(1:(nrow(polyline)-1), function(n) {
      sqrt(((polyline[n, "x"] - polyline[n+1, "x"])^2) + ((polyline[n, "y"] - polyline[n+1, "y"])^2) + ((polyline[n, "z"] - polyline[n+1, "z"])^2))
    })
  }
  length <- sum(unlist(distances)) #sum all distances to get complete length
  return(length)
}
#' @section
#' @rdname rootLengths
#' @export
rootLengthList <- function(plant, route = list("root", "geometry", "polyline")) {
  polyline <- rsmlExtract(rsml = plant, route = route)
  polyline <- lapply(polyline, function(x) {
    lapply(x, function(y) {
      matrix <- do.call("rbind", y)
      apply(matrix, 2, as.numeric)
    })
  })
  lengths.per.root <- lapply(polyline, function(j) {
    lapply(j, function(i) {
      #2D
      if (!"z" %in% colnames(i)) {
        distances <- lapply(1:(nrow(i)-1), function(n) {
          sqrt(((i[n, "x"] - i[n+1, "x"])^2) + (i[n, "y"] - i[n+1, "y"])^2)
        })
      }
      #3D
      else {
        distances <- lapply(1:(nrow(i)-1), function(n) {
          sqrt(((i[n, "x"] - i[n+1, "x"])^2) + ((i[n, "y"] - i[n+1, "y"])^2) + ((i[n, "z"] - i[n+1, "z"])^2))
        })
      }
      sum(unlist(distances))
    })
  })
  return(lengths.per.root)
}
#' @rdname rootLengths
#' @export
rootSysLength <- function(plant, route = list("root", "geometry", "polyline")) {
  polyline <- rsmlExtract(rsml = plant, route = route)
  polyline <- unlist(polyline, recursive = FALSE)
  polyline <- lapply(polyline, function(x) {
    matrix <- do.call("rbind", x)
    matrix <- apply(matrix, 2, as.numeric)
    matrix
  })
  distances.per.root <- lapply(polyline, function(i) {
    if (!"z" %in% colnames(i)) {
      distances <- lapply(1:(nrow(i)-1), function(n) {
        sqrt(((i[n, "x"] - i[n+1, "x"])^2) + (i[n, "y"] - i[n+1, "y"])^2)
      })
    }
    #3D
    else {
      distances <- lapply(1:(nrow(i)-1), function(n) {
        sqrt(((i[n, "x"] - i[n+1, "x"])^2) + ((i[n, "y"] - i[n+1, "y"])^2) + ((i[n, "z"] - i[n+1, "z"])^2))
      })
    }
  })
  syslength <- sum(unlist(distances.per.root))
  return(syslength)
}
