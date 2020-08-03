#' @title Calculate the surface area and volume of root system
#'
#' @description The surface area and volume between 2 adjacent points in a root
#'   is calculated based on the truncated cone model. Surface areas and volumes
#'   for all adjacent points for all roots are then calculated and summed to
#'   give the total surface area and volume of the entire root system by
#'   \code{rootSysSurfArea} and \code{rootSysVol} respectively.
#'
#' @section
#'
#' @param plant plant object stored within main rsml list
#' @param coordinates.route list of sequence of characters that specify the
#'   order of objects to access to extract the coordinates. See
#'   \code{\link{rsmlExtract}} for more details.
#' @param diameter.route list of sequence of characters that specify the order
#'   of objects to access to extract the diameters. See
#'   \code{\link{rsmlExtract}} for more details.
#'
#' @return A numeric value
#'
#' @examples
#' data("sparse", package = "PhenomatoR")
#'
#' rootSysSurfArea(sparse$scene$plant)
#'
#' rootSysVol(sparse$scene$plant)
#'
#' @name geometry
NULL
#' @rdname geometry
#' @export
rootSysSurfArea <- function(plant,
                            coordinates.route = list("root", "geometry", "polyline"),
                            diameter.route = list("root", "functions", "function", list("sample"))) {

  #extract all coordinates for all roots
  polyline <- rsmlExtract(rsml = plant, route = coordinates.route)
  polyline <- lapply(polyline, function(order) { #arrange coordinates into a data.frame per root per order
    lapply(order, function(root) {
      matrix <- do.call("rbind", root)
      apply(matrix, 2, as.numeric)
    })
  })

  #calculate distances between adjacent points for every root
  distances.per.root <- lapply(polyline, function(order) {
    lapply(order, function(root) {
      #2D
      if (!"z" %in% colnames(root)) {
        distances <- lapply(1:(nrow(root)-1), function(n) {
          sqrt(((root[n, "x"] - root[n+1, "x"])^2) + (root[n, "y"] - root[n+1, "y"])^2)
        })
      }
      #3D
      else {
        distances <- lapply(1:(nrow(root)-1), function(n) {
          sqrt(((root[n, "x"] - root[n+1, "x"])^2) + ((root[n, "y"] - root[n+1, "y"])^2) + ((root[n, "z"] - root[n+1, "z"])^2))
        })
      }
    })
  })
  distances <- unlist(distances.per.root, recursive = FALSE) #unlist once since root order details are not needed
  distances <- lapply(distances, unlist) #convert list of distances between adjacent points per root into a vector

  #get diameters per point for every root
  diameters <- unlist(rsmlExtract(rsml = plant, route = diameter.route), recursive = FALSE)

  #calculate surface area
  surf.area.bw.points <- lapply(1:length(distances), function(rootn) {
    lapply(1:length(distances[[rootn]]), function(x) {
      rad1 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[2]/2 #larger radius
      rad2 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[1]/2 #smaller radius
      if (rad1 == rad2) {rad1 * 2 * pi * distances[[rootn]][x]}
      else {
        pi * (rad1 + rad2) * sqrt(as.numeric(distances[[rootn]][x])^2 + (rad1 - rad2)^2)
      }
    })
  })
  total.surf.area <- sum(unlist(surf.area.bw.points))
  return(total.surf.area)
}
#' @section
#' @rdname geometry
#' @export
rootSysVol <- function(plant,
                       coordinates.route = list("root", "geometry", "polyline"),
                       diameter.route = list("root", "functions",  "function", list("sample"))) {
  ##extract all coordinates for all roots
  polyline <- rsmlExtract(rsml = plant, route = coordinates.route)
  polyline <- lapply(polyline, function(x) { #arrange coordinates into a data.frame per root per order
    lapply(x, function(y) {
      matrix <- do.call("rbind", y)
      matrix <- apply(matrix, 2, as.numeric)
      matrix
    })
  })

  #calculate distances between adjacent points for every root
  distances <- lapply(polyline, function(order) {
    lapply(order, function(root) {
      #2D
      if (!"z" %in% colnames(root)) {
        distances <- lapply(1:(nrow(root)-1), function(n) {
          sqrt(((root[n, "x"] - root[n+1, "x"])^2) + (root[n, "y"] - root[n+1, "y"])^2)
        })
      }
      #3D
      else {
        distances <- lapply(1:(nrow(root)-1), function(n) {
          sqrt(((root[n, "x"] - root[n+1, "x"])^2) + ((root[n, "y"] - root[n+1, "y"])^2) + ((root[n, "z"] - root[n+1, "z"])^2))
        })
      }
    })
  })
  distances <- unlist(distances, recursive = FALSE) #unlist once since root order details are not needed
  distances <- lapply(distances, unlist) #convert list of distances between adjacent points per root into a vector

  #get diameters per point for every root
  diameters <- unlist(rsmlExtract(rsml = plant, route = diameter.route), recursive = FALSE)

  #calculate volume
  vol.bw.points <- lapply(1:length(distances), function(rootn) {
    lapply(1:length(distances[[rootn]]), function(x) {
      rad1 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[2]/2 #smaller radius
      rad2 <- range(c(as.numeric(diameters[[rootn]][x]), as.numeric(diameters[[rootn]][x+1])))[1]/2 #larger radius
      if (rad1 == rad2) {rad1^2 * pi * distances[[rootn]][x]}
      else {
        1/3 * pi * distances[[rootn]][x] * (rad1^2 + rad1*rad2 + rad2^2)
      }
    })
  })
  total.vol <- sum(unlist(vol.bw.points))
  return(total.vol)
}
