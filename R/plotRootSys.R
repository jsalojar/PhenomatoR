#' @title Extract coordinates and plot root system
#'
#' @description Extracts coordinates of roots and constructs a 2D plot, as well
#'   as a 3D plot if indicated. \code{plotRootSys} is a wrapper of
#'   \code{\link{drawRoots2D}} and \code{\link{drawRoots3D}}.
#'
#' @import grDevices
#'
#' @param plant plant object stored within main rsml list
#' @param coordinates.route list of sequence of characters that specify the
#'   order of objects to access to extract the coordinates. See
#'   \code{\link{rsmlExtract}} for more details.
#' @param threedim logic. To generate 3D plot or not?
#' @param colour.by character string of name of column that contains factors for
#'   colour grouping
#' @param palette colour specifications
#' @param plot2d.pdf specify file path in a character string to save 2D plot in
#'   a pdf
#' @param pdf.size numeric vector of 2 values corresponding to width and height
#'   of the pdf respectively
#'
#' @return List of 2 elements:
#'
#'   1. a data.frame with columns with x, y and z (if present) coordinates, the
#'   root to which the set of coordinates correspond to, and the root order.
#'
#'   2. invisible "gg" class object representing 2D plot derived from \code{\link[ggplot2]{ggplot2}}
#'
#'   If \code{three.dim} is TRUE, a separate window displaying an interactive 3D
#'   plot is generated.
#'
#' @examples
#' data("sparse")
#' (res <- plotRootSys(sparse$scene$plant))
#'
#' @export
plotRootSys <- function(plant, coordinates.route = list("root", "geometry", "polyline"), three.dim = FALSE,
                        colour.by = "order", palette = NULL,
                        plot2d.pdf = NULL, pdf.size = c(7, 7)) {
  #extract coordinates from all roots
  coordinates <- rsmlExtract(rsml = plant, route = coordinates.route)

  #count number of points per root and per order
  npoints <- unlist(lapply(coordinates, function(x) {
    unlist(lapply(x, function(y) {length(y)}), recursive = FALSE)
  }), recursive = FALSE)
  #count number of points for all roots per order
  npoints.order <- unlist(lapply(coordinates, function(x) {
    length(unlist(x, recursive = FALSE))
  }))

  #convert coordinates list to data.frame
  coordinates <- do.call("rbind", lapply(coordinates, function(x) {
    do.call("rbind", lapply(1:length(x), function(y) {
      per.root <- do.call("rbind", x[[y]])
      per.root <- apply(per.root, 2, as.numeric)
      as.data.frame(per.root)
    }))
  }))

  #indicate which root and order the set of coordinates in the corresponding row are derived from
  coordinates$root <- as.factor(unlist(lapply(1:length(npoints), function(x) {
    rep(x, times = npoints[x])
  })))
  coordinates$order <- as.factor(unlist(lapply(1:length(npoints.order), function(x) {
    rep(x, times = npoints.order[x])
  })))
  rownames(coordinates) <- NULL

  #draw plot
  plot2D <- drawRoots2D(dat = coordinates, x = "x", y = "y",
                        colour.by = colour.by, root = "root", palette = palette)
  if (three.dim == TRUE) {
    drawRoots3D(dat = coordinates, x = "x", y = "y", z = "z",
                colour.by = colour.by, root = "root", palette = palette)
  }

  if (is.null(plot2d.pdf) == FALSE) {
    grDevices::pdf(file = paste0(plot2d.pdf), width = pdf.size[1], height = pdf.size[2])
    print(plot2D)
    grDevices::dev.off()
  }

  #compile outputs
  out <- list()
  out[["coordinates"]] <- coordinates
  out[["plot"]] <- plot2D
  return(out)
}
