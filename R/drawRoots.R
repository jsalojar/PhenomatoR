#' @title Plot root system
#'
#' @description Draws root system in a 2D/3D dimensional plot.
#'
#' @import ggplot2 rgl scales
#'
#' @section
#'
#' @param dat data.frame with columns with x and y (and z if present) coordinates, root number, and root order
#' @param x character string of name of column with x coordinates
#' @param y character string of name of column with y coordinates
#' @param z character string of name of column with z coordinates
#' @param colour.by character string of name of column that contains factors for
#'   colour grouping
#' @param root character string of name of column with root number
#' @param palette colour specifications
#'
#' @return
#' \code{drawRoots2D} returns an invisible "gg" class object representing the 2D plot.
#'
#' \code{drawRoots3D} opens a separate window with an interactive display of a 3D plot.
#'
#' @examples
#' ##get data.frame of coordinates
#' data("sparse")
#' df <- plotRootSys(sparse$scene$plant)$coordinates
#'
#' drawRoots2D(df)
#'
#' drawRoots3D(df)
#'
#' @name drawroots
NULL
#' @rdname drawroots
#' @export
drawRoots2D <- function(dat, x = "x", y = "y", colour.by = "order", root = "root", palette = NULL) {
  #draw plot
  plot <- ggplot2::ggplot(data = dat, mapping = ggplot2::aes_string(x = x, y = y, color = colour.by)) +
    ggplot2::geom_point() +
    ggplot2::geom_path(group = dat[,root]) +
    ggplot2::scale_y_reverse() +
    ggplot2::geom_hline(yintercept = 0)
  if (is.null(palette) == FALSE) {
    plot <- plot + ggplot2::scale_colour_manual(values = palette)
  }
  return(plot)
}
#' @section
#' @rdname drawroots
#' @export
drawRoots3D <- function(dat, x = "x", y = "y", z = "z", colour.by = "order", root = "root", palette = NULL) {
  #set same default colour palette as ggplot2
  if (is.null(palette)) {colour.palette <- scales::hue_pal()(length(levels(dat[,colour.by])))}
  #if custom colour(s) are indicated in argument palette
  else {
    colour.palette <- palette
    if (length(palette) < length(levels(dat[,colour.by]))) {warning("insufficient colours indicated in palette")}
  }

  #empty plot
  rgl::plot3d(1, 1, 1, type = "n",
              xlim = c(range(dat[,x])[1], range(dat[,x])[2]),
              ylim = c(range(dat[,y])[1], range(dat[,y])[2]),
              zlim = c(range(dat[,z])[1], range(dat[,z])[2]),
              xlab = "", ylab = "", zlab = "z")

  #draw roots onto empty plot 1 by 1
  for (i in 1:length(levels(dat[,colour.by]))) {
    per.colour <- dat[dat[,colour.by] == levels(dat[,colour.by])[i],]
    per.colour[,root] <- droplevels(per.colour[,root])
    for (j in 1:length(levels(per.colour[,root]))) {
      per.root <- per.colour[per.colour[,root] == levels(per.colour[,root])[j],]
      rgl::plot3d(x = per.root[,x], y = per.root[,y], z = per.root[,z],
                  type = "l", lwd = 2, col = colour.palette[i],
                  add = TRUE)
    }
  }
}
