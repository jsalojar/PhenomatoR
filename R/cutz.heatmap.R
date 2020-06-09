#' @title Cuts z scores and draws a heatmap
#'
#' @description
#'
#' This function draws non-clustered heatmaps with
#' \code{\link[ggplot2]{geom_tile}} and clustered heatmaps with
#' \code{\link[gplots]{heatmap.2}}.
#'
#' @import ggplot2 gplots graphics grDevices reshape2
#'
#' @param wide.df data.frame of z values to be plotted, wherein 1 column
#'   contains values that specifies the heatmap y axis tick (row) labels
#' @param column.name character string which specifies the name of the x axis
#' @param row.id character string of name of column with y axis tick (row) labels
#' @param breaks numeric vector with elements indicating intervals at which z
#'   values are to be categorized and plotted in the heatmap. By default, z
#'   value intervals are: z<-2.58, -2.58<z<-1.96, -1.96<z<-1.65, -1.65<z<1.65,
#'   1.65<z<1.96, 1.96<z<2.58, z<2.58.
#'
#'   The format to specify customised breaks depends on whether clustering is
#'   performed. For heatmap clustering and no clustering, \code{breaks} are
#'   passed to \code{\link[gplots]{heatmap.2}} and \code{\link[base]{cut}}
#'   respectively.
#' @param labels labels for the categories after breaking
#' @param colour colours for each category after breaking
#' @param print.heatmap if TRUE, heatmap is returned by function
#' @param heatmap.pdf indicate file path and name if heatmap is to be saved in
#'   pdf format
#' @param pdf.size numeric vector of 2 elements which denote the width and
#'   height of the pdf file respectively
#' @param cluster.row logical indicating whether to cluster heatmap rows
#' @param cluster.col logical indicating whether to cluster heatmap columns
#'
#' @return ggplot object is returned if no clustering and \code{print.heatmap} = TRUE.
#'
#' @example
#' require(purrr)
#' bootstrap.model.z(dataset = stomatadata, phenotype = "Density(n.stomata/mm2)", covariant1 = "Genotypes", covariant1.control = "Col-0", covariant2 = "Chemical", covariant2.control = "Control") %>%
#' long.to.wide.df(value = "z", row = "id", col = "Genotypes") %>%
#' cutz.heatmap(column.name = "Genotypes", row.id = "phenotype")
#'
#' @export
cutz.heatmap<-function(wide.df, column.name, row.id,
                       breaks = NULL,
                       labels = c("z<-2.58, p<0.01", "z<-1.96, p<0.05", "z<-1.65, p<0.10", "-1.65<z<1.65, random", "z>1.65, p>0.10", "z>1.96, p>0.05", "z>2.58, p>0.01"),
                       colour = colorRampPalette(c("blue", "white", "firebrick1"))(7),
                       print.heatmap = TRUE, heatmap.pdf = NULL, pdf.size = c(7, 7),
                       cluster.row = FALSE, cluster.col = FALSE,
                       ...) {

  #ggplot2
  if (cluster.row == FALSE && cluster.col == FALSE) {
    #create data.frame for ggplot2 functions
    melted.df<-reshape2::melt(wide.df, id.vars = row.id, variable.name = column.name, value.name = "z")
    melted.df$z<-as.numeric(melted.df$z)

    #cut z scores, assign breaks if not specified
    melted.df$significance_level<-cut(melted.df$z,
                                      breaks = if (is.null(breaks) == TRUE) {c(-Inf, -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, Inf)} else {breaks},
                                      labels = labels, include.lowest = TRUE, right = FALSE)
    if (is.null(breaks) == TRUE) {
      melted.df$significance_level[melted.df$z == -Inf]<-"z<-2.58, p<0.01"
      melted.df$significance_level[melted.df$z == Inf]<-"z>2.58, p>0.01"
    }
    melted.df$significance_level<-as.factor(melted.df$significance_level)

    #create heatmap with ggplot2
    heatmap<-ggplot2::ggplot() +
      ggplot2::geom_tile(ggplot2::aes_string(x = paste("`", column.name, "`", sep = ""),
                                             y = paste("`", row.id, "`", sep = ""),
                                             fill = melted.df$significance_level),
                         colour = "black", data = melted.df) +
      ggplot2::scale_fill_manual(values = colour,
                                 na.value = "grey",
                                 limits = levels(melted.df$significance_level)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
                     axis.ticks = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "z scores, p values"))
  }

  #heatmap.2
  if (cluster.row == TRUE | cluster.col == TRUE) {

    #change wide.df data.frame to matrix
    matrix<-as.matrix(wide.df[,names(wide.df)!=row.id])
    rownames(matrix)<-wide.df[,row.id]

    #remove rows with all NAs
    row.nas <- rowSums(is.na(matrix[,1:ncol(matrix)])) == ncol(matrix)
    remove.row <- names(row.nas[row.nas == TRUE])
    if (length(remove.row) > 0) {
      warning("row(s) ", paste(remove.row, collapse = ", "), " removed because all NAs")
      matrix <- matrix[row.nas == FALSE,]
    }

    #remove columns with all NAs
    col.nas <- colSums(is.na(matrix[1:nrow(matrix),])) == nrow(matrix)
    remove.col <- names(col.nas[col.nas == TRUE])
    if (length(remove.col) > 0) {
      warning("column(s) ", paste(remove.col, collapse = ", "), " removed because all NAs")
      matrix <- matrix[,col.nas == FALSE]
    }

    #assign breaks if not specified
    if (is.null(breaks) == TRUE) {
      min <- range(wide.df[names(wide.df)!=row.id], na.rm = TRUE)[1]
      max <- range(wide.df[names(wide.df)!=row.id], na.rm = TRUE)[2]
      breaks <- c(-3.5, -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, 3.5)
      if (min < breaks[1]) {breaks[1] <- min}
      if (max > breaks[length(breaks)]) {breaks[length(breaks)] <- max}
    }

    heatmap <- gplots::heatmap.2(matrix,
                               dendrogram = if (cluster.row == TRUE && cluster.col == FALSE) {"row"}
                               else if (cluster.row == FALSE && cluster.col == TRUE) {"column"}
                               else if (cluster.row == TRUE && cluster.col == TRUE) {"both"},
                               breaks = breaks, col = colour,
                               trace = "none",
                               sepcolor = "black", sepwidth = c(0.01, 0.01),
                               colsep = 1:ncol(matrix), rowsep = 1:nrow(matrix),
                               na.color = "grey",
                               density.info = "histogram",
                               cexRow = 1, cexCol = 1, margins = c(5,8),
                               lmat = rbind(c(4, 3, 0), c(2, 1, 0)),
                               lwid = c(1.5, 4, 3), lhei = c(1,2),
                               ...)
    graphics::legend(legend = labels, fill = colour, x = 0.55, y = 1, cex = 0.75, ncol = 2)
  }

  ##compile outputs
  #save heatmap as pdf or not
  if (is.null(heatmap.pdf) == FALSE){
    grDevices::pdf(file = paste0(heatmap.pdf), width = pdf.size[1], height = pdf.size[2])
    if (cluster.row == FALSE && cluster.col == FALSE) {print(heatmap)}
    else {
      eval(heatmap$call)
      legend(legend = labels, fill = colour, x = 0.55, y = 1, cex = 0.75, ncol = 2)
    }
    dev.off()
  }

  if (print.heatmap == TRUE) {
    return(heatmap)
  }
}
