#' @export
cutz.heatmap<-function(wide.df, column.name, row.id = "phenotype",
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
      warning("row(s) ", remove.row, " removed because all NAs")
      matrix <- matrix[row.nas == FALSE,]
    }

    #remove columns with all NAs
    col.nas <- colSums(is.na(matrix[1:nrow(matrix),])) == nrow(matrix)
    remove.col <- names(col.nas[col.nas == TRUE])
    if (length(remove.col) > 0) {
      warning("column(s) ", remove.col, " removed because all NAs")
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
    legend(legend = labels, fill = colour, x = 0.55, y = 1, cex = 0.75, ncol = 2)
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
