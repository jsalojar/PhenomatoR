#' @title Converts long data.frame to wide data.frame
#'
#' @description
#'
#' Converts a molten or long data.frame into a casted or wide data.frame. This
#' function serves as a bridge between \code{\link{bootstrap.model.z}} and
#' \code{\link{cutz.heatmap}}.
#'
#' @import plyr
#'
#' @param z.stats output of \code{\link{bootstrap.model.z}}, or any data.frame
#'   in a long data.frame format
#' @param value character string of name of column with values to fill the wide
#'   data.frame
#' @param row character string of name of column with values to serve as "row
#'   names" of the wide data.frame but listed in a column instead
#' @param col character string of name of column with values to be used as
#'   column names of the wide data.frame
#' @param mc.cores number of cores to use. See \code{\link[parallel]{mclapply}}.
#'
#' @return A wide data.frame.
#'
#' @example
#' ##1 covariant
#' res <- bootstrap.model.z(dataset = PSIdata, phenotype = "AREA_PX", covariant1 = "Plant.Info", covariant1.control = "Col-0", randomeffect = "Tray.ID", p.adjust = c("fdr", "hochberg"))
#' long.to.wide.df(z.stats = res, value = "fdr.z", row = "phenotype", col = "Plant.Info")
#'
#' ##2 covariants
#' res2 <- bootstrap.model.z(dataset = stomatadata, phenotype = "Density(n.stomata/mm2)", covariant1 = "Genotypes", covariant1.control = "Col-0", covariant2 = "Chemical", covariant2.control = "Control")
#' long.to.wide.df(z.stats = res2, value = "z", row = "id", col = "Genotypes")
#'
#' @export
long.to.wide.df<-function(z.stats, value, row, col, mc.cores = 1L) {

  #split data.frame based on id
  z.stats[,row]<-as.factor(z.stats[,row])
  z.stats.list<-split(z.stats, z.stats[,row])

  #create list of data.frames with col values as column names
  z.stats.list<-lapply(z.stats.list, function(x){as.data.frame(matrix(x[,value], nrow = 1, dimnames = list(x[,row][1], x[,col])))})

  #compile into a single data.frame
  wide.df<-plyr::rbind.fill(z.stats.list)
  wide.df$phenotype<-levels(z.stats[,row])

  return(wide.df)
}
