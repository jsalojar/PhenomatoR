#' @title Converts long data.frame to wide data.frame
#'
#' @description
#'
#' Converts a long data.frame to a wide data.frame.
#'
#' Function serves as a bridge between \code{\link{bootstrapModelZ}} and
#' \code{\link{heatmapper}}.
#'
#' @import plyr
#'
#' @param z.stats output of \code{\link{bootstrapModelZ}}, or any data.frame
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
#' @examples
#' ##1 covariant
#' data("PSIdata", package = "PhenomatoR")
#' res1 <- bootstrapModelZ(dataset = PSIdata, phenotype = "AREA_PX", covariant1 = "Plant.Info", covariant1.control = "Col-0", random.effects = "Tray.ID", p.adjust = c("fdr", "hochberg"))
#' stretchDf(z.stats = res1, value = "fdr.z", row = "phenotype", col = "Plant.Info")
#'
#' ##2 covariants
#' data("stomataData", package = "PhenomatoR")
#' res2 <- bootstrapModelZ(dataset = stomataData, phenotype = "Density(n.stomata/mm2)", covariant1 = "Genotypes", covariant1.control = "Col-0", covariant2 = "Chemical", covariant2.control = "Control")
#' stretchDf(z.stats = res2, value = "z", row = "id", col = "Genotypes")
#'
#' @export
stretchDf <- function(z.stats, value, row, col, mc.cores = 1L) {

  #split data.frame based on id
  z.stats[,row] <- factor(z.stats[,row], levels = unique(z.stats[,row]))
  z.stats.list <- split(z.stats, z.stats[,row])

  #create list of data.frames with col values as column names
  z.stats.list <- lapply(z.stats.list, function(x){as.data.frame(matrix(x[,value], nrow = 1, dimnames = list(x[,row][1], x[,col])))})

  #compile into a single data.frame
  wide.df <- plyr::rbind.fill(z.stats.list)
  phenotype <- levels(z.stats[,row])
  wide.df <- cbind(phenotype, wide.df)

  return(wide.df)
}
