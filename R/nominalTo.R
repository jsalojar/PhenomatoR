#' @title Change data type from nominal to count or ordinal
#'
#' @description Takes a column of nominal data type and converts it to count or
#'   ordinal data type.
#'
#' @section
#'
#' @param dat data.frame with at least a column containing nominal data
#' @param phenotypes character vector of name(s) of column(s) that contains the
#'   nominal data
#' @param orderings character vector that denotes the order of factor levels
#' @param fill.na specify a character string to fill cells with missing data
#'
#' @return The columns to count or ordinal data types are column-binded behind
#'   the original dataset.
#'
#' @examples
#' data("mildewScreen")
#'
#' count.dat <- nominalToCount(mildewScreen, phenotypes = c("BGH_phenotype", "GO_phenotype"), fill.na = "Normal")
#' head(count.dat)
#'
#' ordinal.dat <- nominalToOrdinal(mildewScreen, phenotypes = c("BGH_phenotype", "GO_phenotype"), orderings = list(c("Normal", "brown coloring of infected leaves", "Chlorotic leaves", "few necrotic spots", "Necrotic spots"), c("Normal", "brown coloring of infected leaves", "Chlorotic leaves", "Necrotic spots")), fill.na = "Normal")
#' head(ordinal.dat)
#'
#' @name nominalTo
NULL
#' @rdname nominalTo
#' @export
nominalToCount <- function(dat, phenotypes, fill.na = NULL) {

  #fill NAs with value specified in fill.na argument
  if (is.null(fill.na) == FALSE) {
    for (i in 1:length(phenotypes)) {
      dat[,phenotypes[i]][is.na(dat[,phenotypes[i]])] <- as.character(fill.na)
    }
  }

  #expand levels of a phenotype into columns
  count.dat <- lapply(phenotypes, function(phenotype) {
    dat[,phenotype] <- as.factor(dat[,phenotype])
    count.pheno <- lapply(levels(dat[,phenotype]), function(level) {as.numeric(dat[,phenotype] == level)})
    count.pheno <- do.call("cbind", count.pheno)
    colnames(count.pheno) <- paste0(phenotype, "-", levels(dat[,phenotype]), "-count")
    count.pheno
  })
  count.dat <- do.call("cbind", count.dat)

  #cbind count data behind original data
  dat <- cbind(dat, count.dat)
  return(dat)
}
#' @section
#' @rdname nominalTo
#' @export
nominalToOrdinal <- function(dat, phenotypes, orderings, fill.na = NULL) {

  #fill NAs with value specified in fill.na argument
  if (is.null(fill.na) == FALSE) {
    for (i in 1:length(phenotypes)) {
      dat[,phenotypes[i]][is.na(dat[,phenotypes[i]])] <- as.character(fill.na)
    }
  }

  #convert phenotype data from factor to numeric
  ordinal.dat <- lapply(1:length(phenotypes), function(x) {
    as.numeric(factor(as.character(dat[,phenotypes[x]]), levels = orderings[[x]]))
  })
  ordinal.dat <- do.call("cbind", ordinal.dat)

  #cbind ordinal data behind original data
  colnames(ordinal.dat) <- paste0(phenotypes, "-ordinal")
  dat <- cbind(dat, ordinal.dat)
  return(dat)
}
