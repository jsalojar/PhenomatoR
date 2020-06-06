#' @title Change data.frame with phenotype as factor or character to numeric
#'
#' @description Splits a column containing phenotypes by character/factor type
#'   into columns each specifying a phenotype. If the specific plant in the
#'   corresponding row exhibits one of the phenotypes, the column would have a
#'   value of 1, while the other columns would have 0s.
#'
#' @param dataset data.frame with a column containing phenotypes
#' @param null.pheno if not already specified, specify the name you want to
#'   label for plants with no observed disease phenotypes
#' @param phenotypes name of column that contains all phenotypes
#'
#' @return Returns a list with a vector of names of columns of phenotypes added
#'   to the original `dataset`, as well as the original `dataset` with
#'   additional columns specifying numeric logical values per phenotype.
#'
#'   The data.frame output can be fed into functions from families
#'   \code{\link{sample.model.z}} and \code{\link{combine.sample.model.z.gg}}.
#'
#' @examples
#' ##create dataset
#' dataset.pheno<-data.frame(phenotype = rep(c("yellow streaks", "brown colouration", "black spots", "wilting", ""), each = 5, times = 3))
#' factor.dataset.transform(dataset = dataset.pheno, phenotypes = "phenotype", null.pheno = "normal")
#'
#' ##Using bloody.roots
#' factor.dataset.transform(dataset = bloody.roots.raw, phenotypes = "BGH_phenotype", null.pheno = "Normal")
#' #>PhenomatoR::bloody.roots.xformed
#' #save<-nominalToCount(bloody.roots, c("BGH_phenotype", "GO_phenotype"), "Normal")
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
