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
#'
#' @export
factor.to.count<-function(dataset, phenotypes, null.type = NULL, null.pheno = NULL){

  #provide label for phenotypes with empty character string
  if (null.type == "character" && is.null(null.pheno) == FALSE) {
    dataset[,phenotypes]<-as.factor(as.character(dataset[,phenotypes]))
    levels(dataset[,phenotypes])[1]<-null.pheno
  }
  #provide label for phenotypes with NA
  else if (null.type == "NA" && is.null(null.pheno) == FALSE) {
    dataset[,phenotypes]<-as.character(dataset[,phenotypes])
    dataset[,phenotypes][is.na(dataset[,phenotypes])]<-null.pheno
    dataset[,phenotypes]<-as.factor(dataset[,phenotypes])
  }

  #split phenotypes per level into columns
  numeric.pheno<-sapply(levels(as.factor(dataset[,phenotypes])), function(x){as.numeric(dataset[,phenotypes]==x)})
  colnames(numeric.pheno)<-paste(phenotypes, levels(as.factor(dataset[,phenotypes])), sep = "-")
  dataset2<-cbind(dataset, numeric.pheno)
  return(dataset2)
}
