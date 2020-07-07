#' Sample dataset with 2 independent variables
#'
#' A dataset with 1174 observations of stomatal phenotypes when plants were
#' subjected to 2 independent variables: genotype and chemical treatment.
#' Experimental runs are also recorded.
#'
#' @docType data
#'
#' @usage stomataData
#'
#' @format A data.frame with 1174 rows and 5 columns:
#' \describe{
#'   \item{Genotypes}{genotype class}
#'   \item{Chemical}{chemical treatment class}
#'   \item{Experiment}{experiment run number}
#'   \item{Density(n.stomata/mm2)}{phenotype (numeric)}
#'   \item{Total_Clusters/mm2}{phenotype (numeric)}
#' }
#'
#' @keywords datasets
#'
#' @source private
#'
#' @examples
#' bootstrapModelZ(dataset = stomatadata,
#'                 phenotype = "Density(n.stomata/mm2)",
#'                 covariant1 = "Genotypes", covariant1.control = "Col-0",
#'                 covariant2 = "Chemical", covariant2.control = "Control")
"stomataData"
