#' Data.frame with genotype expression data
#'
#' Gene expression data. Melted from \code{exparray}.
#'
#' @docType data
#'
#' @usage expDf
#'
#' @format A data.frame with 200 rows and 4 columns:
#' \describe{
#'   \item{genotype}{genotype of plant}
#'   \item{treatment.time}{treatment type and duration of treatment}
#'   \item{value}{gene expression values}
#'   \item{treatment}{treatment type}
#' }
#'
#' @keywords datasets
#'
#' @source private
#'
#' @examples
#' data("expDf", package = "PhenomatoR")
#' pcaDf(df = expDf,
#'       value = "value",
#'       var = "genotype",
#'       samples = "treatment.time",
#'       category = "treatment", ranks = 5)
"expDf"
