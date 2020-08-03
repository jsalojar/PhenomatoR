#' Sample gene expression array
#'
#' Gene expression data.
#'
#' @docType data
#'
#' @usage expMat
#'
#' @format A matrix with 6 rows and 40 columns:
#' \describe{
#'   \item{Flg22_1h}{treatment with flagellin for 1 hour}
#'   \item{Flg22_4h}{treatment with flagellin for 4 hours}
#'   \item{ABA_30min}{treatment with abscisic acid for 30 mins}
#'   \item{ABA_1h}{treatment with abscisic acid for 1 hour}
#'   \item{ABA_3h}{treatment with abscisic acid for 3 hours}
#' }
#'
#' @keywords datasets
#'
#' @source private
#'
#' @examples
#' data("expMat", package = "PhenomatoR")
#' pcaMat(matrix = t(expMat), ranks = 5)
"expMat"
