#' Pre-loaded dataset from importing RSML files by \code{rsmlToDf}
#'
#' Sample RSML files (accessible by running \code{system.file("extdata", "archiDART", package = "PhenomatoR")})
#' imported and arranged into a data.frame by \code{\link{rsmlToDf}}.
#'
#' @docType data
#'
#' @usage archidartDf
#'
#' @format Data.frame with 70 rows and 26 columns:
#' \describe{
#'   \item{filename (column 1)}{names of RSML files}
#'   \item{version - property-definitions.property-definition.unit (columns 2-18)}{metadata}
#'   \item{total root length - root (columns 19-26)}{phenotypes}  }
#'
#' @keywords datasets
#'
#' @source Lobet, Guillaume (2017): Simulated dicot root systems. figshare. Dataset. https://doi.org/10.6084/m9.figshare.5624878.v1
"archidartDf"
