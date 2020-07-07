#' Pre-loaded output from \code{bootstrapModelZHeatmap}
#'
#' @description \code{archidartResults} is derived from passing
#'   \code{\link{archidartDf}} to \code{\link{bootstrapModelZHeatmap}}.
#'
#'   \code{PSIresults} is derived from passing \code{\link{PSIdata}} to
#'   \code{\link{bootstrapModelZHeatmap}}.
#'
#'   \code{stomataResults} is derived from passing \code{\link{stomataData}} to
#'   \code{\link{bootstrapModelZHeatmap}}.
#'
#' @docType data
#'
#' @format List with 3 elements:
#'
#'   1. Data.frame from \code{bootstrapModelZ}
#'
#'   2. Data.frame from \code{stretchDf}
#'
#'   3. Heatmap from \code{heatmapper}
#'
#' @references \code{vignette("PhenomatoR", package = "PhenomatoR")}
#'
#' @name results
NULL
#' @rdname results
"archidartResults"
#' @rdname results
"PSIresults"
#' @rdname results
"stomataResults"
