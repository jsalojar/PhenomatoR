#' Sample dataset from Photon System Instruments (PSI)
#'
#' A dataset containing 500 observations of plant phenotypes derived from different genotypes.
#' Other attributes include date of recording and experimental batches.
#'
#' @docType data
#'
#' @usage PSIdata
#'
#' @format A data.frame with 500 rows and 7 columns:
#' \describe{
#'   \item{Measuring.Date}{date in the format dd/mm/yyyy when data in the corresponding row was recorded}
#'   \item{Tray.ID}{experimental batch identifier}
#'   \item{Plant.Info}{genotype class}
#'   \item{AREA_PX}{phenotypic data(numeric character)}
#'   \item{PERIMETER_PX}{phenotypic data (numeric character)}
#'   \item{COMPACTNESS}{phenotypic data (numeric character)}
#'   \item{ROUNDNESS}{phenotypic data (numeric character)}
#' }
#'
#' @keywords datasets
#'
#' @source Trimmed sample data from Photon Systems Information (PSI)
#'
#' @examples
#' bootstrapModelZ(dataset = PSIdata,
#'                 phenotype = "AREA_PX",
#'                 covariant1 = "Plant.Info", covariant1.control = "Col-0",
#'                 random.effects = "Tray.ID",
#'                 p.adjust = c("fdr", "hochberg"))
"PSIdata"
