#' Sample dataset with observations in ordinal and nominal data types
#'
#' A dataset containing 222 observations of leaf phenotypes in ordinal and
#' nominal formats from different genotypes. Technical replicate number is also
#' recorded.
#'
#' @format A data.frame with 222 rows and 5 columns:
#' \describe{
#'   \item{Genotype}{genotype class}
#'   \item{replicate}{technial replicate number}
#'   \item{BGH_phenotype}{phenotype after infection with parasitic fungus, Blumeria graminis hordei (nominal, character)}
#'   \item{GO.mildew}{mildew infection severity (ordinal, character)}
#'   \item{GO_phenotype}{phenotype after infection (nominal, character)}
#' }
#' @source private
"mildewScreen"
