#' @title two.factor.interaction
#'
#' @description For two-way ANOVA models with interaction, model and contrast
#'   matrices for Dunnett comparisons amongst levels of one factor within the
#'   second factor are constructed. This function serves as an accessory to
#'   \code{\link[multcomp]{glht}}.
#'
#' @import multcomp stats
#'
#' @param dataset data.frame with two columns of factors
#' @param fac1 character string of name of column with first factor
#' @param fac2 character string of name of column with second factor
#'
#' @return List of 2 elements: model matrix and contrast matrix
#'
#' @examples
#' data("warpbreaks")
#' two.factor.interaction(dataset = warpbreaks, fac1 = "wool", fac2 = "tension")
#' two.factor.interaction(dataset = warpbreaks, fac1 = "tension", fac2 = "wool")
#'
#' @references Hothorn, T. (2020) Additional multcomp Example.
#' \url{https://cran.r-project.org/web/packages/multcomp/vignettes/multcomp-examples.pdf}
#'
#' @export
two.factor.interaction <- function(dataset, fac1, fac2) {

  fac1.fac2.df <- expand.grid(fac1 = levels(dataset[,fac1]),
                              fac2 = levels(dataset[,fac2]))
  mod.matrix <- stats::model.matrix(~ fac2 * fac1, data = fac1.fac2.df)
  Dunnett <- multcomp::contrMat(table(dataset[,fac1]), "Dunnett")
  empty.matrix <- matrix(0, nrow = nrow(Dunnett), ncol = ncol(Dunnett))

  n <- length(levels(dataset[,fac2]))
  contrast.matrix <- list()
  for (i in 1:n) {
    contrast.matrix[[i]] <- cbind(do.call("cbind", replicate(i-1, empty.matrix, simplify = FALSE)),
                                  Dunnett,
                                  do.call("cbind", replicate(n-i, empty.matrix, simplify = FALSE)))
    rownames(contrast.matrix[[i]]) <- paste(levels(dataset[,fac2])[i], rownames(contrast.matrix[[i]]), sep = ":")
  }
  contrast.matrix <- do.call("rbind", contrast.matrix)
  colnames(contrast.matrix) <- rep(colnames(Dunnett), times = n)

  out <- list()
  out[["mod.matrix"]] <- mod.matrix
  out[["contrast.matrix"]] <- contrast.matrix
  return(out)
}
