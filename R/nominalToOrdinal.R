#' @export
nominalToOrdinal <- function(dat, phenotypes, orderings, fill.na = NULL) {

  #fill NAs with value specified in fill.na argument
  if (is.null(fill.na) == FALSE) {
    for (i in 1:length(phenotypes)) {
      dat[,phenotypes[i]][is.na(dat[,phenotypes[i]])] <- as.character(fill.na)
    }
  }

  #convert phenotype data from factor to numeric
  ordinal.dat <- lapply(1:length(phenotypes), function(x) {
    as.numeric(factor(as.character(dat[,phenotypes[x]]), levels = orderings[[x]]))
  })
  ordinal.dat <- do.call("cbind", ordinal.dat)

  #cbind ordinal data behind original data
  colnames(ordinal.dat) <- paste0(phenotypes, "-ordinal")
  dat <- cbind(dat, ordinal.dat)
  return(dat)
}

#head(nominalToOrdinal(bloody.roots,
#                      phenotypes = c("BGH_phenotype", "GO_phenotype"),
#                      orderings = list(c("Normal", "brown coloring of infected leaves", "Chlorotic leaves", "few necrotic spots", "Necrotic spots"),
#                                       c("Normal", "brown coloring of infected leaves", "Chlorotic leaves", "Necrotic spots")),
#                      fill.na = "Normal"))
