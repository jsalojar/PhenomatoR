#' @title Compile root ontologies
#'
#' @description Extract plant/root ontologies and compile into a single-row matrix
#'
#' @param plant plant object stored within main rsml list
#' @param route list of sequence of characters that specify the order of objects
#'   to access. See \code{\link{rsmlExtract}} for more details.
#' @param translate logic. Translate plant accession IDs into names or not?
#'
#' @return Single-row matrix
#'
#' @examples
#' data("sparse")
#' rootOntology(sparse$scene$plant)
#'
#' @export
rootOntology <- function(plant, route = list("root", ".attrs", "accession"), translate = TRUE) {
  #extract accession values from all roots
  ontology <- rsmlExtract(rsml = plant, route = route)
  ontology <- table(unlist(ontology))
  ontology <- matrix(ontology, nrow = 1, dimnames = list(NULL, names(ontology)))
  if (translate) {
    colnames(ontology) <- plantOntology[match(colnames(ontology), plantOntology[,1]), 2]
  }
  return(ontology)
}
