#' @title Import an rsml file
#'
#' @description Function imports an rsml file as a list
#'
#' @import XML
#'
#' @param filepath character string of file path to the rsml file to be imported
#'
#' @examples
#' path <- system.file("extdata", "rsml", "arabidopsis-simple.rsml", package = "PhenomatoR")
#' simple <- rsmlImport(path)
#' View(simple)
#'
#' @export
rsmlImport <- function(filepath) {
  rsml <- XML::xmlToList(XML::xmlParse(filepath))
  return(rsml)
}
