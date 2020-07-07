#' @title Build a 2D dataset from rsml files
#'
#' @description Function imports all rsml files within a directory as lists and
#'   applies the specified functions in arguments \code{single.value.functions}
#'   and \code{multi.value.functions} to each imported rsml file.
#'
#'   \code{rsmlToDf} is a wrapper of \code{\link{rsmlImport}},
#'   \code{\link{metaToDf}} and \code{\link{buildRow}}.
#'
#' @import plyr
#'
#' @param directory file path to directory containing rsml files
#' @param single.value.functions character vector of names of functions to be
#'   called that will output a single value
#' @param single.value.function.labels character vector of column names for the
#'   output of the functions specified in \code{single.value.functions}
#' @param multi.value.functions character vector of names of functions to be
#'   called that will output a single-row matrix or data.frame
#'
#' @return A data.frame. Each row corresponds to an rsml file while columns
#'   correspond to the outputs of the specified functions.
#'
#'   \describe{
#'     \item{filename}{file name}
#'     \item{columns 2-tentative}{metadata}
#'     \item{columns tentative-last}{outputs of functions}
#'   }
#'
#' @examples
#' rsml.samples <- system.file("extdata", "rsml", package = "PhenomatoR")
#' rsmlToDf(rsml.samples)
#'
#' @export
rsmlToDf <- function(directory,
                     single.value.functions = c("rootSysLength", "nDescendants", "rootSysXSpan", "rootSysYSpan", "rootSysZSpan", "rootSysSurfArea", "rootSysVol"),
                     single.value.function.labels = c("total root length", "total no. of roots", "x range", "y range", "z range", "total surface area", "total volume"),
                     multi.value.functions = "rootOntology") {
  #import rsml files
  filepaths <- list.files(path = directory, pattern = "*.rsml", full.names = TRUE)
  rsml <- lapply(filepaths, rsmlImport)

  #create a data.frame compiling traits
  filename <- list.files(directory)
  metadata <- plyr::rbind.fill(lapply(rsml, metaToDf))
  df <- plyr::rbind.fill(lapply(rsml, function(x) {
    as.data.frame(buildRow(x$scene$plant, single.value.functions = single.value.functions,
                           single.value.function.labels = single.value.function.labels,
                           multi.value.functions = multi.value.functions))}))
  df <- cbind(filename, metadata, df)
  return(df)
}
