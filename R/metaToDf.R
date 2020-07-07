#' @title Compile metadata into a data.frame
#'
#' @description Function unlists all elements in object \code{metadata} in the \code{rsml} list and compiles them into a single-row data.frame.
#'
#' @param rsml rsml list
#'
#' @return A single-row data.frame.
#'
#' @examples
#' data("sparse")
#' metaToDf(sparse)
#'
#' @export
metaToDf <- function(rsml) {
  metadata <- rsml[["metadata"]]
  metadata <- unlist(metadata)
  metadata <- as.data.frame(matrix(metadata, nrow = 1, dimnames = list(NULL, names(metadata))))
  return(metadata)
}
