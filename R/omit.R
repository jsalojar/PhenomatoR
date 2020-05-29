#' @title Remove rows with data present in the corresponding row in indicated
#'   column
#'
#' @description Rows from the object in the first argument are removed if data
#'   are present in the corresponding rows in the column stated in the second
#'   argument, i.e. only rows with NA values in `omit.col` will be retained.
#'
#' @param dat data.frame object
#' @param omit.col name of column that indicates which rows to remove
#' @param indicator vector of value(s) in \code{omit.col} that indicate which row to remove
#'
#' @return object \code{dat} but with indicated rows removed
#'
#' @examples
#' sample.dat<-data.frame(class = c("Cntrl", "mutant-x"), length = rnorm(6, mean = 10), remove = c(NA, "yes", "no", 1, TRUE, FALSE))
#' omit(sample.dat, "remove", c(NA, "yes", 1))
#' omit(sample.dat, "remove")
#'
#' @export
omit<-function(dat, omit.col, indicator = NULL){

  #remove rows with specified indicator in the corresponding row of omit.col
  if (omit.col %in% names(dat)){
    if (is.null(indicator) == FALSE) {
      rows.to.remove<-match(indicator, dat[,omit.col])
      dat<-dat[-rows.to.remove,]
    }
    #remove rows with any data present in the corresponding row of omit.col
    else {
      dat[,omit.col]<-as.character(dat[,omit.col])
      dat<-subset(dat, subset = is.na(dat[,omit.col]) == TRUE)
    }
  }
  return(dat)
}
