#' @title Remove rows based on data in a specified column
#'
#' @description Certain rows from the data.frame specified in the first argument
#'   are removed if data are present in those rows in the column specified in
#'   the second argument, i.e. only rows with NA values in \code{omit.col} will
#'   be retained. User may also remove rows only with specific values indicated
#'   in  \code{indicator}.
#'
#' @param dat data.frame
#' @param omit.col name of column that indicates which rows to remove
#' @param indicator value(s) that indicate which row to remove
#'
#' @return Returns \code{dat} but with indicated rows removed
#'
#' @examples
#' sample.dat <- data.frame(class = c("Cntrl", "mutant-x"), length = rnorm(6, mean = 10), remove = c(NA, "yes", "no", 1, TRUE, FALSE))
#'
#' omit(dat = sample.dat, omit.col = "remove")
#' omit(dat = sample.dat, omit.col = "remove", indicator = c(NA, "yes", 1))
#'
#' @export
omit <- function(dat, omit.col, indicator = NULL){

  #remove rows with specified indicator in the corresponding row of omit.col
  if (omit.col %in% names(dat)){
    if (is.null(indicator) == FALSE) {
      rows.to.remove <- match(indicator, dat[,omit.col])
      dat <- dat[-rows.to.remove,]
    }
    #remove rows with any data present in the corresponding row of omit.col
    else {
      dat[,omit.col] <- as.character(dat[,omit.col])
      dat <- subset(dat, subset = is.na(dat[,omit.col]) == TRUE)
    }
  }
  return(dat)
}
