#' @title Extracts z and p values from glht-class objects
#'
#' @description
#'
#' \code{extract.glht.z} extracts z values from a list of class \code{glht}
#' objects from \code{\link[multcomp]{glht}} and summarizes the means and
#' standard deviations of z values and their corresponding p values in a
#' data.frame. If \code{\link[stats]{p.adjustment.methods}} are indicated in
#' \code p.adjust, p-adjusted values are extracted instead and converted to z
#' scores. The mean z scores are then computed and reconverted back to p values.
#'
#' @import multcomp parallel stats
#'
#' @param list.glht list of class \code{glht} objects
#' @param p.adjust character vector of desired
#'   \code{\link[stats]{p.adjust.methods}}
#' @param mc.cores number of cores to use. See \code{\link[parallel]{mclapply}}.
#'
#' @return
#'
#' A data.frame of z and p values, and if applicable, sd of z values, adjusted z
#' and p values.
#'
#' @example require (purrr)
#' PSIdata[, c("AREA_PX", "Plant.Info")] %>%
#' lme4::lmer(formula = `AREA_PX` ~ `Plant.Info`, na.action = na.omit) %>%
#' multcomp::glht(linfct = multcomp::mcp(Plant.Info = "Dunnett")) %>%
#' list() %>%
#' extract.glht.z(p.adjust = c("fdr", "hochberg"))
#'
#' @export
extract.glht.z <- function(list.glht, p.adjust = NULL, mc.cores = 1L) {

  if (is.list(list.glht) == FALSE) {stop("data needs to be a list")}

  #retrieve z scores without adjustments and compile into matrix - iterations in rows, levels of covariant in columns
  matrix.z <- do.call("rbind", parallel::mclapply(list.glht, function(x) {summary(x)[["test"]][["tstat"]]}, mc.cores = mc.cores))
  #mean and sd of z scores across iterations and corresponding p value of mean z score
  z <- apply(matrix.z, 2, mean, na.rm = TRUE)
  z.sd <- apply(matrix.z, 2, sd, na.rm = TRUE)
  p <- stats::pnorm(z)

  if (is.null(p.adjust) == FALSE) {
    adjust.list <- list()
    for (i in 1:length(p.adjust)) {
      #retrieve p values after corresponding p adjustment method specified in z.adjust argument
      padjust.list <- parallel::mclapply(list.glht, function(x) {
        summary(object = x, test = multcomp::adjusted(type = p.adjust[i]))[["test"]][["pvalues"]]
        },
        mc.cores = mc.cores)
      #compute z scores after p-value adjustments and compile into matrix - iterations in rows, levels of covariant in columns
      matrix.zadjust <- do.call("rbind", parallel::mclapply(padjust.list, qnorm, mc.cores = mc.cores))

      #mean and sd of z-adjusted scores across iterations and corresponding p value of mean z-adjusted score
      adjust.list[[paste0(p.adjust[i])]][["z"]] <- apply(matrix.zadjust, 2, mean, na.rm = TRUE)
      adjust.list[[paste0(p.adjust[i])]][["z.sd"]] <- apply(matrix.zadjust, 2, sd, na.rm = TRUE)
      adjust.list[[paste0(p.adjust[i])]][["p"]] <- stats::pnorm(adjust.list[[paste0(p.adjust[i])]][["z"]])
    }
  }

  #compile all z and sd and p values in a data.frame
  z.stats <- data.frame(test = names(z), z = z, z.sd = z.sd, p = p) #z and p
  if (is.null(p.adjust) == FALSE) {
    z.stats <- cbind(z.stats, do.call("cbind", unlist(adjust.list, recursive = FALSE)))
  }
  #remove sd since all NAs
  if (length(list.glht) == 1) {z.stats <- z.stats[,-grep(".sd", colnames(z.stats))]}
  rownames(z.stats) <- NULL
  return(z.stats)
}
