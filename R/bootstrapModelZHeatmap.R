#' @title Bootstraps, applies a model, computes z scores, and then visualizes
#'   the z scores in a heatmap
#'
#' @description
#'
#' Function takes a dataset, bootstraps specified variables (optional), applies
#' a model based on the data type, performs post-hoc analysis to compute z
#' scores to estimate the statistical distance of the experimental classes
#' against the control, and then plots these z scores in a heatmap.
#'
#' \code{bootstrapModelZHeatmap} is a wrapper of
#' \code{\link{bootstrapModelZ}}, \code{\link{stretchDf}} and
#' \code{\link{heatmapper}}.
#'
#' @import ggplot2 gplots graphics grDevices multcomp lme4 parallel plyr
#'   reshape2 stats
#'
#' @param dataset data.frame with columns specifying phenotypic trait(s),
#'   covariant(s), and random effect(s) (if present)
#' @param phenotypes character vector of column names of phenotypic traits
#' @param covariant1 character string of name of column with an independent
#'   variable
#' @param covariant1.control character string of the control of covariant1
#' @param random.effects character vector of name(s) of column(s) of random
#'   effect(s)
#' @param model.formula if default formula entered into the model is incorrect,
#'   specify formula to be applied in a character string. See details of
#'   \code{\link{bootstrap.model.z}}.
#' @param covariant2 character string of name of second column with another
#'   independent variable aside from that specified in \code{covariant1}
#' @param covariant2.control character string of control of covariant2
#' @param n number of samples to choose with replacement. To sample at size
#'   equivalent to size of data, leave value at NULL and indicate no. of
#'   iterations (must be more than 1) in the next argument, \code{iterations}.
#' @param iterations number of times to repeat sampling
#' @param p.adjust character vector of p-value adjustment method(s) to perform.
#'   Methods accepted follow \code{\link[stats]{p.adjust.methods}}.
#' @param data.types specify "continuous", "ordinal", or "count" in a character
#'   vector in an order corresponding to the phenotypes specified in argument
#'   \code{phenotypes}. For eg. if first element in \code{phenotypes} is of
#'   continuous data type, specify "continuous" in first element of
#'   \code{data.types}. If only one element is indicated in \code{data.types}
#'   while multiple elements are specified in \code{phenotypes}, all phenotypes
#'   will be assumed to have the data type specified.
#' @param mc.cores number of cores to use. See \code{\link[parallel]{mclapply}}.
#' @param plot.z which z values to be plotted in heatmap. For the p-value
#'   adjustment method(s) specified in \code{p.adjust}, indicate desired z
#'   values to be plotted by pasting method in front of ".z". For eg. "holm.z".
#'   To plot z values with no adjustments, leave at default value of "z".
#' @param breaks numeric vector with elements indicating intervals at which z
#'   values are to be categorized and plotted in the heatmap. By default, z
#'   value intervals are: z<-2.58, -2.58<z<-1.96, -1.96<z<-1.65, -1.65<z<1.65,
#'   1.65<z<1.96, 1.96<z<2.58, z<2.58.
#'
#'   The format to specify customised breaks depends on whether clustering is
#'   performed. For heatmap clustering and no clustering, \code{breaks} are
#'   passed to \code{\link[gplots]{heatmap.2}} and \code{\link[base]{cut}}
#'   respectively.
#' @param labels labels for the categories after breaking
#' @param colour colours for each category after breaking
#' @param print.heatmap if TRUE, heatmap is returned by function
#' @param heatmap.pdf indicate file path and name if heatmap is to be saved in
#'   pdf format
#' @param pdf.size numeric vector of 2 elements which denote the width and
#'   height of the pdf file respectively
#' @param cluster.row logical indicating whether to cluster heatmap rows
#' @param cluster.col logical indicating whether to cluster heatmap columns
#'
#'
#' @return A list with the following elements:
#'
#'   1. Outputs of \code{\link{bootstrapModelZ}} row-binded into a single
#'   data.frame.
#'
#'   2. Output of \code{\link{stretchDf}}; a data.frame.
#'
#'   3. Output of \code{\link{heatmapper}}; \itemize{
#'   \item{if no clustering, an invisible "gg" class object representing the heatmap}
#'   \item{if clustering is performed, a list derived from \code{\link[gplots]{heatmap.2}}, representing the heatmap}
#'   }
#'
#'   If specified by user in \code{heatmap.pdf}, the heatmap may be saved in pdf
#'   format.
#'
#' @examples
#' ##using PSIdata
#' (res <- bootstrapModelZHeatmap(dataset = PSIdata,
#'                                phenotypes = c("AREA_PX", "PERIMETER_PX", "COMPACTNESS", "ROUNDNESS"),
#'                                covariant1 = "Plant.Info", covariant1.control = "Col-0",
#'                                random.effects = "Tray.ID"))
#'
#' @export
bootstrapModelZHeatmap <- function(dataset,
                                   #observations of phenotypes
                                   phenotypes,
                                   #independent variable and corresponding name of control
                                   covariant1, covariant1.control,
                                   #random effects (if present)
                                   random.effects = NULL, model.formula = NULL,
                                   #second independent variable and corresponding name of control (if present)
                                   covariant2 = NULL, covariant2.control = NULL,
                                   #bootstrapping
                                   n = NULL, iterations = 1,
                                   #fdr adjustment
                                   p.adjust = NULL,
                                   #data class of phenotype observation
                                   data.types = "continuous",
                                   #how many cores to use
                                   mc.cores = 1L,
                                   ##heatmapping
                                   #which z value to plot in heatmap (if correction in z.adjust is indicated)
                                   plot.z = "z",
                                   #intervals at which z values are cut and their corresponding labels
                                   breaks = NULL,
                                   labels = c("z<-2.58, p<0.01", "z<-1.96, p<0.05", "z<-1.65, p<0.10", "-1.65<z<1.65, random", "z>1.65, p>0.10", "z>1.96, p>0.05", "z>2.58, p>0.01"),
                                   #colours in heatmap
                                   colour = colorRampPalette(c("blue", "white", "firebrick1"))(7),
                                   #heatmap output format
                                   print.heatmap = TRUE, heatmap.pdf = NULL, pdf.size = c(7, 7),
                                   #clustering
                                   cluster.row = FALSE, cluster.col = FALSE) {
  if (length(data.types) > 1 && !length(data.types) == length(phenotypes)) {
    stop(length(phenotypes), " elements in phenotypes but ", length(data.types), " in data.types")
  }
  if (length(data.types) == 1 && !length(data.types) == length(phenotypes)) {
    warning("all phenotypes are assumed to have ", data.types, " data type")
    data.types <- rep(data.types, times = length(phenotypes))
  }

  #apply bootstrapModelZ to each phenotype
  list.bootstrapModelZ <- parallel::mclapply(1:length(phenotypes), function(x) {
    tryCatch(bootstrapModelZ(dataset = dataset,
                             phenotype = phenotypes[x],
                             covariant1 = covariant1, covariant1.control = covariant1.control,
                             random.effects = random.effects, model.formula = model.formula,
                             covariant2 = covariant2, covariant2.control = covariant2.control,
                             n = n, iterations = iterations,
                             p.adjust = p.adjust,
                             data.type = data.types[x],
                             mc.cores = mc.cores), error = function (e) {NA})
  }, mc.cores = mc.cores)

  #identify which phenotypes produce errors, if any
  if (sum(is.na(list.bootstrapModelZ)) == length(list.bootstrapModelZ)) {
    stop("bootstrapModelZ throws only errors")
  }
  if (TRUE %in% is.na(list.bootstrapModelZ)) {
    error.present <- TRUE
    error.index <- which(is.na(list.bootstrapModelZ))
    error.phenotypes <- phenotypes[error.index]
  }
  else {error.present <- FALSE}

  #compile all non-error outputs into a single data.frame
  list.bootstrapModelZ <- list.bootstrapModelZ[!is.na(list.bootstrapModelZ)]
  z.stats <- do.call("rbind", list.bootstrapModelZ)

  if (is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {
    wide.df <- plyr::rbind.fill(parallel::mclapply(list.bootstrapModelZ, stretchDf, value = plot.z, row = "phenotype", col = covariant1, mc.cores = mc.cores))
  }
  if (is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {
    wide.df <- plyr::rbind.fill(parallel::mclapply(list.bootstrapModelZ, stretchDf, value = plot.z, row = "id", col = covariant1, mc.cores = mc.cores))
  }

  #if errors were present
  if (error.present == TRUE) {
    error.df <- data.frame(phenotype = error.phenotypes)
    wide.df <- plyr::rbind.fill(wide.df, error.df)
  }

  #create heatmap
  heatmap <- tryCatch(heatmapper(wide.df = wide.df, row.id = "phenotype", column.name = covariant1,
                                 breaks = breaks, labels = labels, colour = colour,
                                 print.heatmap = print.heatmap, heatmap.pdf = heatmap.pdf, pdf.size = pdf.size,
                                 cluster.row = cluster.row, cluster.col = cluster.col),
                      error = function(e) {NA})

  ##compile outputs
  out <- list()
  out[["z.stats"]] <- z.stats
  out[["plot.values"]] <- wide.df
  if (print.heatmap == TRUE) {
    out[["heatmap"]] <- heatmap
  }

  return(out)
}
