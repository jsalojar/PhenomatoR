#' @title Convert dataset into z scores and generate a heatmap
#'
#' @description
#'
#' \code{bootstrap.model.z.heatmap} is a wrapper of
#' \code{\link{bootstrap.model.z}}.
#'
#' \code{bootstrap.model.z.heatmap} draws \code{n} number of samples randomly with
#' replacement from the specified \code{phenotype}(s) per genotype found in
#' column \code{genotype}. If random effect(s) are present, samples are drawn
#' for all possible permutations of the genotypes and random effect(s) found in
#' the columns specified in \code{genotype} and \code{randomeffect}
#' respectively. The sampling is repeated \code{iterations} number of times. The
#' samples are then fed into a linear model or linear mixed effect model should
#' random effect(s) be absent or present respectively. z scores are calculated
#' using general linear hypothesis testing and averaged across all iterations to
#' provide estimates of the effect sizes of each mutant genotype against the
#' \code{control} (wild-type). The averaged z scores are summarised in a
#' data.frame and visualized in a heatmap (if indicated by user).
#'
#' @import ggplot2 grDevices multcomp lme4 plyr reshape2 stats
#'
#' @param dataset data.frame with multiple columns specifying each phenotypic
#'   trait, one column specifying genotypes, and another column specifying the
#'   random effect (if present)
#' @param genotype column name in rawdata that contains the genotypes (wild-type
#'   and mutants) as a character string
#' @param control name of the control (wild-type) (character string)
#' @param phenotype column name indicating values of a phenotypic trait to be
#'   analysed (character string)
#' @param vector.of.phenotypes character vector of phenotypes to be analysed
#' @param randomeffect column name in rawdata containing levels of random effect
#'   (character string)
#' @param z.adjust
#' @param n number of samples to choose
#' @param iterations number of times to repeat sampling
#' @param parallel if TRUE, apply function in parallel
#' @param print.heatmap if TRUE, heatmap is included in function output
#' @param heatmap.pdf indicate file path if heatmap is to be saved in pdf format
#'
#' @return 1. data.frame of mean z scores across all iterations per genotype
#'   (column) per phenotype (row)
#'
#'   (2. if specified by user, a heatmap printed as output of function)
#'
#'   (3. if specified by user, a heatmap saved in pdf format)
#'
#' @examples
#' some.dataset<-data.frame(batches = rep(c("batch1", "batch2", "batch3", "batch4", "batch5"), each = 6),
#'  zones = rep(c("zone1", "zone2", "zone3", "zone4", "zone5", "zone6"), times = 5),
#'  classes = c("Cntrl", "mutant-x", "mutant-y"),
#'  length = rnorm(30, mean = 10),
#'  width = rnorm(30, mean = 2),
#'  height = rnorm(30, mean = 5))
#' sample.model.z.gg(some.dataset, "classes", "Cntrl", c("length", "width", "height"), c("batches, "zones"))
#' ##using dummydata
#' bootstrap.model.z.gg(dummydata, "Plant.Info", "Col-0", c("AREA_PX", "PERIMETER_PX", "COMPACTNESS", "ROUNDNESS"), "Tray.ID")
#'
#' ##using dat1:4
#' test.list<-list(paste0("dat", 1:4))
#' OR
#' test.list<-list(dat1, dat2, dat3, dat4)
#' combine.sample.model.z.gg(test.list, omit.col = "OMIT", "Plant.Info", "Col-0", c("AREA_PX", "AREA_MM", "PERIMETER_PX", "PERIMETER_MM", "COMPACTNESS", "ROUNDNESS", "ISOTROPY", "ECCENTRICITY"), "Tray.ID")
#' @export
bootstrap.model.z.heatmap<-function(dataset,
                                    #phenotype observations
                                    phenotypes,
                                    #independent variable and corresponding name of control
                                    covariant1, covariant1.control,
                                    #random effect (if present)
                                    randomeffect = NULL, model.formula = NULL,
                                    #second independent variable and corresponding name of control (if present)
                                    covariant2 = NULL, covariant2.control = NULL,
                                    #bootstrapping
                                    n = 15, iterations = 5,
                                    #fdr adjustment
                                    z.adjust = NULL,
                                    #data class of phenotype observation
                                    data.type = "continuous",
                                    #how many cores to use
                                    mc.cores = 1L,
                                    #heatmapping
                                    #which z value to plot in heatmap (if correction in z.adjust is indicated)
                                    plot.z = "z",
                                    #intervals at which z values are cut and their corresponding labels
                                    breaks = NULL,
                                    labels = c("z<-2.58, p<0.01", "z<-1.96, p<0.05", "z<-1.65, p<0.10", "-1.65<z<1.65, random", "z>1.65, p>0.10", "z>1.96, p>0.05", "z>2.58, p>0.01"),
                                    #colours in heatmap
                                    colour = colorRampPalette(c("blue", "white", "firebrick1"))(7),
                                    #heatmap output format
                                    print.heatmap = TRUE, heatmap.pdf = NULL,
                                    #clustering
                                    cluster.row = FALSE, cluster.col = FALSE){

  #apply bootstrap.model.z to each phenotype
  list.bootstrap.model.z<-lapply(phenotypes,
                          #function
                          bootstrap.model.z,
                          #arguments
                          dataset = dataset,
                          covariant1 = covariant1, covariant1.control = covariant1.control,
                          randomeffect = randomeffect, model.formula = model.formula,
                          covariant2 = covariant2, covariant2.control = covariant2.control,
                          n = n, iterations = iterations,
                          z.adjust = z.adjust,
                          data.type = data.type,
                          mc.cores = mc.cores)

  z.stats<-do.call("rbind", list.bootstrap.model.z)

  if (is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {
    wide.df<-plyr::rbind.fill(parallel::mclapply(list.bootstrap.model.z, long.to.wide.df, value = plot.z, row = "phenotype", col = covariant1, mc.cores = mc.cores))
  }
  if (is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {
    wide.df<-plyr::rbind.fill(parallel::mclapply(list.bootstrap.model.z, long.to.wide.df, value = plot.z, row = "id", col = covariant1, mc.cores = mc.cores))
  }

  #create heatmap
  heatmap<-cutz.heatmap(wide.df = wide.df, row.id = "phenotype", column.name = covariant1,
                        breaks = if (is.null(breaks) == TRUE) {c(range(wide.df[names(wide.df) != "phenotype"], na.rm = TRUE)[1], -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, range(wide.df[names(wide.df) != "phenotype"], na.rm = TRUE)[2])} else {breaks},
                        labels = labels, colour = colour,
                        print.heatmap = print.heatmap, heatmap.pdf = heatmap.pdf,
                        cluster.row = cluster.row, cluster.col = cluster.col)

  ##compile outputs
  out<-list()
  out[["z.stats"]]<-z.stats
  out[["plot.values"]]<-wide.df
  if (print.heatmap == TRUE) {
    out[["heatmap"]]<-heatmap
  }

  return(out)
}
