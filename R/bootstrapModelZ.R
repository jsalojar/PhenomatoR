#' @title Bootstraps, applies a model and computes z scores
#'
#' @description
#'
#' Function selects and applies a model based on the data type
#' of the phenotype and then conducts post-hoc analysis to compute the z scores
#' which estimates the effect sizes of the experimental classes against the
#' control. Bootstrapping can also be performed.
#'
#' @import lme4 multcomp parallel plyr stats
#'
#' @param dataset data.frame with columns specifying at least a phenotypic
#'   trait, covariant(s), and random effect(s) (if present)
#' @param phenotype character string of column name of a phenotypic trait
#' @param covariant1 character string of name of column with an independent
#'   variable
#' @param covariant1.control character string of the control of covariant1
#' @param random.effects character vector of name(s) of column(s) of random
#'   effect(s)
#' @param model.formula if default formula entered into the model is incorrect,
#'   specify formula to be applied in a character string. See details.
#' @param covariant2 character string of name of second column with another
#'   independent variable aside from that specified in \code{covariant1}
#' @param covariant2.control character string of control of covariant2
#' @param n number of samples to choose with replacement.
#' @param iterations number of times to repeat sampling
#' @param p.adjust character vector of p-value adjustment method(s) to perform.
#'   Methods accepted follow \code{\link[stats]{p.adjust.methods}}.
#' @param data.type specify either "continuous", "ordinal", or "count"
#'   corresponding to the phenotypes specified in argument \code{phenotypes}.
#' @param mc.cores number of cores to use. See \code{\link[parallel]{mclapply}}.
#'
#' @details
#'
#' \code{bootstrapModelZ} wraps \code{\link[plyr]{ddply}} and
#' \code{\link[base]{sample}} for bootstrapping, \code{\link[stats]{lm}},
#' \code{\link[lme4]{lmer}}, \code{\link[stats]{glm}}, \code{\link[lme4]{glmer}}
#' for modelling, and \code{\link[multcomp]{glht}} for post-hoc analysis.
#' \code{\link[stats]{pnorm}} and \code{\link[stats]{qnorm}} are also wrapped
#' for p- and z-value calculations respectively.
#'
#' Bootstrap: By default where \code{n} = NULL and \code{iterations} = 1,
#' bootstrapping will not be performed. If \code{n} = NULL and \code{iterations}
#' > 1, resampling size will default to the size of the cleaned sample data (NAs
#' removed) with replacement and repeated \code{iterations} number of times. If
#' \code{n} > 0 and \code{iterations} > 1, \code{n} resamples are drawn with
#' replacement and repeated \code{iterations} number of times. Resamples of the
#' user-specified phenotype are drawn from every possible permutation of
#' covariant(s) and random effect(s).
#'
#' Model: Ordinal data types are assumed to have equal distances between ordinal
#' levels. Therefore, for both data types "continuous" and "ordinal", linear and
#' linear mixed effects models are used where random effects are absent and
#' present respectively. For count data, general linear and general linear mixed
#' effects model are used. The selected model will be applied for every
#' iteration. Random effects can be specified in \code{random.effects} and the
#' default formula is written as "response ~ terms + (1|random1) + (1|random2) +
#' ...". Users may overwrite with a different formula through
#' \code{model.formula}.
#'
#' Post-hoc analysis: \code{\link{glhtExtract}} is used to extract z and p
#' values from the objects returned from \code{\link[multcomp]{glht}} and neatly
#' organize the outputs in a single data.frame. Where
#' \code{\link[stats]{p.adjust.methods}} are indicated in argument
#' \code{p.adjust}, \code{glhtExtract} extracts p values from every iteration
#' and computes the z scores and their means and standard deviations across
#' iterations for every level of the covariant(s).
#'
#' @return A data.frame. \describe{
#'     \item{test}{names of coefficients of linear functions. See \code{\link[multcomp]{glht}} for more details.}
#'     \item{z}{z scores without adjustments}
#'     \item{z.sd (if iterations > 1)}{standard deviation of z scores across iterations}
#'     \item{p}{p values without adjustments}
#'     \item{\code{covariant1}}{levels of covariant1}
#'     \item{\code{covariant2} (if specified)}{levels of covariant2}
#'     \item{control (if \code{covariant2} was specified)}{name of control from which covariant}
#'     \item{phenotype}{phenotype specified in \code{phenotype}}
#'     \item{id (if \code{covariant2} was specified)}{values in columns "phenotype", "control" merged. \code{covariant1} under which level of \code{covariant2} is also indicated.}
#'   }
#'
#' Additional columns for z, z.sd and p may be included, such as when p adjustment methods are indicated in \code{p.adjust}.
#'
#' @examples
#' ##Using PSIdata
#' bootstrapModelZ(dataset = PSIdata, phenotype = "AREA_PX", covariant1 = "Plant.Info", covariant1.control = "Col-0", random.effects = "Tray.ID", p.adjust = c("fdr", "hochberg"))
#'
#' ##Using stomatadata (2 covariants)
#' bootstrapModelZ(dataset = stomatadata, phenotype = "Density(n.stomata/mm2)", covariant1 = "Genotypes", covariant1.control = "Col-0", covariant2 = "Chemical", covariant2.control = "Control")
#'
#' @export
bootstrapModelZ <- function(dataset,
                            #phenotype observations
                            phenotype,
                            #independent variable and corresponding name of control
                            covariant1, covariant1.control,
                            #random effect (if present)
                            random.effects = NULL, model.formula = NULL,
                            #second independent variable and corresponding name of control (if present)
                            covariant2 = NULL, covariant2.control = NULL,
                            #bootstrapping
                            n = NULL, iterations = 1,
                            #fdr adjustment
                            p.adjust = NULL,
                            #data class of phenotype observation
                            data.type = "continuous",
                            #how many cores to use
                            mc.cores = 1L) {
  #organise data
  trim.data <- dataset[,c(phenotype, covariant1, covariant2, random.effects)] #trim data
  trim.data[,phenotype]<-as.numeric(trim.data[,phenotype]) #ensure phenotype is numeric
  trim.data<-trim.data[stats::complete.cases(trim.data[,phenotype]),] #remove rows with NAs
  trim.data[,covariant1]<-stats::relevel(as.factor(as.character(trim.data[,covariant1])), covariant1.control) #set control as first factor level
  if (is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {
    #check for and remove the levels of covariant1 without data for all levels of covariant2
    level2.per.level1<-sapply(split(trim.data, trim.data[,covariant1]), function(x){length(table(as.character(x[,covariant2])))})
    level2.per.level1<-data.frame(level1 = names(level2.per.level1), count = level2.per.level1)
    trim.data[,covariant2]<-as.factor(as.character(trim.data[,covariant2]))
    level1.to.remove<-unlist(apply(level2.per.level1, 1, function(x){if (!x["count"] == length(levels(trim.data[,covariant2]))){x["level1"]}}))
    trim.data<-trim.data[!trim.data[,covariant1] %in% level1.to.remove,]
    if (length(level1.to.remove) > 0) {
      warning(paste(level1.to.remove, sep = ", "), " removed due to incomplete data for every level of ", covariant2)
    }

    trim.data[,covariant1]<-stats::relevel(as.factor(as.character(trim.data[,covariant1])), covariant1.control)
    trim.data[,covariant2]<-stats::relevel(as.factor(as.character(trim.data[,covariant2])), covariant2.control)
  }

  #no bootstrapping
  list.iterations <- list()
  if (is.null(n) == TRUE && iterations == 1) {
    list.iterations[[iterations]] <- trim.data
  }
  #bootstrapping
  else {
    #pasting backticks to specified covariant(s) and random effect(s)
    variables.covariant1 <- paste("`", covariant1, "`", sep = "")
    if (is.null(random.effects) == FALSE) {
      variables.random.effects <- paste("`", random.effects, "`", sep = "")
    }
    else {variables.random.effects <- NULL}
    if (is.null(covariant2) == FALSE) {
      variables.covariant2 <- paste("`", covariant2, "`", sep = "")
    }
    else {variables.covariant2 <- NULL}
    variables <- c(variables.covariant1, variables.random.effects, variables.covariant2)

    #assign size of dataset to n
    if (is.null(n) == TRUE) {
      list.iterations <- parallel::mclapply(1:iterations, function(x){plyr::ddply(.data = trim.data,
                                                                                  .variables = variables,
                                                                                  .fun = function(y){y[sample(1:nrow(y),
                                                                                                              size = nrow(y),
                                                                                                              replace = TRUE),]},
                                                                                  .parallel = TRUE)},
                                            mc.cores = mc.cores)
    }
    else {
      list.iterations <- parallel::mclapply(1:iterations, function(x){plyr::ddply(.data = trim.data,
                                                                                  .variables = variables,
                                                                                  .fun = function(y){y[sample(1:nrow(y),
                                                                                                              size = n,
                                                                                                              replace = TRUE),]},
                                                                                  .parallel = TRUE)},
                                            mc.cores = mc.cores)
    }
  }

  #linear model
  if (c(data.type == "continuous" | data.type == "ordinal")
      && is.null(random.effects) == TRUE && is.null(model.formula) == TRUE
      && is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {
    list.lm <- parallel::mclapply(list.iterations, stats::lm,
                                  formula = formula(paste(paste("`", phenotype, "`", sep = ""),
                                                    "~",
                                                    paste("`", covariant1, "`", sep = ""), sep = " ")),
                                  na.action = na.omit, mc.cores = mc.cores)

    #general linear hypothesis testing
    K <- diag(length(coef(list.lm[[1]])))[-1,]
    if (is.vector(K) == TRUE) {K <- matrix(K, nrow = 1)}
    rownames(K) <- names(coef(list.lm[[1]]))[-1]

    list.glht <- parallel::mclapply(list.lm, multcomp::glht,
                                    linfct = K,
                                    mc.cores = mc.cores)
  }
  else if (c(data.type == "continuous" | data.type == "ordinal")
           && is.null(random.effects) == TRUE && is.null(model.formula) == TRUE
           && is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {
    list.lm <- parallel::mclapply(list.iterations, stats::lm,
                                  formula = formula(paste(paste("`", phenotype, "`", sep = ""),
                                                          "~",
                                                          paste("`", covariant1, "`", sep = ""),
                                                          "*",
                                                          paste("`", covariant2, "`", sep = ""))),
                                  na.action = na.omit,
                                  mc.cores = mc.cores)

    #compare covariant1 levels within each level of covariant2
    res.interaction1 <- twoFactorInteraction(dataset = trim.data, fac1 = covariant1, fac2 = covariant2)
    mod.matrix1 <- res.interaction1$mod.matrix
    contrast.matrix1 <- res.interaction1$contrast.matrix

    list.glht1 <- parallel::mclapply(list.lm, multcomp::glht,
                                   linfct = contrast.matrix1 %*% mod.matrix1,
                                   mc.cores = mc.cores)

    #compare covariant2 levels within each level of covariant1
    res.interaction2 <- twoFactorInteraction(dataset = trim.data, fac1 = covariant2, fac2 = covariant1)
    mod.matrix2 <- res.interaction2$mod.matrix
    contrast.matrix2 <- res.interaction2$contrast.matrix

    list.glht2 <- parallel::mclapply(list.lm, multcomp::glht,
                                     linfct = contrast.matrix2 %*% mod.matrix2,
                                     mc.cores = mc.cores)
  }

  #linear mixed effect model
  if (c(data.type == "continuous" | data.type == "ordinal")
      && is.null(random.effects) == FALSE
      && is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {

    #elements from `random.effects` are applied using ~1|element1/element2/... model
    if (is.null(model.formula) == TRUE) {
      random <- paste(paste0("(1|`", random.effects, "`)"), collapse = " + ")
      fixed <- paste(paste("`", phenotype, "`", sep = ""),
                     "~",
                     paste("`", covariant1, "`", sep = ""))
      formula <- paste(fixed, "+", random)
    }
    #apply formula in `model.formula` directly
    else {
      formula <- model.formula
    }

    list.lmer <- parallel::mclapply(list.iterations, lme4::lmer,
                                   formula = formula(formula),
                                   na.action = na.omit,
                                   mc.cores = mc.cores)

    #general linear hypothesis testing
    K <- diag(nrow(coef(summary(list.lmer[[1]]))))[-1,]
    if (is.vector(K) == TRUE) {K <- matrix(K, nrow = 1)}
    rownames(K) <- rownames(coef(summary(list.lmer[[1]])))[-1]

    list.glht <- parallel::mclapply(list.lmer, multcomp::glht,
                                    linfct = K,
                                    mc.cores = mc.cores)
  }
  else if (c(data.type == "continuous" | data.type == "ordinal")
           && is.null(random.effects) == FALSE
           && is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {

    #elements from `random.effects` are applied using ~1|element1/element2/... model
    if (is.null(model.formula) == TRUE) {
      random <- paste(paste0("(1|`", random.effects, "`)"), collapse = " + ")
      fixed <- paste(paste("`", phenotype, "`", sep = ""),
                     "~",
                     paste("`", covariant1, "`", sep = ""),
                     "*",
                     paste("`", covariant2, "`", sep = ""))
      formula <- paste(fixed, "+", random)
    }
    #apply formula in `model.formula` directly
    else {
      formula <- model.formula
    }

    list.lme <- parallel::mclapply(list.iterations, lme4::lmer,
                                  formula = formula(formula),
                                  na.action = na.omit,
                                  mc.cores = mc.cores)

    #compare covariant1 levels within each level of covariant2
    res.interaction1 <- twoFactorInteraction(dataset = trim.data, fac1 = covariant1, fac2 = covariant2)
    mod.matrix1 <- res.interaction1$mod.matrix
    contrast.matrix1 <- res.interaction1$contrast.matrix

    list.glht1 <- parallel::mclapply(list.lme, multcomp::glht,
                                     linfct = contrast.matrix1 %*% mod.matrix1,
                                     mc.cores = mc.cores)

    #compare covariant2 levels within each level of covariant1
    res.interaction2 <- twoFactorInteraction(dataset = trim.data, fac1 = covariant2, fac2 = covariant1)
    mod.matrix2 <- res.interaction2$mod.matrix
    contrast.matrix2 <- res.interaction2$contrast.matrix

    list.glht2 <- parallel::mclapply(list.lme, multcomp::glht,
                                     linfct = contrast.matrix2 %*% mod.matrix2,
                                     mc.cores = mc.cores)
  }

  #generalized linear model
  if (data.type == "count"
      && is.null(random.effects) == TRUE
      && is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {
    list.glm <- parallel::mclapply(list.iterations, stats::glm,
                                   formula = formula(paste(paste("`", phenotype, "`", sep = ""),
                                                           "~",
                                                           paste("`", covariant1, "`", sep = ""))),
                                   family = poisson,
                                   na.action = na.omit,
                                   mc.cores = mc.cores)

    #general linear hypothesis testing
    K <- diag(length(coef(list.glm[[1]])))[-1,]
    if (is.vector(K) == TRUE) {K <- matrix(K, nrow = 1)}
    rownames(K) <- names(coef(list.glm[[1]]))[-1]

    list.glht <- parallel::mclapply(list.glm, multcomp::glht,
                                    linfct = K,
                                    mc.cores = mc.cores)
  }
  else if (data.type == "count"
           && is.null(random.effects) == TRUE
           && is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {
    list.glm <- parallel::mclapply(list.iterations, stats::glm,
                                  formula = formula(paste(paste("`", phenotype, "`", sep = ""),
                                                          "~",
                                                          paste("`", covariant1, "`", sep = ""),
                                                          "*",
                                                          paste("`", covariant2, "`", sep = ""))),
                                  family = poisson,
                                  na.action = na.omit,
                                  mc.cores = mc.cores)

    #compare covariant1 levels within each level of covariant2
    res.interaction1 <- twoFactorInteraction(dataset = trim.data, fac1 = covariant1, fac2 = covariant2)
    mod.matrix1 <- res.interaction1$mod.matrix
    contrast.matrix1 <- res.interaction1$contrast.matrix

    list.glht1 <- parallel::mclapply(list.glm, multcomp::glht,
                                     linfct = contrast.matrix1 %*% mod.matrix1,
                                     mc.cores = mc.cores)

    #compare covariant2 levels within each level of covariant1
    res.interaction2 <- twoFactorInteraction(dataset = trim.data, fac1 = covariant2, fac2 = covariant1)
    mod.matrix2 <- res.interaction2$mod.matrix
    contrast.matrix2 <- res.interaction2$contrast.matrix

    list.glht2 <- parallel::mclapply(list.glm, multcomp::glht,
                                     linfct = contrast.matrix2 %*% mod.matrix2,
                                     mc.cores = mc.cores)
  }

  #generalized linear mixed effect model
  if (data.type == "count"
      && is.null(random.effects) == FALSE
      && is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {

    #elements from `random.effects` are applied using ~1|element1/element2/... model
    if (is.null(model.formula) == TRUE) {
      random <- paste(paste0("(1|`", random.effects, "`)"), collapse = " + ")
      fixed <- paste(paste("`", phenotype, "`", sep = ""),
                     "~",
                     paste("`", covariant1, "`", sep = ""))
      formula <- paste(fixed, "+", random)
    }
    #apply formula in `model.formula` directly
    else {
      formula <- model.formula
    }

    list.glmer <- parallel::mclapply(list.iterations, lme4::glmer,
                                     formula = formula(formula),
                                     family = poisson,
                                     na.action = na.omit,
                                     mc.cores = mc.cores)

    #general linear hypothesis testing
    K <- diag(nrow(coef(summary(list.glmer[[1]]))))[-1,]
    if (is.vector(K) == TRUE) {K <- matrix(K, nrow = 1)}
    rownames(K) <- rownames(coef(summary(list.glmer[[1]])))[-1]

    list.glht <- parallel::mclapply(list.glmer, multcomp::glht,
                                    linfct = K,
                                    mc.cores = mc.cores)
  }
  else if (data.type == "count"
           && is.null(random.effects) == FALSE
           && is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {

    #elements from `random.effects` are applied using ~1|element1/element2/... model
    if (is.null(model.formula) == TRUE) {
      random <- paste(paste0("(1|`", random.effects, "`)"), collapse = " + ")
      fixed <- paste(paste("`", phenotype, "`", sep = ""),
                     "~",
                     paste("`", covariant1, "`", sep = ""),
                     "*",
                     paste("`", covariant2, "`", sep = ""))
      formula <- paste(fixed, "+", random)
    }
    #apply formula in `model.formula` directly
    else {
      formula <- model.formula
    }

    list.glmer <- parallel::mclapply(list.iterations, lme4::glmer,
                                     formula = formula(formula),
                                     family = poisson,
                                     na.action = na.omit,
                                     mc.cores = mc.cores)

    #compare covariant1 levels within each level of covariant2
    res.interaction1 <- twoFactorInteraction(dataset = trim.data, fac1 = covariant1, fac2 = covariant2)
    mod.matrix1 <- res.interaction1$mod.matrix
    contrast.matrix1 <- res.interaction1$contrast.matrix

    list.glht1 <- parallel::mclapply(list.glmer, multcomp::glht,
                                     linfct = contrast.matrix1 %*% mod.matrix1,
                                     mc.cores = mc.cores)

    #compare covariant2 levels within each level of covariant1
    res.interaction2 <- twoFactorInteraction(dataset = trim.data, fac1 = covariant2, fac2 = covariant1)
    mod.matrix2 <- res.interaction2$mod.matrix
    contrast.matrix2 <- res.interaction2$contrast.matrix

    list.glht2 <- parallel::mclapply(list.glmer, multcomp::glht,
                                     linfct = contrast.matrix2 %*% mod.matrix2,
                                     mc.cores = mc.cores)
  }

  #extract z scores and p values and compile into a data.frame
  #1 covariant
  if (is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {
    z.stats <- glhtExtract(list.glht = list.glht, p.adjust = p.adjust, mc.cores = mc.cores)
    z.stats[,covariant1] <- levels(trim.data[,covariant1])[-1]
    z.stats$phenotype <- phenotype
  }
  #2 covariants
  else {
    z.stats1 <- glhtExtract(list.glht = list.glht1, p.adjust = p.adjust, mc.cores = mc.cores)
    z.stats1[,covariant1] <- rep(levels(trim.data[,covariant1])[-1], times = length(levels(trim.data[,covariant2])))
    z.stats1[,covariant2] <- as.factor(rep(levels(trim.data[,covariant2]), each = length(levels(trim.data[,covariant1]))-1))
    z.stats1$control <- paste(covariant1.control, "of", covariant1)
    z.stats1$phenotype <- phenotype
    z.stats1$id <- apply(z.stats1, 1, function(x){paste(phenotype, "-", covariant1, "under", x[covariant2], "against", x["controls"])})

    z.stats2 <- glhtExtract(list.glht = list.glht2, p.adjust = p.adjust, mc.cores = mc.cores)
    z.stats2[,covariant1] <- rep(levels(trim.data[,covariant1]), each = length(levels(trim.data[,covariant2]))-1)
    z.stats2[,covariant2] <- as.factor(rep(levels(trim.data[,covariant2])[-1], times = length(levels(trim.data[,covariant1]))))
    z.stats2$control <- paste(covariant2.control, "of", covariant2)
    z.stats2$phenotype <- phenotype
    z.stats2$id <- apply(z.stats2, 1, function(x){paste(phenotype, "-", covariant1, "under", x[covariant2], "against", x["controls"])})

    z.stats <- rbind(z.stats1, z.stats2)
  }

  return(z.stats)
}
