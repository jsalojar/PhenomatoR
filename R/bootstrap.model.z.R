#' @export
bootstrap.model.z<-function(dataset,
                            #phenotype observations
                            phenotype,
                            #independent variable and corresponding name of control
                            covariant1, covariant1.control,
                            #random effect (if present)
                            randomeffect = NULL, model.formula = NULL,
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
  trim.data <- dataset[,c(phenotype, covariant1, covariant2, randomeffect)] #trim data
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

  #options for bootstrapping
  list.iterations <- list()
  if (is.null(n) == TRUE && iterations == 1) { #no bootstrapping
    list.iterations[[iterations]] <- trim.data
  }
  if (iterations > 1) { #bootstrapping
    #pasting backticks to specified covariant(s) and randomeffect(s)
    variables.covariant1 <- paste("`", covariant1, "`", sep = "")
    if (is.null(randomeffect) == FALSE) {
      variables.randomeffect <- paste("`", randomeffect, "`", sep = "")
    }
    else {variables.randomeffect <- NULL}
    if (is.null(covariant2) == FALSE) {
      variables.covariant2 <- paste("`", covariant2, "`", sep = "")
    }
    else {variables.covariant2 <- NULL}
    variables <- c(variables.covariant1, variables.randomeffect, variables.covariant2)

    #assign size of dataset to n
    if (is.null(n) == TRUE) {n <- nrow(trim.data)}

    list.iterations <- parallel::mclapply(1:iterations, function(x){plyr::ddply(.data = trim.data,
                                                                                .variables = variables,
                                                                                .fun = function(y){y[sample(1:nrow(y),
                                                                                                     size = n,
                                                                                                     replace = TRUE),]},
                                                                                .parallel = TRUE)},
                                          mc.cores = mc.cores)
  }

  #linear model
  if (c(data.type == "continuous" | data.type == "ordinal")
      && is.null(randomeffect) == TRUE && is.null(model.formula) == TRUE
      && is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {
    list.lm<-parallel::mclapply(list.iterations, stats::lm,
                                formula = formula(paste(paste("`", phenotype, "`", sep = ""),
                                                  "~",
                                                  paste("`", covariant1, "`", sep = ""), sep = " ")),
                                na.action = na.omit, mc.cores = mc.cores)

    #general linear hypothesis testing
    K <- diag(length(coef(list.lm[[1]])))[-1,]
    if (is.vector(K) == TRUE) {K <- matrix(K, nrow = 1)}
    rownames(K) <- names(coef(list.lm[[1]]))[-1]

    list.glht<-parallel::mclapply(list.lm, multcomp::glht,
                                  linfct = K,
                                  mc.cores = mc.cores)
  }
  else if (c(data.type == "continuous" | data.type == "ordinal")
           && is.null(randomeffect) == TRUE && is.null(model.formula) == TRUE
           && is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {
    list.lm<-parallel::mclapply(list.iterations, stats::lm,
                                formula = formula(paste(paste("`", phenotype, "`", sep = ""),
                                                        "~",
                                                        paste("`", covariant1, "`", sep = ""),
                                                        "*",
                                                        paste("`", covariant2, "`", sep = ""))),
                                na.action = na.omit,
                                mc.cores = mc.cores)

    #compare covariant1 levels within each level of covariant2
    res.interaction1<-two.factor.interaction(dataset = trim.data, fac1 = covariant1, fac2 = covariant2)
    mod.matrix1<-res.interaction1$mod.matrix
    contrast.matrix1<-res.interaction1$contrast.matrix

    list.glht1<-parallel::mclapply(list.lm, multcomp::glht,
                                   linfct = contrast.matrix1 %*% mod.matrix1,
                                   mc.cores = mc.cores)

    #compare covariant2 levels within each level of covariant1
    res.interaction2<-two.factor.interaction(dataset = trim.data, fac1 = covariant2, fac2 = covariant1)
    mod.matrix2<-res.interaction2$mod.matrix
    contrast.matrix2<-res.interaction2$contrast.matrix

    list.glht2<-parallel::mclapply(list.lm, multcomp::glht,
                                   linfct = contrast.matrix2 %*% mod.matrix2,
                                   mc.cores = mc.cores)
  }

  #linear mixed effect model
  if (c(data.type == "continuous" | data.type == "ordinal")
      && is.null(randomeffect) == FALSE
      && is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {

    #elements from `randomeffect` are applied using ~1|element1/element2/... model
    if (is.null(model.formula) == TRUE) {
      random<-paste(paste0("(1|`", randomeffect, "`)"), collapse = " + ")
      fixed<-paste(paste("`", phenotype, "`", sep = ""),
                   "~",
                   paste("`", covariant1, "`", sep = ""))
      formula<-paste(fixed, "+", random)
    }
    #apply formula in `model.formula` directly
    else {
      formula<-model.formula
    }

    list.lmer<-parallel::mclapply(list.iterations, lme4::lmer,
                                 formula = formula(formula),
                                 na.action = na.omit,
                                 mc.cores = mc.cores)

    #general linear hypothesis testing
    K <- diag(nrow(coef(summary(list.lmer[[1]]))))[-1,]
    if (is.vector(K) == TRUE) {K <- matrix(K, nrow = 1)}
    rownames(K) <- rownames(coef(summary(list.lmer[[1]])))[-1]

    list.glht<-parallel::mclapply(list.lmer, multcomp::glht,
                                  linfct = K,
                                  mc.cores = mc.cores)
  }
  else if (c(data.type == "continuous" | data.type == "ordinal")
           && is.null(randomeffect) == FALSE
           && is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {

    #elements from `randomeffect` are applied using ~1|element1/element2/... model
    if (is.null(model.formula) == TRUE) {
      random<-paste(paste0("(1|`", randomeffect, "`)"), collapse = " + ")
      fixed<-paste(paste("`", phenotype, "`", sep = ""),
                   "~",
                   paste("`", covariant1, "`", sep = ""),
                   "*",
                   paste("`", covariant2, "`", sep = ""))
      formula<-paste(fixed, "+", random)
    }
    #apply formula in `model.formula` directly
    else {
      formula<-model.formula
    }

    list.lme<-parallel::mclapply(list.iterations, lme4::lmer,
                                formula = formula(formula),
                                na.action = na.omit,
                                mc.cores = mc.cores)

    #compare covariant1 levels within each level of covariant2
    res.interaction1<-two.factor.interaction(dataset = trim.data, fac1 = covariant1, fac2 = covariant2)
    mod.matrix1<-res.interaction1$mod.matrix
    contrast.matrix1<-res.interaction1$contrast.matrix

    list.glht1<-parallel::mclapply(list.lme, multcomp::glht,
                                   linfct = contrast.matrix1 %*% mod.matrix1,
                                   mc.cores = mc.cores)

    #compare covariant2 levels within each level of covariant1
    res.interaction2<-two.factor.interaction(dataset = trim.data, fac1 = covariant2, fac2 = covariant1)
    mod.matrix2<-res.interaction2$mod.matrix
    contrast.matrix2<-res.interaction2$contrast.matrix

    list.glht2<-parallel::mclapply(list.lme, multcomp::glht,
                                   linfct = contrast.matrix2 %*% mod.matrix2,
                                   mc.cores = mc.cores)
  }

  #generalized linear model
  if (data.type == "nominal"
      && is.null(randomeffect) == TRUE
      && is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {
    list.glm<-parallel::mclapply(list.iterations, stats::glm,
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

    list.glht<-parallel::mclapply(list.glm, multcomp::glht,
                                  linfct = K,
                                  mc.cores = mc.cores)
  }
  else if (data.type == "nominal"
           && is.null(randomeffect) == TRUE
           && is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {
    list.glm<-parallel::mclapply(list.iterations, stats::glm,
                                formula = formula(paste(paste("`", phenotype, "`", sep = ""),
                                                        "~",
                                                        paste("`", covariant1, "`", sep = ""),
                                                        "*",
                                                        paste("`", covariant2, "`", sep = ""))),
                                family = poisson,
                                na.action = na.omit,
                                mc.cores = mc.cores)

    #compare covariant1 levels within each level of covariant2
    res.interaction1<-two.factor.interaction(dataset = trim.data, fac1 = covariant1, fac2 = covariant2)
    mod.matrix1<-res.interaction1$mod.matrix
    contrast.matrix1<-res.interaction1$contrast.matrix

    list.glht1<-parallel::mclapply(list.glm, multcomp::glht,
                                   linfct = contrast.matrix1 %*% mod.matrix1,
                                   mc.cores = mc.cores)

    #compare covariant2 levels within each level of covariant1
    res.interaction2<-two.factor.interaction(dataset = trim.data, fac1 = covariant2, fac2 = covariant1)
    mod.matrix2<-res.interaction2$mod.matrix
    contrast.matrix2<-res.interaction2$contrast.matrix

    list.glht2<-parallel::mclapply(list.glm, multcomp::glht,
                                   linfct = contrast.matrix2 %*% mod.matrix2,
                                   mc.cores = mc.cores)
  }

  #generalized linear mixed effect model
  if (data.type == "nominal"
      && is.null(randomeffect) == FALSE
      && is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {

    #elements from `randomeffect` are applied using ~1|element1/element2/... model
    if (is.null(model.formula) == TRUE) {
      random<-paste(paste0("(1|`", randomeffect, "`)"), collapse = " + ")
      fixed<-paste(paste("`", phenotype, "`", sep = ""),
                   "~",
                   paste("`", covariant1, "`", sep = ""))
      formula<-paste(fixed, "+", random)
    }
    #apply formula in `model.formula` directly
    else {
      formula<-model.formula
    }

    list.glmer<-parallel::mclapply(list.iterations, lme4::glmer,
                                   formula = formula(formula),
                                   family = poisson,
                                   na.action = na.omit,
                                   mc.cores = mc.cores)

    #general linear hypothesis testing
    K <- diag(nrow(coef(summary(list.glmer[[1]]))))[-1,]
    if (is.vector(K) == TRUE) {K <- matrix(K, nrow = 1)}
    rownames(K) <- rownames(coef(summary(list.glmer[[1]])))[-1]

    list.glht<-parallel::mclapply(list.glmer, multcomp::glht,
                                  linfct = K,
                                  mc.cores = mc.cores)
  }
  else if (data.type == "nominal"
           && is.null(randomeffect) == FALSE
           && is.null(covariant2) == FALSE && is.null(covariant2.control) == FALSE) {

    #elements from `randomeffect` are applied using ~1|element1/element2/... model
    if (is.null(model.formula) == TRUE) {
      random<-paste(paste0("(1|`", randomeffect, "`)"), collapse = " + ")
      fixed<-paste(paste("`", phenotype, "`", sep = ""),
                   "~",
                   paste("`", covariant1, "`", sep = ""),
                   "*",
                   paste("`", covariant2, "`", sep = ""))
      formula<-paste(fixed, "+", random)
    }
    #apply formula in `model.formula` directly
    else {
      formula<-model.formula
    }

    list.glmer<-parallel::mclapply(list.iterations, lme4::glmer,
                                 formula = formula(formula),
                                 family = poisson,
                                 na.action = na.omit,
                                 mc.cores = mc.cores)

    #compare covariant1 levels within each level of covariant2
    res.interaction1<-two.factor.interaction(dataset = trim.data, fac1 = covariant1, fac2 = covariant2)
    mod.matrix1<-res.interaction1$mod.matrix
    contrast.matrix1<-res.interaction1$contrast.matrix

    list.glht1<-parallel::mclapply(list.glmer, multcomp::glht,
                                   linfct = contrast.matrix1 %*% mod.matrix1,
                                   mc.cores = mc.cores)

    #compare covariant2 levels within each level of covariant1
    res.interaction2<-two.factor.interaction(dataset = trim.data, fac1 = covariant2, fac2 = covariant1)
    mod.matrix2<-res.interaction2$mod.matrix
    contrast.matrix2<-res.interaction2$contrast.matrix

    list.glht2<-parallel::mclapply(list.glmer, multcomp::glht,
                                   linfct = contrast.matrix2 %*% mod.matrix2,
                                   mc.cores = mc.cores)
  }

  #extract z scores
  if (is.null(covariant2) == TRUE && is.null(covariant2.control) == TRUE) {
    z.stats<-extract.glht.z(list.glht = list.glht, p.adjust = p.adjust, mc.cores = mc.cores)
    z.stats[,covariant1]<-levels(trim.data[,covariant1])[-1]
    z.stats$phenotype<-phenotype
  }
  else {
    z.stats1<-extract.glht.z(list.glht = list.glht1, p.adjust = p.adjust, mc.cores = mc.cores)
    z.stats1[,covariant1]<-rep(levels(trim.data[,covariant1])[-1], times = length(levels(trim.data[,covariant2])))
    z.stats1$controls<-paste(covariant1.control, "of", covariant1)
    z.stats1[,covariant2]<-as.factor(rep(levels(trim.data[,covariant2]), each = length(levels(trim.data[,covariant1]))-1))
    z.stats1$phenotype<-phenotype
    z.stats1$id<-apply(z.stats1, 1, function(x){paste(phenotype, "-", covariant1, "under", x[covariant2], "against", x["controls"])})

    z.stats2<-extract.glht.z(list.glht = list.glht2, p.adjust = p.adjust, mc.cores = mc.cores)
    z.stats2[,covariant1]<-rep(levels(trim.data[,covariant1]), each = length(levels(trim.data[,covariant2]))-1)
    z.stats2$controls<-paste(covariant2.control, "of", covariant2)
    z.stats2[,covariant2]<-as.factor(rep(levels(trim.data[,covariant2])[-1], times = length(levels(trim.data[,covariant1]))))
    z.stats2$phenotype<-phenotype
    z.stats2$id<-apply(z.stats2, 1, function(x){paste(phenotype, "-", covariant1, "under", x[covariant2], "against", x["controls"])})

    z.stats<-rbind(z.stats1, z.stats2)
  }

  return(z.stats)
}
