#' @export
#(save<-extract.glht.z(list.glht))
extract.glht.z <- function(list.glht, p.adjust = c("fdr", "hochberg"), mc.cores = 1L) {

  if (is.list(list.glht) == FALSE) {stop("data needs to be a list")}

  #retrieve z scores without adjustments
  list.zscores <- parallel::mclapply(list.glht, function(x) {summary(x)[["test"]][["tstat"]]}, mc.cores = mc.cores)
  #compile into matrix; iterations in rows, levels of covariant in columns
  matrix.zscores <- do.call("rbind", list.zscores)
  #mean and sd of z scores across iterations and corresponding p value of mean z score
  mean.zscores <- apply(matrix.zscores, 2, mean, na.rm = TRUE)
  sd.zscores <- apply(matrix.zscores, 2, sd, na.rm = TRUE)
  p <- stats::pnorm(mean.zscores)

  if (is.null(p.adjust) == FALSE) {
    z.adjust.list <- list()
    for (i in 1:length(p.adjust)) {
      #retrieve p values after corresponding p adjustment method specified in z.adjust argument
      list.padjust <- parallel::mclapply(list.glht, function(x) {
        summary(object = x, test = multcomp::adjusted(type = p.adjust[i]))[["test"]][["pvalues"]]
        },
        mc.cores = mc.cores)
      #compute z scores after p-value adjustments
      list.zadjust <- parallel::mclapply(list.padjust, qnorm, mc.cores = mc.cores)
      #compile into matrix; iterations in rows, levels of covariant in columns
      matrix.zadjust <- do.call("rbind", list.zadjust)

      #mean and sd of z-adjusted scores across iterations and corresponding p value of mean z-adjusted score
      mean.zadjust <- apply(matrix.zadjust, 2, mean, na.rm = TRUE)
      sd.zadjust <- apply(matrix.zadjust, 2, sd, na.rm = TRUE)
      z.adjust.list[[paste0(p.adjust[i])]][["z"]] <- mean.zadjust
      z.adjust.list[[paste0(p.adjust[i])]][["sd"]] <- sd.zadjust
      z.adjust.list[[paste0(p.adjust[i])]][["p"]] <- stats::pnorm(mean.zadjust)
    }
  }

  #output stats in a data.frame
  z.stats <- data.frame(test = names(mean.zscores), p = p, z = mean.zscores) #z and p
  if (length(list.glht) > 1) {z.stats$sd <- sd.zscores} #sd
  if (is.null(p.adjust) == FALSE && length(list.glht) == 1) { #adjusted z and p
    #adjusted z
    z.adjust.df <- do.call("cbind", parallel::mclapply(z.adjust.list, `[[`, "z", mc.cores = mc.cores))
    colnames(z.adjust.df) <- paste0(colnames(z.adjust.df), ".z")
    #p of adjusted z
    p.adjust.df <- do.call("cbind", parallel::mclapply(z.adjust.list, `[[`, "p", mc.cores = mc.cores))
    colnames(p.adjust.df) <- paste0(colnames(p.adjust.df), ".p")
    z.stats <- cbind(z.stats, z.adjust.df, p.adjust.df)
  }
  if (is.null(p.adjust) == FALSE && length(list.glht) > 1) { #adjusted z and sd and p
    if (ncol(matrix.zadjust) > 1) {
      z.sd.p.adjust.df <- do.call("cbind", unlist(z.adjust.list, recursive = FALSE))
      z.stats <- cbind(z.stats, z.sd.p.adjust.df)
    }
    else {
      #adjusted z
      z.adjust.df <- do.call("cbind", parallel::mclapply(z.adjust.list, `[[`, "z", mc.cores = mc.cores))
      colnames(z.adjust.df) <- paste0(colnames(z.adjust.df), ".z")
      #sd of adjusted z
      sd.adjust.df <- do.call("cbind", parallel::mclapply(z.adjust.list, `[[`, "sd", mc.cores = mc.cores))
      colnames(sd.adjust.df) <- paste0(colnames(sd.adjust.df), ".sd")
      #p of adjusted z
      p.adjust.df <- do.call("cbind", parallel::mclapply(z.adjust.list, `[[`, "p", mc.cores = mc.cores))
      colnames(p.adjust.df) <- paste0(colnames(p.adjust.df), ".p")
      z.stats <- cbind(z.stats, z.adjust.df, sd.adjust.df, p.adjust.df)
      }
  }
  rownames(z.stats) <- NULL
  return(z.stats)
}
