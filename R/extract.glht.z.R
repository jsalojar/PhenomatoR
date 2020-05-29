#' @export
extract.glht.z<-function(list.glht, z.adjust = c("fdr", "hochberg"), mc.cores = 1L) {

  if (is.list(list.glht) == FALSE) {stop("data needs to be a list")}

  #compute z scores with summary()
  list.glht.summary<-parallel::mclapply(list.glht, summary, mc.cores = mc.cores)
  list.zscores<-parallel::mclapply(list.glht.summary, function(x){x[["test"]][["tstat"]]}, mc.cores = mc.cores)

  #compile
  matrix.zscores<-do.call("rbind", list.zscores)

  #mean and sd of z scores across iterations
  mean.zscores<-apply(matrix.zscores, 2, mean, na.rm = TRUE)
  sd.zscores<-apply(matrix.zscores, 2, sd, na.rm = TRUE)

  if (is.null(z.adjust) == FALSE) {
    z.adjust.list<-list()
    for (i in 1:length(z.adjust)) {
      list.glht.summary.zadjust<-parallel::mclapply(list.glht, summary, test = multcomp::adjusted(type = z.adjust[i]), mc.cores = mc.cores)
      list.padjust<-parallel::mclapply(list.glht.summary.zadjust, function(x){x[["test"]][["pvalues"]]}, mc.cores = mc.cores)
      list.zadjust<-parallel::mclapply(list.padjust, qnorm, mc.cores = mc.cores)

      #variable in columns, iteration in rows
      matrix.zadjust<-do.call("rbind", list.zadjust)

      #mean and sd of z-adjusted scores across iterations
      mean.zadjust<-apply(matrix.zadjust, 2, mean, na.rm = TRUE)
      sd.zadjust<-apply(matrix.zadjust, 2, sd, na.rm = TRUE)
      z.adjust.list[[paste0(z.adjust[i])]][["z"]]<-mean.zadjust
      z.adjust.list[[paste0(z.adjust[i])]][["sd"]]<-sd.zadjust
    }
  }

  #output stats
  z.stats<-data.frame(test = names(mean.zscores), z = mean.zscores) #z
  if (length(list.glht) > 1) {z.stats$sd<-sd.zscores} #sd
  if (is.null(z.adjust) == FALSE && length(list.glht) == 1) { #adjustsed z
    z.adjust.df<-do.call("cbind", parallel::mclapply(z.adjust.list, `[[`, "z", mc.cores = mc.cores))
    colnames(z.adjust.df)<-paste0(colnames(z.adjust.df), ".z")
    z.stats<-cbind(z.stats, z.adjust.df)
  }
  if (is.null(z.adjust) == FALSE && length(list.glht) > 1) { #adjusted z AND sd
    if (ncol(matrix.zadjust) > 1) {
      z.sd.adjust.df<-do.call("cbind", unlist(z.adjust.list, recursive = FALSE))
      z.stats<-cbind(z.stats, z.sd.adjust.df)
    }
    else {
      z.adjust.df<-do.call("cbind", parallel::mclapply(z.adjust.list, `[[`, "z", mc.cores = mc.cores))
      colnames(z.adjust.df)<-paste0(colnames(z.adjust.df), ".z")
      sd.adjust.df<-do.call("cbind", parallel::mclapply(z.adjust.list, `[[`, "sd", mc.cores = mc.cores))
      colnames(sd.adjust.df)<-paste0(colnames(sd.adjust.df), ".sd")
      z.stats<-cbind(z.stats, z.adjust.df, sd.adjust.df)
      }
  }
  rownames(z.stats)<-NULL
  return(z.stats)
}
