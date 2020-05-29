#' @export
two.factor.interaction<-function(dataset, fac1, fac2) {

  fac1.fac2.df<-expand.grid(fac1 = levels(dataset[,fac1]),
                            fac2 = levels(dataset[,fac2]))
  mod.matrix<-stats::model.matrix(~ fac2 * fac1, data = fac1.fac2.df)
  Dunnett<-multcomp::contrMat(table(dataset[,fac1]), "Dunnett")
  empty.matrix<-matrix(0, nrow = nrow(Dunnett), ncol = ncol(Dunnett))

  n<-length(levels(dataset[,fac2]))
  contrast.matrix<-list()
  for (i in 1:n) {
    contrast.matrix[[i]]<-cbind(do.call("cbind", replicate(i-1, empty.matrix, simplify = FALSE)),
                                Dunnett,
                                do.call("cbind", replicate(n-i, empty.matrix, simplify = FALSE)))
    rownames(contrast.matrix[[i]])<-paste(levels(dataset[,fac2])[i], rownames(contrast.matrix[[i]]), sep = ":")
  }
  contrast.matrix<-do.call("rbind", contrast.matrix)
  colnames(contrast.matrix)<-rep(colnames(Dunnett), times = n)

  out<-list()
  out[["mod.matrix"]]<-mod.matrix
  out[["contrast.matrix"]]<-contrast.matrix
  return(out)
}
