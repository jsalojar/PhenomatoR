#' @export
long.to.wide.df<-function(z.stats, value = "z", row = "id", col = "Genotypes", mc.cores = 1L) {

  #split data.frame based on id
  z.stats[,row]<-as.factor(z.stats[,row])
  z.stats.list<-split(z.stats, z.stats[,row])

  #create list of data.frames with col values as column names
  z.stats.list<-lapply(z.stats.list, function(x){as.data.frame(matrix(x[,value], nrow = 1, dimnames = list(x[,row][1], x[,col])))})

  #compile into a single data.frame
  wide.df<-plyr::rbind.fill(z.stats.list)
  wide.df$phenotype<-levels(z.stats[,row])

  return(wide.df)
}
