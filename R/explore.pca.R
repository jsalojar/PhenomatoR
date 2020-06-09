#' @export
#eg. save<-explore.pca(matrix = t(E2), PC = c(1, 2), ranks = 5)
explore.pca<-function(matrix, PC = c(1, 2), ranks = ncol(matrix)) {

  if (ranks > ncol(matrix)) stop("maximum ranks allowed = ", ncol(matrix))

  #call stats::prcomp to do PCA on matrix data
  pca.data<-stats::prcomp(matrix, scale. = TRUE)

  #scree plot
  pca.variance.percentage<-round(pca.data$sdev^2/sum(pca.data$sdev^2)*100, 1)
  pca.variance.percentage.ggplot2<-data.frame(PC = as.character(1:length(pca.variance.percentage)),
                                              percentage_variation = pca.variance.percentage)
  bar.plot<-ggplot2::ggplot(data = pca.variance.percentage.ggplot2,
                            ggplot2::aes(x = PC, y = percentage_variation)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(name = "percentage variation",
                                breaks = seq(0, 100, by = 20),
                                limits = c(0, 100)) +
    ggplot2::labs(title = "scree plot")

  #PCA graph
  pca.data.ggplot2<-data.frame(sample = rownames(pca.data$x),
                               x = pca.data$x[,PC[1]],
                               y = pca.data$x[,PC[2]])
  rownames(pca.data.ggplot2)<-NULL
  pca.plot<-ggplot2::ggplot(data = pca.data.ggplot2, ggplot2::aes(x = x, y = y, label = sample)) +
    ggplot2::geom_point() +
    ggrepel::geom_text_repel(ggplot2::aes(label = sample)) +
    ggplot2::xlab(paste("PC", PC[1], " - ", pca.variance.percentage[PC[1]], "%", sep = "")) +
    ggplot2::ylab(paste("PC", PC[2], " - ", pca.variance.percentage[PC[2]], "%", sep = "")) +
    ggplot2::labs(title = "PCA graph")

  #loading scores of top `ranks` number of genes in a data.frame
  loading.scores<-lapply(PC, function(x){
    loading.scores.pc<-pca.data$rotation[,x][1:ranks]
    genes.ordered.by.magnitude<-names(sort(abs(loading.scores.pc), decreasing = TRUE))
    loading.scores.ordered<-loading.scores.pc[match(genes.ordered.by.magnitude, names(loading.scores.pc))]
    data.frame(PC = rep(x, times = ranks),
               rank = 1:ranks,
               gene = genes.ordered.by.magnitude,
               loading.scores = loading.scores.ordered)
  })
  loading.scores<-do.call("rbind", loading.scores)
  rownames(loading.scores)<-NULL

  #compile outputs
  out<-list()
  out[["data"]]<-pca.data
  out[["scree.plot"]]<-bar.plot
  out[["PCA"]]<-pca.plot
  out[["loading.scores"]]<-loading.scores
  return(out)
}

#' @export
#eg. explore.pca2(E4)
explore.pca2<-function(df = E4, value = "value", genes = "gene", samples = "treatment.time", category = "treatment",
                       PC = c(1, 2), ranks = length(unique(df[,genes]))) {

  if (ranks > length(unique(df[,genes]))) stop("maximum ranks allowed = ", length(unique(df[,genes])))

  #ensure data in columns contain appropriate data class
  if (is.null(category) == FALSE) {df[,category]<-as.factor(df[,category])}
  df[,value]<-as.numeric(as.character(df[,value]))

  #pca
  matrix<-reshape2::dcast(df, formula(paste(samples, "~", genes)), value.var = paste(value))
  rownames(matrix)<-matrix[,samples]
  matrix<-as.matrix(matrix[,names(matrix)!=samples])
  pca.data<-stats::prcomp(matrix, scale. = TRUE)

  #scree plot
  pca.variance.percentage<-round(pca.data$sdev^2/sum(pca.data$sdev^2)*100, 1)
  pca.variance.percentage.ggplot2<-data.frame(PC = as.character(1:length(pca.variance.percentage)),
                                              percentage_variation = pca.variance.percentage)
  bar.plot<-ggplot2::ggplot(data = pca.variance.percentage.ggplot2,
                            ggplot2::aes(x = PC, y = percentage_variation)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(name = "percentage variation",
                                breaks = seq(0, 100, by = 20),
                                limits = c(0, 100)) +
    ggplot2::labs(title = "scree plot")

  #PCA graph
  pca.data.ggplot2<-data.frame(sample = rownames(pca.data$x),
                               x = pca.data$x[,PC[1]],
                               y = pca.data$x[,PC[2]])
  rownames(pca.data.ggplot2)<-NULL
  if (is.null(category) == FALSE) {
    reference<-unique(df[c(samples, category)])
    pca.data.ggplot2$factor<-reference[,category][match(pca.data.ggplot2$sample, reference[,samples])]

    pca.plot<-ggplot2::ggplot(data = pca.data.ggplot2, ggplot2::aes(x = x, y = y, label = sample, color = factor)) +
      ggplot2::geom_point() +
      ggrepel::geom_label_repel(ggplot2::aes(label = sample, fill = factor), color = "black", show.legend = FALSE) +
      ggplot2::labs(title = "PCA graph", color = category) +
      ggplot2::theme(legend.position = "right")
  }
  else {
    pca.plot<-ggplot2::ggplot(data = pca.data.ggplot2, ggplot2::aes(x = x, y = y, label = sample)) +
      ggplot2::geom_point() +
      ggrepel::geom_text_repel(ggplot2::aes(label = sample)) +
      ggplot2::labs(title = "PCA graph")
  }

  pca.plot<-pca.plot +
    ggplot2::xlab(paste("PC", PC[1], " - ", pca.variance.percentage[PC[1]], "%", sep = "")) +
    ggplot2::ylab(paste("PC", PC[2], " - ", pca.variance.percentage[PC[2]], "%", sep = ""))

  #loading scores of top `ranks` number of genes in a data.frame
  loading.scores<-lapply(PC, function(x){
    loading.scores.pc<-pca.data$rotation[,x][1:ranks]
    genes.ordered.by.magnitude<-names(sort(abs(loading.scores.pc), decreasing = TRUE))
    loading.scores.ordered<-loading.scores.pc[match(genes.ordered.by.magnitude, names(loading.scores.pc))]
    data.frame(PC = rep(x, times = ranks),
               rank = 1:ranks,
               gene = genes.ordered.by.magnitude,
               loading.scores = loading.scores.ordered)
  })
  loading.scores<-do.call("rbind", loading.scores)
  rownames(loading.scores)<-NULL

  #compile outputs
  out<-list()
  out[["data"]]<-pca.data
  out[["scree.plot"]]<-bar.plot
  out[["PCA"]]<-pca.plot
  out[["loading.scores"]]<-loading.scores
  return(out)
}
