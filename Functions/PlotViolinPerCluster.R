#Fonction pour voir la distribution de chaque marqueur par cluster en histogrammes afin de déterminer si on peut faire le MFI
PlotViolinPerCluster = function(df, colCluster, markers) {
  #'@param df Dataframe containing expression values per marker
  #'@param colCluster Column in df containing the cluster numbers
  #'@param markers markers to plot
  #'@param group Column containing the group that will be used to color the curves
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(ggplot2, ggpubr,tidyverse,dplyr, RColorBrewer)
  set.seed(123)
  # color_clusters <- c(RColorBrewer::brewer.pal(12, 'Paired'), RColorBrewer::brewer.pal(8, 'Dark2'))
  # shuffled_colors <- sample(color_clusters)
  brewerPlus <- distinct_palette()
  shuffled_colors <- sample(brewerPlus)
  plots = list()
  for (ma in markers) {
    plots[[ma]] = ggplot(df, aes_string(x=ma, y=colCluster, fill=colCluster)) + 
      geom_violin()  + 
      theme(axis.title.y = element_blank(), legend.position = "none")+ guides(fill = FALSE)+
      ylab("")+
      scale_fill_manual(values = brewerPlus)+
      theme_bw()
  }
  return(plots)
}
