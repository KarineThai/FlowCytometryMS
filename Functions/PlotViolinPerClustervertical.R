#Fonction pour voir la distribution de chaque marqueur par cluster en histogrammes afin de déterminer si on peut faire le MFI
PlotViolinPerClustervertical = function(df, colRegion, markers) {
  #'@param df Dataframe containing expression values per marker
  #'@param colCluster Column in df containing the cluster numbers
  #'@param markers markers to plot
  #'@param group Column containing the group that will be used to color the curves
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(ggplot2, ggpubr,tidyverse,dplyr, RColorBrewer, microViz)
  set.seed(123)
  # color_clusters <- c(RColorBrewer::brewer.pal(12, 'Paired'), RColorBrewer::brewer.pal(8, 'Dark2'))
  # shuffled_colors <- sample(color_clusters)
  brewerPlus <- distinct_palette()
  shuffled_colors <- sample(brewerPlus)
  plots = list()
  for (ma in markers) {
    plots[[ma]] = ggplot(df, aes_string(x=colRegion, y=ma, fill=colRegion)) + 
      geom_violin(drop = F)  + 
      theme_bw()+
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))+ guides(fill = FALSE)+ xlab("")+
      scale_fill_manual(values = brewerPlus)
      
  }
  return(plots)
}
